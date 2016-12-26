#---------------------------------------------------------------------------------------------------#
# Now only take top market data for our estimation purpose. This is because we have too much data...
# It's computationally infeasible to estimate such a model.
load(paste(output_dir, "/hh_product_panel.RData", sep=""))

# Load household and demographic data
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))
hh_info_panel = hh_info_panel[dma_code%in%top_dma_list, ]

# Restrict to households in the top DMAs only.
dma_hh_list = unique(hh_prod_panel[as.integer(dma_code) %in% top_dma_list, household_code])
hh_market_prod = hh_prod_panel[household_code %in% dma_hh_list, ]
rm(hh_prod_panel)
gc()

# Only keep the occassions with purchases - Conditional on purchasing coffee
hh_market_prod = hh_market_prod[brand_descr != "0NOTHING", ]
hh_market_prod[, npurch:=sum(as.integer(quantity>=1)), by = "trip_code_uc"]
hh_market_prod = hh_market_prod[as.integer(npurch)>=1, ]

# Drop trips with multiple purchases - not discrete choice
#hh_market_prod[, npurch:=sum(as.integer(quantity>=1)), by = "trip_code_uc"]
#hh_market_prod = hh_market_prod[as.integer(npurch)==1, ]

# Drop trips with very high expenditure on coffee over 100.
hh_market_prod[, total_price_paid:=sum(total_price_paid, na.rm=TRUE), by = "trip_code_uc"]
hh_market_prod = hh_market_prod[total_price_paid<=100, ]

# Filter Households with less than 3 Purchases
hh_market_prod[, npurch:=sum(as.integer(quantity>=1)), by = "household_code"]
hh_market_prod = hh_market_prod[as.integer(npurch)>=3, ]
hh_market_prod[, `:=`(total_spent = mean(total_spent, na.rm=TRUE)), by = "trip_code_uc"]

# Modify brand_descr to take into account private label issues -- two private may not be the same
hh_market_prod[brand_descr=="CTL BR", brand_descr_modified := paste(brand_descr, retailer_code, sep="_")]
hh_market_prod[is.na(brand_descr_modified), brand_descr_modified := brand_descr]
hh_market_prod[brand_lag==brand_descr_modified & ptype_lag==ptype, `:=`(is_lag = 1)]
hh_market_prod[brand_lag!=brand_descr_modified | ptype_lag!=ptype, `:=`(is_lag = 0, brand_cum = 0)]

# Hassle cost of purchase to rationalize nonpurchase as well
hh_market_prod[, `:=`(hassle = as.integer(brand_descr!="0NOTHING"))]

# Input the number of periods of consumption, and impute the new inventory...
# Method 1 - Inventory as a way to gauge purchases only! 
hh_market_prod[, keurig_lag := as.integer(ptype_lag=="KEURIG")]
hh_market_prod[, `:=`(sinventory = size1_amount)]
hh_market_prod[brand_descr=="0NOTHING", `:=`(sinventory = inv_type)]
hh_market_prod[brand_descr!="0NOTHING", `:=`(inv_type=0)]
hh_market_prod[, kinventory := keurig_lag * inv_type]
hh_market_prod[, `:=`(brand_descr_modified=NULL)]

# Obtain size and inventory.
hh_market_prod[grepl("CTL BR", brand_lag), brand_lag:="CTL BR"]
hh_market_prod[, `:=`(brand_descr_c = ifelse(brand_descr=="0NOTHING" & sinventory>=1, 
                                             brand_lag, brand_descr),
                      keurig_c = ifelse(brand_descr=="0NOTHING" & sinventory>=1, 
                                        as.integer(ptype_lag=="KEURIG"), keurig))]
hh_market_prod[as.integer(keurig_c)==1 & brand_descr=="0NOTHING", ptype := "KEURIG"]
hh_market_prod[as.integer(keurig_c)==0 & ptype_lag=="SENSEO" & brand_descr=="0NOTHING", ptype := "SENSEO"]
hh_market_prod[as.integer(keurig_c)==0 & ptype_lag=="GUSTO" & brand_descr=="0NOTHING", ptype := "GUSTO"]
hh_market_prod[as.integer(keurig_c)==0 & ptype_lag=="TASSIMO" & brand_descr=="0NOTHING", ptype := "TASSIMO"]
hh_market_prod[as.integer(keurig_c)==0 & ptype_lag=="VUE" & brand_descr=="0NOTHING", ptype := "VUE"]

# Merge in consumption rate by household and product type.
hh_rate[, ptype:=ptype_lag]
setkey(hh_market_prod, household_code, ptype)
setkey(hh_rate, household_code, ptype)
hh_market_prod = hh_rate[,.(household_code, ptype, crate)][hh_market_prod]
# Input the before/after rate in case of missing.
hh_market_prod = hh_list[, .(household_code, brate, arate, k_first_week_end)][hh_market_prod]
hh_market_prod[is.na(crate) & ptype=="KEURIG", crate := arate]
hh_market_prod[is.na(crate) & ptype!="KEURIG", crate := brate]
hh_market_prod[is.na(crate), crate := arate] # Never observe purchasing ground!
hh_market_prod[is.na(crate), crate := brate] # Purchased Keurig only once/twice!
hh_market_prod[, `:=`(arate=NULL, brate=NULL, k_first_week_end=NULL)]

# Given the inventory and product size - compute the relevant cofficients
dbeta = 0.995
hh_market_prod[, Tj:=sinventory/crate]
hh_market_prod[, `:=`(coef1 = (1 - dbeta^Tj)/(1 - dbeta), 
                      coef2 = sinventory * (1 - dbeta^Tj)/(1 - dbeta) + 
                        crate*((Tj * dbeta^Tj)/(1-dbeta) - (1 - dbeta^Tj)/(1 - dbeta)^2))]

# Drop variables not needed.
hh_market_prod[,`:=`(retailer_code=NULL, coffee_trip=NULL, brand_lag=NULL, brand_descr_c=NULL,
                     keurig_c=NULL, ptype_lag=NULL, inventory = inv_type)]
hh_market_prod[brand_descr=="0NOTHING", `:=`(brand_descr="0NOTHING", keurig=0)] # Trip - quantity choice


# Modify the brand to create idiosyncratic match values for top Keurig Brands
hh_market_prod[, brand_descr_temp:=brand_descr]
hh_market_prod[as.integer(keurig)==1 & brand_descr_temp %in% top_keurig_brands, 
               brand_descr := paste(brand_descr, "KEURIG")]

# Flag brands not often purchased, and put them in the other category.
bpurch = hh_market_prod[quantity>=1, .(nb=.N), by = "brand_descr"]
top_brands = bpurch[(nb>=4000 & grepl("KEURIG", brand_descr)) | (nb>=15000) , brand_descr]

# Given the numerous brands, it's not feasible to estimate the brand intercepts for all brands.
# I constraint small brands to have the same brand intercept.
hh_market_prod[(!(brand_descr %in% top_brands)) & brand_descr!="0NOTHING", brand_descr:="OTHER"]
hh_market_prod[brand_descr=="OTHER", brand_descr:="0OTHER"]

# Create new household id, and try to get the brand code etc...
setkey(hh_market_prod, household_code)
hh_market_prod[, hh := .GRP, by = "household_code"]
setkey(hh_market_prod, brand_descr)
hh_market_prod[, brand := .GRP, by = "brand_descr"]

# Obtain the list of corresponding household demographics
hh_household_code = unique(hh_market_prod[, .(household_code, hh)])
setkey(hh_household_code, household_code)
hh_household_code[, one:=1]
# Obtain the demographic data at household level using the most recent information available.
hh_info_panel[, year_filter := as.integer(panel_year==max(panel_year)), by = "household_code"]
hh_info_panel = hh_info_panel[as.integer(year_filter)==1, ]
setkey(hh_info_panel, household_code)
hh_demo = hh_household_code[hh_info_panel[,.(household_code, dma_code, overall_rate, inc25, inc40, inc50, inc60, 
                                             inc70, hhsize1, hhsize2, hhsize3, hhsize5, onefamily, twofamily,
                                             threefamily, fulltime, partime, notemployed, head_age_numeric,
                                             presence_of_children, african_american, hispanic, cable, internet)], 
                            nomatch=0L]
#hh_demo[, household_code:=NULL]
setkey(hh_demo, hh)

# Create the brand dummies - and prepare data
br_list = sort(hh_market_prod[, unique(brand)])
bvars = paste0("a", br_list)
dbt = data.table(model.matrix(~factor(hh_market_prod$brand)-1))
setnames(dbt, names(dbt), bvars)
hh_market_prod = cbind(hh_market_prod, dbt)
hh_market_prod[, t:=.GRP, by = "trip_code_uc"]
hh_market_prod[, `:=`(brand_lag = is_lag, purchased = as.integer(quantity>=1))]

# Make brand_lag different if the last purchase is K-Cup versus Ground Coffee
# Make the flavor and roast dummies
hh_market_prod[, `:=`(brand_lag_keurig = brand_lag * keurig,
                      lightR = as.integer(roast==1),
                      mediumR = as.integer(roast==2),
                      medDR = as.integer(roast==3),
                      darkR = as.integer(roast==4),
                      assorted = as.integer(roast==0))]

# Scale price by 10, and inventory by 100
hh_market_prod[, `:=`(inventory = inventory/10, kinventory = kinventory/10, sinventory = sinventory/10,
                      coef1 = coef1/10, coef2 = pmax(coef2/100, 0), brand_cum_log = log(brand_cum+1), 
                      Tj = Tj/30, brand_cum = brand_cum/100)]
hh_market_prod[, `:=`(invsq = inventory^2, sinvsq = sinventory^2, Tjsq = Tj^2, brand_cum_sq = brand_cum^2)]
name_order = c("household_code", "trip_code_uc", "dma_code", "hh", "t", "brand_descr", "brand", bvars, 
               "keurig",  "flavored", "roast", "lightR", "mediumR", "medDR", "darkR", "assorted", "kona",
               "colombian", "sumatra", "wb", "price", "size1_amount", "brand_lag", "brand_lag_keurig",
               "brand_cum", "brand_cum_sq", "brand_cum_log", "total_spent", "purchased", "hassle")
hh_market_prod = hh_market_prod[, name_order, with = FALSE]

# Temp fix
hh_market_prod[is.na(flavored), flavored:=0]

save(hh_market_prod, hh_demo, file = paste(output_dir,"/Coffee-Panel-Cond-Purchase-Flavors.RData", sep=""))
