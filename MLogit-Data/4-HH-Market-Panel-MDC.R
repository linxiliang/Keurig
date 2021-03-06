#---------------------------------------------------------------------------------------------------#
# Load relevant data
# Load Appropriate Auxiliary data sets generated in Price Imputation Algorithm
load(paste(output_dir, "/Assist_Data_Sets_Retailer_Prices.RData", sep=""))

# Load Imputed Price and Availability Data - this is based on both RMS and Homescan
load(paste(output_dir, "/Retailer_Price_Panel.RData", sep=""))
#load(paste(output_dir, "/Retailer_Price_Panel_Region.RData", sep=""))

# Load household and demographic data
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))

# Load household panel 
load(paste(output_dir, "/hh_trip_panel.RData", sep=""))

# Settings
year_threshold = 2006
cond_purch = TRUE

# For the Kim Allenby and Rossi version of the model:
# (1) We don't need size --- rather an average price
# (2) Compute the expenditure on each product (defined as brand type/flavor combination)

# Make retail price panel product unique -- not price and size unique
# (i) Create weights base on overall sales in Homescan
# (ii) Obtain average price weighted by these sales
# Filter Outlier Prices
retailer_panel = retailer_panel[(price>0.20&!is.na(price)) | brand_descr=="0NOTHING", ] 
retailer_panel[brand_descr=="0NOTHING", ptype:="NOTHING"]
prod_weights = purchases[, .(hms_revenue = sum(total_price_paid)), 
                         by = c("brand_descr_orig", "ptype", "keurig", "roast", "size1_amount",
                                "flavored", "kona", "colombian", "sumatra", "wb")]
setkeyv(prod_weights, c("brand_descr_orig", "ptype", "keurig", "roast", "size1_amount",
                        "flavored", "kona", "colombian", "sumatra", "wb"))
setkeyv(retailer_panel, c("brand_descr_orig", "ptype", "keurig", "roast", "size1_amount",
                          "flavored", "kona", "colombian", "sumatra", "wb"))
retailer_panel = prod_weights[retailer_panel]
retailer_panel[brand_descr!="0NOTHING", price:=price/size1_amount] # Normalize all prices to per serving
retailer_panel = retailer_panel[(price>0.02&price<2.00&!is.na(price)) | brand_descr=="0NOTHING", ] 
retailer_panel = retailer_panel[, .(price = ifelse(brand_descr=="0NOTHING", 0, 
                                                   sum(price*hms_revenue)/sum(hms_revenue)), 
                                    prod_id = .GRP),
                                by = c("dma_code", "retailer_code", "week_end", "brand_descr",
                                       "brand_descr_orig", "ptype", "keurig", "roast", 
                                       "flavored", "kona","colombian", "sumatra", "wb", "in_rms")]
retailer_panel[, `:=`(nprod = length(unique(prod_id)), nbrand=length(unique(brand_descr_orig))), 
               by = c("dma_code", "retailer_code", "week_end", "ptype")]
retailer_panel[, `:=`(nprod = length(unique(prod_id))), 
               by = c("dma_code", "retailer_code", "week_end", "ptype", "brand_descr_orig")]
retailer_panel[, `:=`(prod_id = NULL)]
retailer_panel = retailer_panel[(price>0.02&price<2.00&!is.na(price)) | brand_descr=="0NOTHING", ] 
save(retailer_panel, file = paste(output_dir, "/Retailer_Brand_Price_Panel.RData", sep=""))

# Now contrain panel to trips with valid lags and year threshold 
hh_panel = hh_panel[panel_year>=year_threshold & !is.na(brand_type_lag), ]

# Get rid of variables not needed for now.
hh_panel = hh_panel[, .(dma_code, retailer_code, household_code, kholding, week_end, panel_year, 
                        norder, trip_code_uc, coffee_trip, total_spent, total_spent_week, caf, grocery,
                        brand_type_lag, brand_cum, ptype_lag, brand_type_lag2, ptype_lag2, 
                        inv_all, inv_type, inv_brand_type, crate)]
hh_panel = hh_panel[dma_code%in%top_dma_list & !is.na(trip_code_uc), ] # Constrain both to be in the top DMAs

# Constrain to only coffee purchasing trips
if (cond_purch){
  hh_panel = hh_panel[coffee_trip==1, ]
} else{
  # Sample 25% of non-purchase trips otherwise data become too big
  # s_trips = hh_panel[coffee_trip==0, .(trip_code_uc=sample(trip_code_uc, ceiling(0.25 * .N))), 
  #                   by = c("household_code")][, trip_code_uc]
  # Use caf drink purchase as outside option
  s_trips = hh_panel[caf>0, unique(trip_code_uc)] 
  hh_panel = hh_panel[trip_code_uc%in%s_trips | coffee_trip==1, ]
}

# Now use the trip data to create purchase panel for mixed logit demand estimation
retailer_panel = retailer_panel[dma_code%in%top_dma_list, ]
setkeyv(hh_panel, c("dma_code", "retailer_code", "week_end"))
setkeyv(retailer_panel, c("dma_code", "retailer_code", "week_end"))
hh_market_prod = hh_panel[retailer_panel, allow.cartesian=TRUE, nomatch=0L]
gc()

# Get rid of Keurig products when the consumer doesn't hold keurig machine
hh_market_prod = hh_market_prod[kholding==1 | (kholding == 0 & keurig == 0), ]

# Focal Purch collapse the size
focal_purch=focal_purch[, .(total_price_paid = sum(total_price_paid), 
                            coupon_value = sum(coupon_value),
                            size = sum(quantity*size1_amount)), 
                        by = c("trip_code_uc", "brand_descr", "brand_descr_orig", "ptype","keurig",
                               "roast", "flavored", "kona", "colombian", "sumatra", "wb")]

# Merge in the purchase data to locate the purchased product
setkey(focal_purch, trip_code_uc, brand_descr, brand_descr_orig, keurig, ptype, 
       roast, flavored, kona, colombian, sumatra, wb)
setkey(hh_market_prod, trip_code_uc, brand_descr, brand_descr_orig, keurig, ptype, 
       roast, flavored, kona, colombian, sumatra, wb)
hh_market_prod = focal_purch[, .(trip_code_uc, brand_descr, brand_descr_orig, keurig, ptype,
                                 roast, flavored, kona, colombian, sumatra, wb, size,
                                 total_price_paid, coupon_value)][hh_market_prod]
hh_market_prod[is.na(total_price_paid), `:=`(total_price_paid=0, size=0, coupon_value=0)]

# Remove purchases of VUE, TASSIMO, SENSEO or GUSTO -- They are also single cup serving machine
hh_market_prod = hh_market_prod[ptype%in%c("KEURIG","OTHER","NOTHING"), ]

# Still, for some shopping occasions, the consumer could be choosing from over 90 alternatives, 
# which is not realistic
# So, I drop all brands below 90th percentile in ground coffee
brand_size_type_sales = purchases[, .(revenue = sum(total_price_paid-coupon_value)), 
                                  by = c("dma_code", "ptype", "keurig", "brand_descr_orig", "roast", 
                                         "flavored", "kona", "colombian", "sumatra", "wb")]
brand_size_type_sales[, rshare:=revenue/sum(revenue), by=c("dma_code", "ptype", "keurig")]
brand_size_type_sales = brand_size_type_sales[order(-rshare), ]
setkeyv(brand_size_type_sales, c("dma_code", "ptype", "keurig"))
brand_size_type_sales[, rcumshare:=cumsum(rshare), by=c("dma_code", "ptype", "keurig")]
brand_size_type_sales[, rcumshare:=rcumshare-rshare]

# Merge the criteria into the hh_market_prod
setkeyv(brand_size_type_sales, c("dma_code", "ptype", "keurig", "brand_descr_orig", "roast", 
                                 "flavored", "kona", "colombian", "sumatra", "wb"))
setkeyv(hh_market_prod, c("dma_code", "ptype", "keurig", "brand_descr_orig", "roast", 
                          "flavored", "kona", "colombian", "sumatra", "wb"))
hh_market_prod = brand_size_type_sales[hh_market_prod]
hh_market_prod = hh_market_prod[rcumshare<=0.90|keurig==1|ptype=="NOTHING", ]
hh_market_prod[,`:=`(rshare=NULL)]
gc()

# Get rid of extreme prices -- below 0.05% percentile and 99.95% percentile 
plimit = hh_market_prod[ptype!="NOTHING", quantile(price, c(0.0005, 0.9995))]
hh_market_prod = hh_market_prod[(price>=plimit[1] & price<=plimit[2])|ptype=="NOTHING", ]

# Only keep the occassions with purchases conditonal on purchasing coffee [not any more]
hh_market_prod[, npurch:=sum(total_price_paid>0.009), by = "trip_code_uc"]
hh_market_prod = hh_market_prod[as.integer(npurch)>=1|coffee_trip==0, ]

# Drop trips with very high expenditure on coffee over 150 (extremely rare!!!)
hh_market_prod[, total_coffee_expend:=sum(total_price_paid, na.rm=TRUE), by = "trip_code_uc"]
hh_market_prod = hh_market_prod[total_coffee_expend<=150, ]

# Filter Households with less than 2 Purchases
hh_market_prod[, npurch:=length(unique(trip_code_uc)), by = "household_code"]
hh_market_prod = hh_market_prod[as.integer(npurch)>=2, ]

# Modify brand_descr to take into account private label issues -- two private may not be the same
hh_market_prod[brand_descr_orig=="CTL BR", brand_type := paste(brand_descr_orig, retailer_code, sep="_")]
hh_market_prod[brand_descr_orig!="CTL BR", brand_type := brand_descr_orig]
hh_market_prod[, `:=`(brand_type = paste(brand_type, ptype, sep = "_"), nobs = 1:.N)]
hh_market_prod[, `:=`(is_lag = as.integer(grepl(brand_type, brand_type_lag)),
                      is_lag2 = as.integer(grepl(brand_type, brand_type_lag2))), by = "nobs"]

# Input the number of periods of consumption, and impute the new inventory...
# Method 1 - Inventory as a way to gauge purchases only! 
hh_market_prod[, `:=`(keurig_lag = as.integer(grepl("KEURIG", ptype_lag)))]
hh_market_prod[, kinventory := keurig_lag * inv_type]

# Drop variables not needed.
hh_market_prod[,`:=`(retailer_code=NULL, brand_type_lag=NULL, brand_type_lag2=NULL,
                     ptype_lag=NULL, ptype_lag2=NULL, norder=NULL, crate =NULL, 
                     nobs=NULL, rcumshare=NULL, revenue=NULL, 
                     in_rms=NULL, npurch=NULL, inventory = inv_type)]
gc()

# Modify the brand to create idiosyncratic match values for top Keurig Brands
hh_market_prod[, brand_descr_temp:=brand_descr]
hh_market_prod[as.integer(keurig)==1 & brand_descr_temp %in% top_keurig_brands, 
               brand_descr := paste(brand_descr, "KEURIG")]

# Flag brands not often purchased, and put them in the other category.
bpurch = hh_market_prod[total_price_paid>=0.009, .(nb=.N), by = "brand_descr"]
top_brands = bpurch[(nb>=3000 & grepl("KEURIG", brand_descr)) | (nb>=15000), brand_descr]

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
hh_household = unique(hh_market_prod[, .(household_code, hh)])
setkey(hh_household, household_code)
hh_household[, one:=1]
# Obtain the demographic data at household level using the most recent information available.
hh_info_panel[, year_filter := as.integer(panel_year==max(panel_year)), by = "household_code"]
hh_info_panel = hh_info_panel[as.integer(year_filter)==1, ]
setkey(hh_info_panel, household_code)
hh_demo = hh_household[hh_info_panel[,.(household_code, dma_code, overall_rate, inc25, inc40, inc50, inc60, 
                                        inc70, hhsize1, hhsize2, hhsize3, hhsize5, onefamily, twofamily,
                                        threefamily, fulltime, partime, notemployed, head_age_numeric,
                                        presence_of_children, african_american, hispanic, cable, internet,
                                        total_spent, coff, caf, drink, grocery, rest, ntrips, nretailers,
                                        total_spent_hhi, drink_hhi, grocery_hhi, rest_hhi, ntrips_hhi)], 
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
hh_market_prod[, `:=`(brand_lag = is_lag, brand_lag2 = is_lag2, purchased = as.integer(total_price_paid>=0.009))]

# Make brand_lag different if the last purchase is K-Cup versus Ground Coffee
# Make the flavor and roast dummies
hh_market_prod[, `:=`(brand_lag_keurig = brand_lag * keurig,
                      brand_lag2_keurig = brand_lag2 * keurig,
                      lightR = as.integer(roast==1),
                      mediumR = as.integer(roast==2),
                      medDR = as.integer(roast==3),
                      darkR = as.integer(roast==4),
                      assorted = as.integer(roast==0))]

# Scale price by 10, and inventory by 100
hh_market_prod[, `:=`(inventory = inventory/10, kinventory = kinventory/10,
                      brand_cum_log = log(brand_cum+1), brand_cum = brand_cum/100)]
hh_market_prod[, `:=`(invsq = inventory^2, brand_cum_sq = brand_cum^2, 
                      outside = as.integer(brand_descr=="0NOTHING"))]
name_order = c("household_code", "trip_code_uc", "dma_code", "hh", "t", "brand_descr", "brand", bvars, 
               "outside", "keurig", "flavored", "roast", "lightR", "mediumR", "medDR", "darkR", "assorted", 
               "kona", "colombian", "sumatra", "wb", "price", "brand_lag", "brand_lag_keurig", "brand_lag2_keurig",
               "nbrand", "nprod", "total_spent", "total_spent_week", "caf", "grocery", "purchased", "size", 
               "inventory", "kinventory", "total_price_paid",  "coupon_value", "brand_descr_orig", "kholding")
hh_market_prod = hh_market_prod[, name_order, with = FALSE]
hh_market_prod[brand_descr=="0NOTHING", `:=`(size=caf, total_price_paid=caf, price=1, purchased=as.integer(caf>0))]
hh_market_prod[grepl(" KEURIG", brand_descr), keurig:=0]
setkey(hh_market_prod, hh, t, brand_descr, keurig)

if (cond_purch){
  save(hh_market_prod, hh_demo, file = paste(output_dir,"/MDC-Cond-Purchase-Flavors.RData", sep=""))
} else{
  save(hh_market_prod, hh_demo, file = paste(output_dir,"/MDC-Purchase-Flavors.RData", sep=""))
}

