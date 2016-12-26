#---------------------------------------------------------------------------------------------------#
# Load appropriate data
# Load HH file with flags
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))

# Only keep households in the top N DMAs in terms of adopters
dma_rank_list = unique(hh_info_panel[kholder==1, .(household_code, dma_code)]) # Households double count if moved. 
dma_rank_list = dma_rank_list[, .(nhh = .N), by = "dma_code"]
dma_rank_list = dma_rank_list[order(-nhh), ]
dma_rank_list[, cumshare := cumsum(nhh/sum(nhh))]
top_dma_list = dma_rank_list[1:30, dma_code]

# In terms of total coffee consuming households
setkey(hh_info_panel, NULL)
dma_rank_list = unique(hh_info_panel[, .(household_code, dma_code)]) # Households double count if moved. 
dma_rank_list = dma_rank_list[, .(nhh = .N), by = "dma_code"]
dma_rank_list = dma_rank_list[order(-nhh), ]
dma_rank_list[, share := (nhh/sum(nhh))]
dma_rank_list[dma_code%in%top_dma_list, sum(share)] # About 50% of total coffee consuming households.

# Now, keep only the households in those market. 
hh_info_panel = hh_info_panel[dma_code%in%top_dma_list, ]
hh_code_list = unique(hh_info_panel[, .(household_code)])
setkey(hh_code_list, household_code)
setkey(hh_list, household_code)
hh_list = hh_list[hh_code_list, nomatch=0L]

# Load HMS Trips Data
load(paste(HMS_trip_dir, "/Trips.RData", sep=""))

# Load HMS Purchase data
load(paste(HMS_input_dir, "/HMS-Purchases-Coffee.RData", sep=""))

# Load Product information
load(paste(meta_dir, "/Products.RData", sep=""))

# Load Store information
load(paste(meta_dir, "/Stores.RData", sep=""))

# Make purchases trip, product unique
pnamelist = names(purchases)
pnamelist = setdiff(pnamelist, c("total_spent", "total_price_paid", "coupon_value", "deal_flag_uc","quantity"))
purchases[upc==9955504002 & quantity == 24, quantity:=2]
purchases[upc==9955504002 & quantity == 12, quantity:=1]

purchases = purchases[, .(total_price_paid = sum(total_price_paid),
                          coupon_value = sum(coupon_value),
                          quantity = sum(quantity)), by = pnamelist]

# Merge in the type information to the purchase data
setkeyv(purchases, c("upc", "upc_ver_uc"))
purchases=purchases[products[, .(upc, upc_ver_uc, ptype, brand_descr, size1_amount, multi)], nomatch=0L]
purchases[, month:=as.factor(format(purchase_date, '%Y-%m'))]

# Restrict purchases to specific software purchases
purchases = purchases[product_module_code %in% focal_module, ]

# Only Purchases Beyond 2008 matters 
purchases = purchases[panel_year>=2008, ]

# Only purchases from the chosen households matter
setkey(purchases, household_code)
purchases = purchases[hh_list[,.(household_code)], nomatch=0L]

# Obtain the list of household retailers (retailers where the household shop for coffee).
setkey(purchases, NULL)
hh_retailer_list = unique(purchases[, .(household_code, retailer_code)])

# Merge back into trips data to get only the relevant trips.
setkey(hh_retailer_list, household_code, retailer_code)
setkey(trips, household_code, retailer_code)
trips = trips[hh_retailer_list, nomatch=0L]

# Only Trips Beyond 2008 matters 
trips = trips[panel_year>=2008, ]
setkeyv(trips, c("household_code", "purchase_date", "trip_code_uc"))

# Flag Trips with coffee purchases.
coffee_trips = unique(purchases[, .(trip_code_uc)])
coffee_trips[, coffee_trip := 1]
setkey(trips, trip_code_uc)
setkey(coffee_trips, trip_code_uc)
trips = coffee_trips[trips]
trips[is.na(coffee_trip), coffee_trip:=0]
rm(coffee_trips)
setkeyv(trips, c("household_code", "purchase_date", "trip_code_uc"))

#---------------------------------------------------------------------------------------------------#
# First obtain the share of sales by brands and type - basis of brand to be included in estimation.
purchases[, keurig:=as.integer(ptype=="KEURIG")]
brand_type_sales = purchases[, .(brand_sales = sum(total_price_paid-coupon_value)), 
                             by = c("brand_descr", "keurig")]
brand_type_sales[, total_sales := sum(brand_sales), by = "keurig"]
brand_type_sales[, brand_share := brand_sales/total_sales]
brand_type_sales = brand_type_sales[order(-brand_share), ]
brand_type_sales[, cumshare := cumsum(brand_share), by = "keurig"]
brand_type_sales[, presence := .N, by = "brand_descr"]

# Obtain list of brands of interest
select_brand_list = brand_type_sales[keurig==1,][1:20, brand_descr]
select_brand_list = c(select_brand_list, "DUNKIN' DONUTS", "CHOCK FULL O NUTS")

#---------------------------------------------------------------------------------------------------#
# Obtain the top retailers for Keurig and other ground coffees -- determine which retailers to keep, 
# and which are purely for state transition purposes.
retailer_type_sales = purchases[, .(retailer_sales = sum(total_price_paid-coupon_value),
                                    npurch = .N), 
                                by = c("retailer_code", "keurig")]
retailer_type_sales[, total_sales := sum(retailer_sales), by = "keurig"]
retailer_type_sales[, retailer_share := retailer_sales/total_sales]
retailer_type_sales = retailer_type_sales[order(-retailer_share), ]
retailer_type_sales[, cumshare := cumsum(retailer_share), by = "keurig"]
retailer_type_sales[, presence := .N, by = "retailer_code"]
retailer_type_sales[, retailer_rank := 1:length(retailer_code), by = "keurig"]

# Obtain the list of retailers in RMS. 
rms_retailer_list = unique(stores[!is.na(retailer_code), .(retailer_code)])
rms_retailer_list[, in_rms:=1]

# Merge in RMS information into purchases
setkey(rms_retailer_list, retailer_code)
setkey(retailer_type_sales, retailer_code)
retailer_type_sales = rms_retailer_list[retailer_type_sales]
retailer_type_sales[is.na(in_rms), in_rms:=0]
setkey(retailer_type_sales, keurig, retailer_rank)

# Obtain list of retailers -- either high in volume (top 30 in sales) or in RMS. 
# With this criteria, we obtain about 90% of the volume in either ground coffee or Keurig K-Cups.
retailer_type_sales[, selected := as.integer((keurig==1&retailer_rank<=30)|in_rms==TRUE)]
retailer_type_sales[, selected := as.integer(sum(selected)>=1), by = "retailer_code"]
selected_retailers = unique(retailer_type_sales[selected==1, .(retailer_code)])
#---------------------------------------------------------------------------------------------------#
# Now build the retailer - week panel of prices and product availability.


#---------------------------------------------------------------------------------------------------#
# Focus on the platform of choice
hh_list = hh_list[ptype==platform, ]
hh_list[, ptype:=NULL]
purchases = purchases[ptype=="KEURIG"|ptype=="OTHER", ]

# Focus on households make purchases above the threshold
hh_list = hh_list[sn>= npurch, ]

# Select only purchases from the selected households
setkeyv(purchases, c("household_code"))
setkey(hh_list, household_code)

# Select only purchases made by the selected households
purchases = purchases[hh_list[, .(household_code, sfirst_date, slast_date)], nomatch=0L]

# Define week_end to be the saturday ending that week. 
first_week_end = as.Date("2004-01-03")
max_purchase_date = purchases[, max(purchase_date)]
purchases[purchase_date<=first_week_end, week_end:=first_week_end]
cweek = first_week_end
while (cweek < max_purchase_date){
  cweek = cweek + 7
  purchases[purchase_date<=cweek & is.na(week_end), week_end:=cweek]
}

# Adjust price to average price per unit of serving
purchases[, price_paid := (total_price_paid - coupon_value)/(quantity*size1_amount)]

# Merge in household projection factor
purchases = purchases[hh_list[, .(household_code, projection_factor)], nomatch=0L]

# Restrict the purchases made between the first purchase of software and 
# the last purchase of software.
setkeyv(purchases, c("household_code", "purchase_date", "trip_code_uc"))
orig_purch = copy(purchases)
purchases = purchases[purchase_date>=sfirst_date & purchase_date <=slast_date, ]

# ----------------------------------------------------------------------------------------------------#

# Split the purchases into RMS stores, non-RMS stores but with store code, 
# and no store codes
setkeyv(purchases, c("store_code_uc", "panel_year"))
stores[, in_rmst := 1]
store_temp = stores[, .(store_code_uc, panel_year, in_rmst)]
purchases = store_temp[purchases]
purchases[, `:=`(in_rms = as.numeric(!is.na(in_rmst)), in_rmst = NULL)]
purch_rstores = purchases[in_rms == 1, ]
purch_ostores = purchases[in_rms == 0 & store_code_uc!=0, ]
purch_nstores = purchases[store_code_uc==0, ]
rm(purchases, store_temp)

# ----------------------------------------------------------------------------------------------------#
# It's not plausible to consider all brands either
# There are a total of 55 brands excluding the VUE line. 
brand_sales_all = orig_purch[ptype=="KEURIG" & panel_year>=2009 & panel_year<=2013, 
                             .(first_date = min(week_end),
                               last_date = max(week_end),
                               total_paid = sum(total_price_paid-coupon_value),
                               npurch = .N),
                             by = c("brand_descr")]
brand_sales_all[, all_expenditure := sum(total_paid)]
brand_sales_all[, eshare := total_paid/all_expenditure]
brand_sales_all=brand_sales_all[order(-eshare), ]
brand_sales_all[,cumshare := cumsum(eshare)]

brand_sales = purch_rstores[ptype=="KEURIG" & panel_year>=2009 & panel_year<=2013, 
                            .(first_date = min(week_end),
                              last_date = max(week_end),
                              total_paid = sum(total_price_paid-coupon_value),
                              npurch = .N),
                            by = c("brand_descr")]
brand_sales[, all_expenditure := sum(total_paid)]
brand_sales[, eshare := total_paid/all_expenditure]
brand_sales=brand_sales[order(-eshare), ]
brand_sales[, cumshare := cumsum(eshare)]

# Some brands seem to be extremely small.
# I attempt to exclude them.
brand_exclude_list_3000 = brand_sales[total_paid<=3000, brand_descr]
brand_exclude_list_8000 = brand_sales[total_paid<=8000, brand_descr]
brand_exclude_list_3000 = c(brand_exclude_list_3000, "WHITE COFFEE", "COPPER MOON")
brand_exclude_list_8000 = c(brand_exclude_list_8000, "WHITE COFFEE", "COPPER MOON")

# purchases[brand_descr %in% brand_exclude_list, brand_descr:="OTHER KEURIG"]

# ----------------------------------------------------------------------------------------------------#
# Obtain RMS Shopping Environment
# Extract unique store week_end combinations
store_week_end = unique(purch_rstores[, .(store_code_uc, week_end)])
setkeyv(store_week_end, c("store_code_uc", "week_end"))

# Extract RMS data only for the listed store and weeks
rms_prices = data.table(NULL)
for (mod in focal_module){
  load(paste(RMS_input_dir, "/", mod, ".RData", sep=""))
  move[, `:=`(feature=NULL, display=NULL)]
  setkeyv(move, c("store_code_uc", "week_end"))
  rms_prices = rbindlist(list(rms_prices, move[store_week_end, nomatch=0L]))
  rm(move)
  gc()
}

# ----------------------------------------------------------------------------------------------------#
# Construct the price weights for ground coffee for each individual and collectively for all panelists.
# The individual weights are for brands, and collective weights are for products within a brand

# Individual weights
# Find the last and first purchase of the brands, and total spent inclusive of coupons
hh_brand_sales = orig_purch[ptype=="OTHER", .(first_brand_date = min(week_end),
                                              last_brand_date = max(week_end),
                                              total_paid = sum(total_price_paid-coupon_value)),
                            by = c("household_code", "brand_descr")]
setkey(hh_brand_sales, household_code)

# Normalize the sales to take care of product availibility
hh_brand_sales = hh_brand_sales[hh_list[,.(household_code, dma_code, y2004, y2005, y2006, y2007,
                                           y2008, y2009, y2010, y2011, y2012, y2013)], nomatch=0L]
hh_brand_sales[, `:=`(b_init_date = min(first_brand_date), b_end_date = max(last_brand_date)), 
               by = c("brand_descr", "dma_code")]
hh_brand_sales[, `:=`(h_init_date = min(first_brand_date), h_end_date = max(last_brand_date)), 
               by = c("household_code")]
hh_brand_sales[, `:=`(init_date = pmax(b_init_date, h_init_date), 
                      end_date = pmin(b_end_date, h_end_date))]
hh_brand_sales[, `:=`(first_year = year(init_date), 
                      last_year = year(end_date))]

hh_brand_sales[, nweeks := as.numeric(end_date-init_date)/7 + 1]

# Get rid of products present for less than 5 weeks
hh_brand_sales[nweeks>=5, ]

ylist = c(2004:2013)
for (yr in ylist){
  varname = paste("y",yr,sep="")
  setnames(hh_brand_sales, varname, "fvar")
  hh_brand_sales[first_year<yr & last_year>yr & fvar==0, nweeks := nweeks-52]
  setnames(hh_brand_sales, "fvar", varname)
}

# Compute weekly paid conditional on consumer presence in the panel.
hh_brand_sales[, week_paid := total_paid/nweeks]

# Compute the sales by upcs within brands
prod_brand_sales = orig_purch[ptype=="OTHER", 
                              .(first_date = min(week_end),
                                last_date = max(week_end),
                                total_paid = sum(projection_factor*(total_price_paid-coupon_value))),
                                by = c("brand_descr","upc","upc_ver_uc")]
prod_brand_sales[, nprod_weeks:=as.numeric(last_date-first_date)/7 + 1]

# Compute the brands sales
prod_brand_sales[, `:=`(first_brand_date = min(first_date),
                        last_brand_date  = max(last_date),
                        total_brand_paid = sum(total_paid)),
                 by = c("brand_descr")]
prod_brand_sales[, nbrand_weeks:=as.numeric(last_brand_date-first_brand_date)/7 + 1]
prod_brand_sales[, `:=`(prod_avg_sales = total_paid/nprod_weeks,
                        brand_avg_sales = total_brand_paid/nbrand_weeks)]
prod_brand_sales[, prod_weight := prod_avg_sales/brand_avg_sales]

# ---------------------------------------------------------------------------------------------------- #
# Rather than relying imputation. For now, I only consider purchases made in RMS stores. 
# The purchases made in other stores will be imputed later, and only serve as state and 
# inventory shifters for now. 

# Merge in product information for RMS prices
setkeyv(rms_prices, c("upc", "upc_ver_uc"))
rms_prices=rms_prices[products[, .(upc, upc_ver_uc, upc_descr, size1_amount, multi, brand_descr, ptype)], nomatch=0L]
rms_prices[, price := price/(size1_amount*multi)]
rms_norder = names(rms_prices)

# Map UPC description if private label
ctl_br_list = unique(products[ptype=="KEURIG"&brand_descr=="CTL BR", .(upc_descr, size1_amount, multi, brand_descr)])
ctl_br_list[, mx:=1]
setkeyv(rms_prices, c("upc_descr", "size1_amount", "multi", "brand_descr"))
setkeyv(ctl_br_list, c("upc_descr", "size1_amount", "multi", "brand_descr"))
rms_prices = ctl_br_list[rms_prices]
rms_prices[!is.na(mx), ptype:="KEURIG"]
rms_prices[, mx:=NULL]
setkeyv(rms_prices, c("upc", "upc_ver_uc"))
setcolorder(rms_prices, rms_norder)

# Split the RMS price environment into two-parts - the part with ground coffee, and the part for KEURIG
rms_prices_keurig = rms_prices[ptype=="KEURIG"]
rms_prices_other = rms_prices[ptype=="OTHER"]
rm(rms_prices)

# Create the price index for brands for 
# merge in weights for products to create price index for brands in ground
setkeyv(prod_brand_sales, c("upc", "upc_ver_uc"))
rms_prices_other = rms_prices_other[prod_brand_sales[, .(upc, upc_ver_uc, prod_weight)], nomatch=0L]
rms_prices_other = rms_prices_other[, .(price=weighted.mean(price, prod_weight)), 
                                    by = c("store_code_uc","brand_descr", "week_end")]
setkey(rms_prices_other, brand_descr)

# It's not possible to create cartesian products and then aggregate - memory explosion.
# Now execute in loops over households to create idiosyncratic price index for each households
hh_code_loop = unique(hh_brand_sales[, household_code])
setkeyv(purch_rstores, NULL)
hh_store_week = unique(purch_rstores[, .(household_code, store_code_uc, week_end)])
setkey(hh_store_week, household_code, store_code_uc, week_end)

# Do so in parallel would save time. 
# Function to compute price index
hh_ofun <- function(hh_code){
  hh_temp = hh_brand_sales[.(hh_code), .(household_code, brand_descr, week_paid)]
  setkey(hh_temp, brand_descr)
  rms_temp = hh_temp[rms_prices_other, nomatch=0L]
  rms_temp = rms_temp[, .(oprice = weighted.mean(price, week_paid)),
                      by = c("household_code", "store_code_uc", "week_end")]
  setkeyv(rms_temp, c("household_code", "store_code_uc", "week_end"))
  return(hh_store_week[.(hh_code),][rms_temp, nomatch=0L])
}

# Export necessary functions and objects to workers. 
clusterExport(cl,c('hh_brand_sales', 'rms_prices_other', 'hh_store_week', 'hh_ofun'))

# Parallel Execution
rms_hh_other_prices = parLapply(cl, hh_code_loop, hh_ofun)
rms_hh_other_prices = rbindlist(rms_hh_other_prices)

setkeyv(rms_hh_other_prices, c("household_code", "store_code_uc", "week_end"))

# ----------------------------------------------------------------------------------------------------#

# Aggregate across flavors of the same size and brand to create a single price index for each pair. 
rms_prices_keurig[, aprice:=mean(price, na.rm=TRUE), 
                  by = c("store_code_uc", "week_end", "brand_descr", "size1_amount")]
rms_prices_keurig[, cor(price, aprice)] # Correlation is 0.978962, or 0.9583666 of the variation.

rms_prices_keurig=rms_prices_keurig[, .(aprice=mean(price, na.rm=TRUE)), 
                  by = c("store_code_uc", "week_end", "brand_descr", "size1_amount")]

# ----------------------------------------------------------------------------------------------------#

# Aggregate brand size - in other words, ignore variety seeking in flavors
setkeyv(purch_rstores, c("household_code", "purchase_date", "trip_code_uc", "store_code_uc"))
purch_rstores[, `:=`(purchased = 1, trip_id = c(1:.N))]
purch_rstores[ptype=="OTHER", brand_descr:="OTHER"]

# Initialize the complete purchase panel
hh_rms_panel = as.data.table(expand.grid(trip_id = purch_rstores[, trip_id], 
                                         brand_descr = c(unique(rms_prices_keurig$brand_descr)),
                                         size1_amount = unique(rms_prices_keurig[, size1_amount])))
hh_rms_panel_2 = data.table(trip_id = purch_rstores[, trip_id])
hh_rms_panel_2[, `:=`(brand_descr = "OTHER", size1_amount=NA)]
hh_rms_panel = rbindlist(list(hh_rms_panel, hh_rms_panel_2))
rm(hh_rms_panel_2)
setkey(hh_rms_panel, trip_id, brand_descr, size1_amount)

# Recode the size1_amount to the closet match in RMS, and rescale price
purch_rstores_keurig = purch_rstores[ptype=="KEURIG", ]
purch_rstores_keurig[, price_paid:=price_paid * size1_amount*multi]
size_list = sort(unique(rms_prices_keurig[, size1_amount]))
k = 0
for (s in size_list){
  k = k+1
  if (s == 1){
    mid_e = (s + size_list[k+1])/2
    purch_rstores_keurig[size1_amount<=mid_e, size1_amount:=s]
  } else if (s==80){
    mid_i = (s[k] + size_list[k-1])/2
    purch_rstores_keurig[size1_amount>=mid_i, size1_amount:=s]
  } else{
    mid_i = (s + size_list[k-1])/2
    mid_e = (s + size_list[k+1])/2
    purch_rstores_keurig[size1_amount>mid_i & size1_amount<=mid_e, size1_amount:=s]
  }
}
purch_rstores_keurig[, price_paid:=price_paid/(size1_amount*multi)]

# Merge in trip information and purchase and price
setkey(purch_rstores_keurig, trip_id, brand_descr, size1_amount)
setkey(hh_rms_panel, trip_id, brand_descr, size1_amount)
hh_rms_panel=purch_rstores_keurig[,.(trip_id, brand_descr, size1_amount, purchased, price_paid)][hh_rms_panel]

# Merge in trip information and purchase and price
purch_rstores_other = purch_rstores[ptype=="OTHER", ]
setnames(purch_rstores_other, c("size1_amount", "purchased", "price_paid"),
         c("osize1_amount", "opurchased", "oprice_paid"))
setkey(purch_rstores_other, trip_id, brand_descr)
setkey(hh_rms_panel, trip_id, brand_descr)
hh_rms_panel=purch_rstores_other[,.(trip_id, brand_descr, osize1_amount, opurchased, oprice_paid)][hh_rms_panel]
hh_rms_panel[brand_descr=="OTHER", `:=`(size1_amount = osize1_amount, purchased  = opurchased,
                                        price_paid = oprice_paid)]
hh_rms_panel[,`:=`(osize1_amount=NULL, opurchased = NULL, oprice_paid = NULL)]
hh_rms_panel[is.na(purchased), purchased:=0]

# Check all trip occasion has a purchase decision.
hh_rms_panel[, npurch := sum(purchased), by = trip_id]
trip_list = unique(hh_rms_panel[npurch==0, trip_id])
table(hh_rms_panel[, sum(purchased), by = trip_id][,V1])
hh_rms_panel = hh_rms_panel[npurch>0.1,]
hh_rms_panel[, npurch:=NULL]

# Merge in trip information including household_code, store_code, week_end
setkey(purch_rstores, trip_id)
setkey(hh_rms_panel, trip_id)
hh_rms_panel=purch_rstores[,.(trip_id, household_code, store_code_uc, week_end,
                              trip_code_uc)][hh_rms_panel]

# Merge in prices of all other Keurig products. aprice
setkeyv(rms_prices_keurig, c("store_code_uc", "week_end", "brand_descr", "size1_amount"))
setkeyv(hh_rms_panel, c("store_code_uc", "week_end", "brand_descr", "size1_amount"))
hh_rms_panel = rms_prices_keurig[,.(store_code_uc, week_end, brand_descr, size1_amount, aprice)][hh_rms_panel]

# Merge in prices of outside option. oprice
rms_hh_other_prices[, brand_descr:="OTHER"]
setkeyv(rms_hh_other_prices, c("household_code", "store_code_uc", "week_end", "brand_descr"))
setkeyv(hh_rms_panel, c("household_code", "store_code_uc", "week_end", "brand_descr"))
hh_rms_panel = rms_hh_other_prices[,.(household_code, store_code_uc, week_end, brand_descr, oprice)][hh_rms_panel]

# Construct a single price for all alternatives
hh_rms_panel[, price:=ifelse(brand_descr=="OTHER", oprice, ifelse(purchased==1, price_paid, aprice))]
hh_rms_panel[is.na(price) & !is.na(price_paid), price:=price_paid]
hh_rms_panel = hh_rms_panel[!is.na(price), ]
hh_rms_panel[,`:=`(oprice=NULL, aprice=NULL, price_paid=NULL)]
hh_rms_panel[brand_descr=="OTHER", size1_amount:=0]
setkeyv(hh_rms_panel, c("household_code", "week_end", "store_code_uc", "trip_id"))

# -------------------------------------------------------------------------------------------------------# 
# Now impute the state dependence variables - last purchase and cumulative purchase without interruption
# Run this in parallel 
setkeyv(orig_purch, c("household_code", "purchase_date", "trip_code_uc", "store_code_uc"))
state_func <- function(hh_code){
  hpurch = orig_purch[.(hh_code), ]
  nobs = nrow(hpurch)
  ptype_lag = rep(as.character(NA), nobs)
  brand_lag = rep(as.character(NA), nobs)
  brand_cum = rep(0, nobs)
  for (i in 1:nobs){
    if (i==1) {
      ptype_lag[i]=as.character(NA)
      brand_lag[i]=as.character(NA)
      brand_cum[i]=0
      ptype_state = hpurch[i,]$ptype
      brand_state = hpurch[i,]$brand_descr
      trip_state = hpurch[i,]$trip_code_uc
      cum_state = hpurch[i, quantity*size1_amount]
    } else{
      cptype_state = hpurch[i,]$ptype
      cbrand_state = hpurch[i,]$brand_descr
      ctrip_state = hpurch[i,]$trip_code_uc
      if (ctrip_state != trip_state){
        ptype_lag[i] = ptype_state
        brand_lag[i] = brand_state
        brand_cum[i] = cum_state
        if (brand_state == cbrand_state){
          cum_state = cum_state + hpurch[i, quantity*size1_amount]
        } else{
          cum_state = hpurch[i, quantity*size1_amount]
        }
        ptype_state = cptype_state
        brand_state = cbrand_state
        trip_state = ctrip_state
      } else{
        ptype_lag[i] = ptype_lag[i-1]
        brand_lag[i] = brand_lag[i-1]
        brand_cum[i] = brand_cum[i-1]
        if (brand_state == cbrand_state){
          cum_state = cum_state + hpurch[i, quantity*size1_amount]
        } else{
          cum_state = hpurch[i, quantity*size1_amount]
        }
        ptype_state = cptype_state
        brand_state = cbrand_state
      }
    }
  }
  return(data.table(ptype_lag = ptype_lag, brand_lag = brand_lag, brand_cum = brand_cum))
}

hh_codes = unique(orig_purch$household_code)
clusterExport(cl, c('orig_purch', 'state_func'))
state_list = parLapply(cl, hh_codes, state_func)
state_list = rbindlist(state_list)
orig_purch = cbind(orig_purch, state_list)
rm(state_list)

# -------------------------------------------------------------------------------------------------------# 
# Merge in state variables for purchases made in RMS stores to get trip_id 
setkeyv(purch_rstores, c("household_code", "trip_code_uc", "store_code_uc", "upc", "upc_ver_uc"))
setkeyv(orig_purch, c("household_code", "trip_code_uc", "store_code_uc", "upc", "upc_ver_uc"))
purch_rtemp = purch_rstores[, .(household_code, trip_code_uc, store_code_uc, 
                               upc, upc_ver_uc, trip_id)]
purch_rstores_state = orig_purch[, .(household_code, trip_code_uc, store_code_uc, 
                                     upc, upc_ver_uc, brand_lag, brand_cum)][purch_rtemp, nomatch=0L]

# Merge into the hh_rms_panel
purch_rstate = purch_rstores_state[, .(trip_id, brand_lag, brand_cum)]
purch_rstate[, brand_descr := brand_lag]
setkeyv(purch_rstate, c("trip_id", "brand_descr"))
setkeyv(hh_rms_panel, c("trip_id", "brand_descr"))
hh_rms_panel = purch_rstate[hh_rms_panel]
hh_rms_panel[, brand_lag:=as.numeric(!is.na(brand_lag))]
hh_rms_panel[, brand_cum:=ifelse(is.na(brand_cum), 0, brand_cum)]

# Now create the data panel 
hh_rms_panel[, npurch := sum(purchased), by = "household_code"]
hh_rms_panel = hh_rms_panel[npurch>=rms_purch_criteria, ]
hh_rms_panel[, hh := .GRP, by = "household_code"]
hh_rms_panel[brand_descr == "OTHER", brand_descr:="0OTHER"]
setkeyv(hh_rms_panel, c("trip_id", "hh", "brand_descr", "size1_amount"))
hh_rms_panel[, brand_descr_lin := ifelse(brand_descr %in% brand_exclude_list_8000, "OTHER KEURIG", brand_descr)]
hh_rms_panel[, brand := .GRP, by = "brand_descr_lin"]
hh_panel = hh_rms_panel[, .(hh, trip_id, brand, price, size1_amount, brand_lag, brand_cum, purchased)]

# Create the brand dummies - and prepare data
br_list = sort(hh_panel[, unique(brand)])
bvars = paste0("a", br_list)
dbt = data.table(model.matrix(~factor(hh_panel$brand)-1))
setnames(dbt, names(dbt), bvars)
hh_panel = cbind(hh_panel, dbt)
name_order = c("hh", "trip_id", "brand", bvars, "price", "size1_amount", "brand_lag", "brand_cum", "purchased")
setcolorder(hh_panel, name_order)
setnames(hh_panel, "trip_id", "t")

# Make panel balanced # Only necessary if running for julia
hh_panel[, product:=1:(.N), by = c("t")]
hh_julia = as.data.table(expand.grid(t = unique(hh_panel$t),
                                     product = 1:max(hh_panel$product)))
setkeyv(hh_panel, c("t", "product"))
setkeyv(hh_julia, c("t", "product"))
hh_julia = hh_panel[hh_julia]
hh_julia[, hh := as.integer(mean(hh, na.rm=TRUE)), by ="t"]
nlist = names(hh_julia)
nlist = setdiff(nlist, c("hh","t","product"))
hh_julia[, avail := as.numeric(!is.na(brand))]
for (v in nlist){
  setnames(hh_julia, v, "vx")
  hh_julia[is.na(vx), vx:=0]
  setnames(hh_julia, "vx", v)
}

# Export/save data
write.csv(hh_julia, file = paste(output_dir,"/keurig_julia.csv", sep=""), row.names=FALSE)
save(hh_panel, hh_julia, hh_rms_panel, file = paste(output_dir,"/keurig_panel.RData", sep=""))

# Optimization code takes too long in julia ... Try to take a sample of 1000 households 
hh_samp = sort(sample(unique(hh_julia$hh), 1000))
hh_julia = hh_julia[ hh %in% hh_samp, ]
hh_julia[, hh:=.GRP, by = "hh"]
hh_julia[, t:=.GRP, by = "t"]
write.csv(hh_julia, file = paste(output_dir,"/keurig_julia_samp.csv", sep=""), row.names=FALSE)
