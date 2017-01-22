#---------------------------------------------------------------------------------------------------#
# Load appropriate data
# Load HH file with flags
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))
load(paste(meta_dir, "/HH.RData", sep=""))

# Load HMS Trips Data
load(paste(HMS_trip_dir, "/Trips.RData", sep=""))

# Merge back into trips data to get only the relevant trips.
trips = trips[household_code %in% hh_list[, household_code]]

# Only Trips Beyond 2008 matters 
trips = trips[panel_year>=2008, ]
setkeyv(trips, c("household_code", "purchase_date", "trip_code_uc"))

# Load Product information
load(paste(meta_dir, "/Products.RData", sep=""))
products[.("064964520029", 1), upc_descr:="KK-C CF BRWR K45/GCSC&WF="] # is actually K45
products[.("064964500440", 1), upc_descr:="KRIG CF BRWR B44"] # is actually B44

# Classify products based on features, series and prices
c_list = as.list(1:5)
c_list[[1]] = c("B30", "K130")
c_list[[2]] = c("B31", "K10")
c_list[[3]] = c("B40", "K40", "K45", "B44")
c_list[[4]] = c("B60", "K60", "K65", "B66", "B50")
c_list[[5]] = c("B70", "K70", "B77", "K75")
#c_list[[4]] = c("B66", "B50")
#c_list[[4]] = c("B60", "K60", "K65")
#c_list[[5]] = c("K70/GCSC", "B77/GCSC=")
#c_list[[6]] = c("B70/FGCSC&TSC", "K70/GCSC&WF=", "K70/GCSC=")

products[, series:= as.integer(NA)]
for (i in c(1:5)){
  for (b in c_list[[i]]){
    products[grepl(b, upc_descr) & product_module_code == 7755 & ptype=="KEURIG", series:= i ]
  }
}

# Load store information
load(paste(meta_dir, "/Stores.RData", sep=""))
load(paste(meta_dir, "/Retailers.RData", sep=""))

# Obtain the list of retailers in RMS - parent_code and retailer_code
stores[, `:=`(nretailer_codes = length(na.omit(unique(retailer_code)))), by = "store_code_uc"]
stores[nretailer_codes==0, `:=`(retailer_code = parent_code)]
stores[nretailer_codes==1, `:=`(retailer_code = na.omit(retailer_code)[1]), by = "store_code_uc"]
# For the rest of the stores roll to the last
stores_temp = stores[!is.na(retailer_code), ]
setnames(stores_temp, "retailer_code", "retailer_code_temp")
setkeyv(stores_temp, c("store_code_uc", "parent_code", "panel_year"))
setkeyv(stores, c("store_code_uc", "parent_code", "panel_year"))
stores = stores_temp[, .(store_code_uc, parent_code, panel_year, 
                         retailer_code_temp)][stores, roll=TRUE, rollends=c(TRUE, TRUE)]
stores[is.na(retailer_code), retailer_code:=retailer_code_temp]
stores[, retailer_code_temp:=NULL]

# Load HMS Purchase data
load(paste(HMS_input_dir, "/HMS-Purchases-Coffee.RData", sep=""))

# coffee trip code list
coffee_trip_code_list = unique(purchases[as.integer(product_module_code)==1463, trip_code_uc])

# Restrict purchases to specific software purchases
purchases = purchases[product_module_code %in% maker_modules, ]

# Make purchases trip, product unique
pnamelist = names(purchases)
pnamelist = setdiff(pnamelist, c("total_spent", "total_price_paid", "coupon_value", "deal_flag_uc","quantity"))

# Make purchase trip-product unique
purchases = purchases[, .(total_price_paid = sum(total_price_paid),
                          coupon_value = sum(coupon_value),
                          quantity = sum(quantity)), by = pnamelist]

# Merge in the type information to the purchase data
setkeyv(purchases, c("upc", "upc_ver_uc"))
purchases=purchases[products[, .(upc, upc_ver_uc, upc_descr, ptype, brand_descr,
                                 size1_amount, multi, series)], nomatch=0L]
purchases[, month:=as.factor(format(purchase_date, '%Y-%m'))]

# Focus on the platform of choice
purchases = purchases[ptype=="KEURIG" & brand_descr=="KEURIG", ]

# Only keep households in the top N DMAs in terms of adopters
# Households double count if moved. 
dma_rank_list = unique(hh_info_panel[as.integer(kholder)==1, .(household_code, dma_code)]) 
dma_rank_list = dma_rank_list[, .(nhh = .N), by = "dma_code"]
dma_rank_list = dma_rank_list[order(-nhh), ]
dma_rank_list[, cumshare := cumsum(nhh/sum(nhh))]
top_dma_list = dma_rank_list[1:30, dma_code]

#---------------------------------------------------------------------------------------------------#

# Define week_end to be the saturday ending that week. 
first_week_end = as.Date("2005-12-10")
max_purchase_date = purchases[, max(purchase_date)]
purchases[purchase_date<=first_week_end, week_end:=first_week_end]
cweek = first_week_end
while (cweek < max_purchase_date){
  cweek = cweek + 7
  purchases[purchase_date<=cweek & is.na(week_end), week_end:=cweek]
}

# Adjust price to average price per unit of serving
purchases[, price_paid := (total_price_paid - coupon_value)/(quantity)]
purchases[, price := total_price_paid/(quantity)]

# Locate the households and thus the purchases
setkeyv(purchases, c("household_code", "panel_year"))
setkeyv(hh, c("household_code", "panel_year"))
purchases = hh[,.(household_code, panel_year, dma_code, projection_factor)][purchases, nomatch=0L]

# Obtain the table where the machine is bought
sort(purchases[, table(retailer_code)])

# Cross retailer price correlation check - any market
hms_prices = purchases[, .(price = mean(price_paid)), by = c("retailer_code", "week_end", "series")]
setkeyv(hms_prices, c("retailer_code", "week_end", "series"))
r4401_hms_prices = hms_prices[retailer_code == 4401, ]
r9005_hms_prices = hms_prices[retailer_code == 9005, ]
setkeyv(r4401_hms_prices, c("week_end", "series"))
setkeyv(r9005_hms_prices, c("week_end", "series"))
r4401_hms_prices[r9005_hms_prices, nomatch=0L][series==3, cor(price, i.price)]

# Cross retailer price correlation check - within a market
hms_prices = purchases[, .(price = mean(price_paid)), by = c("dma_code", "retailer_code", "week_end", "series")]
setkeyv(hms_prices, c("dma_code", "retailer_code", "week_end", "series"))
r4401_hms_prices = hms_prices[retailer_code == 4401 & dma_code==501, ]
r9005_hms_prices = hms_prices[retailer_code == 9005 & dma_code==501, ]
setkeyv(r4401_hms_prices, c("week_end", "series"))
setkeyv(r9005_hms_prices, c("week_end", "series"))
r4401_hms_prices[r9005_hms_prices, nomatch=0L][series==3, cor(price, i.price)]

# Cross Market within retailer price correlation check
chicago_hms_prices = hms_prices[dma_code == 602 & retailer_code==9005, ]
newyork_hms_prices = hms_prices[dma_code == 501 & retailer_code==9005, ]
setkeyv(chicago_hms_prices, c("week_end", "series"))
setkeyv(newyork_hms_prices, c("week_end", "series"))
chicago_hms_prices[newyork_hms_prices, nomatch=0L][series==2, cor(price, i.price)]

# ----------------------------------------------------------------------------------------------------#
# Obtain the share and sales of top products
prod_sales = purchases[ptype=="KEURIG"&brand_descr!="MR COFFEE", .(revenue = sum(price_paid), np = .N),
                       by = c("upc", "upc_ver_uc", "upc_descr", "brand_descr", "series")]
prod_sales[, `:=`(rshare = revenue/sum(revenue), pshare = np/sum(np),
                  aprice = revenue/np)]

# To illustrate the type of data we have including market share, and the similarity of prices within a series.
prod_sales[order(-pshare), `:=`(crshare = cumsum(rshare), cpshare = cumsum(pshare))]
prod_sales[order(-pshare)]
prod_sales[order(series), .(upc, upc_ver_uc, upc_descr, series, aprice, revenue, np, pshare)]

# ----------------------------------------------------------------------------------------------------#
# Obtain RMS Shopping Environment
load(paste(RMS_input_dir, "/7755.RData", sep=""))
move[, `:=`(feature=NULL, display=NULL)]
setkey(move, upc, upc_ver_uc)
setkey(products, upc, upc_ver_uc)
move = move[products[, .(upc, upc_ver_uc, upc_descr, brand_descr, series)], nomatch=0L]
setkeyv(move, c("store_code_uc", "panel_year"))
setkeyv(stores, c("store_code_uc", "panel_year"))
move = move[stores[, .(store_code_uc, panel_year, dma_code, retailer_code)], nomatch=0L]

# Only consider purchases of Keurig Machines -- not anything else.
move = move[brand_descr=="KEURIG", ]
move = move[!is.na(series)]

# Obtain the list of retailers, stores, by week - to be used to understand variety in selection.
store_first_last_week= move[, .(first_week_observed = min(week_end), last_week_end = max(week_end)), 
                            by = c("store_code_uc")]
store_list = unique(move$store_code_uc)
week_list = unique(move$week_end)
store_exists_panel = data.table(expand.grid(store_code_uc = store_list, week_end = week_list))
setkey(store_exists_panel, store_code_uc)
setkey(store_first_last_week, store_code_uc)
store_exists_panel = store_exists_panel[store_first_last_week, nomatch=0L]
store_exists_panel = store_exists_panel[week_end>=first_week_observed & week_end<=last_week_end, nomatch=0L]
store_exists_panel[, panel_year := year(week_end)]
setkeyv(store_exists_panel, c("store_code_uc", "panel_year"))
setkeyv(stores, c("store_code_uc", "panel_year"))
store_exists_panel = store_exists_panel[stores[, .(store_code_uc, panel_year, 
                                                   dma_code, retailer_code)], nomatch=0L]
setkey(store_exists_panel, dma_code, retailer_code, store_code_uc, week_end)

# Filter out outliers
move[, pmedian:=median(price), by = c("upc", "upc_ver_uc")]
move = move[price<=2*pmedian & price>=(1/3 * pmedian), ]

# Obtain the average and median price by retailer.
rms_hw_prices = move[, .(price_avg=mean(price), price_mid = median(price), price_sd = sd(price), 
                         revenue = sum(units * price), units = sum(units), 
                         nstores = length(unique(store_code_uc))), 
                         by = c("retailer_code", "series", "week_end")]
setkeyv(rms_hw_prices, c("retailer_code", "series", "week_end"))

# Obtain the first and last position of the retailer and series combination
rms_hw_prices[, row:=1:.N]
retailer_series_position = rms_hw_prices[, list(start=min(row), end=max(row)), 
                                              by = c("retailer_code", "series")]
rms_hw_prices[, price_regular:=baseprice(retailer_series_position, rms_hw_prices, 14)]

# Filter weeks where less than 15% of the stores sold the product -- probably the product is not available. 
nstore_panel = store_exists_panel[, .(tot_stores = .N), by = c("retailer_code", "week_end")]
setkey(rms_hw_prices, retailer_code, week_end)
setkey(nstore_panel, retailer_code, week_end)
rms_hw_prices = rms_hw_prices[nstore_panel[, .(retailer_code, week_end, tot_stores)], nomatch=0L]
rms_hw_prices = rms_hw_prices[nstores/tot_stores>=0.15 & tot_stores>=5, ]

# Get rid of retailers with less than a year of data (52 Weeks).
rms_hw_prices[, nweek:=.N, by = "retailer_code"]
rms_hw_prices = rms_hw_prices[nweek>=52, ]
save(rms_hw_prices, file = paste(output_dir, "/RMS-HW-Prices.RData", sep=""))

# ----------------------------------------------------------------------------------------------------#
# Obtain the HMS prices, and the shopping environment for the following list of retailers
purchases = purchases[!is.na(series)]

# Get rid of prices that are outliers
# Filter out outliers
purchases[, pmedian:=median(price), by = c("upc", "upc_ver_uc")]
purchases = purchases[price<=2*pmedian & price>=(1/3 * pmedian), ]

# Obtain the series prices -- it looks that I cannot treat all retailers the same.
hms_series_prices = purchases[, .(price = mean(price), price_paid = mean(price_paid), units = sum(quantity)),
                       by = c("series", "week_end")]
setkeyv(hms_prices, c("series", "week_end"))
retailer_series_prices = purchases[, .(price = mean(price), price_paid = mean(price_paid), 
                                       price_mid = median(price), price_paid_mid = median(price_paid),
                                       units = sum(quantity)),
                              by = c("retailer_code", "series", "week_end")]
retailer_list = c(9005, 4401, 6920, 9101, 9103, 9006, 9001)

# Initialize Retailer, series, and time panel. 
week_list = sort(unique(rms_hw_prices$week_end))
hms_hw_prices = data.table(expand.grid(retailer_code = retailer_list, series = 1:5, week_end = week_list))
setkey(hms_hw_prices, retailer_code, week_end, series)

# Obtain the first and last date of purchase, and then restrain the dates to the date of existence
retailer_dates = purchases[, .(first_week_end = min(week_end), last_week_end = max(week_end)),
                           by = c("retailer_code", "series")]
setkey(retailer_dates, retailer_code, series)
setkey(hms_hw_prices, retailer_code, series)
hms_hw_prices = hms_hw_prices[retailer_dates, nomatch=0L]
hms_hw_prices = hms_hw_prices[week_end>=first_week_end & week_end<=last_week_end, ]

# The imputation is done by looking at the price series. Given scarcity of data, 
# this is the best that I can do. 
# Imputation prices retailer by retailer
hms_hw_prices[, price_regular := as.numeric(NA)]
# Retailer 9005
hms_hw_prices[retailer_code==9005&series==1&week_end<="2009-08-22", price_regular:=99.99]
hms_hw_prices[retailer_code==9005&series==1&week_end>="2009-08-29", price_regular:=119.99]
hms_hw_prices[retailer_code==9005&series==2&week_end<="2011-08-27", price_regular:=114.99]
hms_hw_prices[retailer_code==9005&series==2&week_end>="2011-09-03", price_regular:=124.99]
hms_hw_prices[retailer_code==9005&series==3&week_end<="2011-02-19", price_regular:=129.99]
hms_hw_prices[retailer_code==9005&series==3&week_end>="2011-02-26"&week_end<="2011-09-03", 
              price_regular:=139.99]
hms_hw_prices[retailer_code==9005&series==3&week_end>="2011-09-10", price_regular:=149.99]
hms_hw_prices[retailer_code==9005&series==4&week_end<="2011-02-19", price_regular:=169.99]
hms_hw_prices[retailer_code==9005&series==4&week_end>="2011-02-26"&week_end<="2011-10-01", 
              price_regular:=179.99]
hms_hw_prices[retailer_code==9005&series==4&week_end>="2011-10-08", price_regular:=189.99]
hms_hw_prices[retailer_code==9005&series==5&week_end<="2011-02-19", price_regular:=209.99]
hms_hw_prices[retailer_code==9005&series==5&week_end>="2011-02-26"&week_end<="2011-10-01", 
              price_regular:=219.99]
hms_hw_prices[retailer_code==9005&series==5&week_end>="2011-10-08", price_regular:=239.99]
# Retailer 4401
hms_hw_prices[retailer_code==4401&series==1&week_end<="2009-06-06", price_regular:=79.99]
hms_hw_prices[retailer_code==4401&series==1&week_end>="2009-06-13"&week_end<="2010-02-20", 
              price_regular:=89.99]
hms_hw_prices[retailer_code==4401&series==1&week_end>="2010-02-27"&week_end<="2010-03-06", 
              price_regular:=69.98]
hms_hw_prices[retailer_code==4401&series==1&week_end>="2010-03-13"&week_end<="2010-03-27", 
              price_regular:=55.99]
hms_hw_prices[retailer_code==4401&series==1&week_end>="2010-04-03", price_regular:=49.99]
hms_hw_prices[retailer_code==4401&series==2, price_regular:=99.99]
hms_hw_prices[retailer_code==4401&series==3&week_end<="2011-02-26", price_regular:=109.99]
hms_hw_prices[retailer_code==4401&series==3&week_end>="2011-03-05", price_regular:=119.99]
hms_hw_prices[retailer_code==4401&series==4&week_end<="2009-03-14", price_regular:=149.99]
hms_hw_prices[retailer_code==4401&series==4&week_end>="2009-03-21"&week_end<="2010-02-20", 
              price_regular:=129.99]
hms_hw_prices[retailer_code==4401&series==4&week_end>="2010-02-27"&week_end<="2011-02-19", 
              price_regular:=139.99]
hms_hw_prices[retailer_code==4401&series==4&week_end>="2011-02-26", price_regular:=149.99]
hms_hw_prices[retailer_code==4401&series==5&week_end<="2011-02-19", price_regular:=169.99]
hms_hw_prices[retailer_code==4401&series==5&week_end>="2011-02-26", price_regular:=179.99]
# Retailer 6920
hms_hw_prices[retailer_code==6920&series==1&week_end<="2011-06-11", price_regular:=89.95]
hms_hw_prices[retailer_code==6920&series==1&week_end>="2011-06-18", price_regular:=99.99]
hms_hw_prices[retailer_code==6920&series==2, price_regular:=99.99]
hms_hw_prices[retailer_code==6920&series==3&week_end<="2010-02-20", price_regular:=99.96]
hms_hw_prices[retailer_code==6920&series==3&week_end>="2010-02-27"&week_end<="2011-01-22", 
              price_regular:=109.96]
hms_hw_prices[retailer_code==6920&series==3&week_end>="2011-01-29"&week_end<="2012-11-03", 
              price_regular:=119.95]
hms_hw_prices[retailer_code==6920&series==3&week_end>="2012-11-10", price_regular:=109.00]
hms_hw_prices[retailer_code==6920&series==4&week_end<="2013-03-23", price_regular:=149.00]
hms_hw_prices[retailer_code==6920&series==4&week_end>="2013-03-30", price_regular:=139.00]
hms_hw_prices[retailer_code==6920&series==5&week_end<="2013-03-23", price_regular:=179.00]
hms_hw_prices[retailer_code==6920&series==5&week_end>="2013-03-30", price_regular:=169.00]
# Retailer 9006
hms_hw_prices[retailer_code==9006&series==1&week_end<="2009-04-18", price_regular:=119.99]
hms_hw_prices[retailer_code==9006&series==1&week_end>="2009-04-25", price_regular:=129.99]
hms_hw_prices[retailer_code==9006&series==2&week_end<="2011-03-12", price_regular:=139.99]
hms_hw_prices[retailer_code==9006&series==2&week_end>="2011-03-19", price_regular:=149.99]
hms_hw_prices[retailer_code==9006&series==3&week_end<="2011-03-12", price_regular:=164.99]
hms_hw_prices[retailer_code==9006&series==3&week_end>="2011-03-19", price_regular:=174.99]
hms_hw_prices[retailer_code==9006&series==4&week_end<="2011-03-12", price_regular:=209.99]
hms_hw_prices[retailer_code==9006&series==4&week_end>="2011-03-19", price_regular:=219.99]
hms_hw_prices[retailer_code==9006&series==5, price_regular:=259.99]
# Retailer 9103
hms_hw_prices[retailer_code==9103&series==4&week_end<="2009-02-21", price_regular:=129.99]
hms_hw_prices[retailer_code==9103&series==4&week_end>="2009-02-28"&week_end<="2009-08-15", 
              price_regular:=119.99]
hms_hw_prices[retailer_code==9103&series==5, price_regular:=149.99]
# Retailer 9101
hms_hw_prices[retailer_code==9101&series==1, price_regular:=69.99]
hms_hw_prices[retailer_code==9101&series==2, price_regular:=79.98]
hms_hw_prices[retailer_code==9101&series==4&week_end<="2011-02-19", price_regular:=129.98]
hms_hw_prices[retailer_code==9101&series==4&week_end>="2011-02-26"&week_end<="2011-07-23", 
              price_regular:=139.98]
hms_hw_prices[retailer_code==9101&series==4&week_end>="2011-07-30", price_regular:=129.98]
# Retailer 9001
hms_hw_prices[retailer_code==9001&series==2, price_regular:=129.99]
hms_hw_prices[retailer_code==9001&series==3&week_end<="2011-02-19", price_regular:=149.99]
hms_hw_prices[retailer_code==9001&series==3&week_end>="2011-02-26", price_regular:=159.99]
hms_hw_prices[retailer_code==9001&series==4&week_end<="2011-02-19", price_regular:=179.99]
hms_hw_prices[retailer_code==9001&series==4&week_end>="2011-02-26", price_regular:=189.99]

# Drop the missing prices observations
hms_hw_prices = hms_hw_prices[!is.na(price_regular), ]

# Merge in the prices observed from HMS and flag lower prices as promotions
setkey(hms_hw_prices, retailer_code, series, week_end)
setkey(retailer_series_prices, retailer_code, series, week_end)
hms_hw_prices = retailer_series_prices[hms_hw_prices]
hms_hw_prices[,`:=`(price = NULL, units=NULL)]
hms_hw_prices[, price := price_mid]
hms_hw_prices[is.na(price), price := price_regular]
# Likely measurement error, and only 44 observations.
hms_hw_prices[price>=price_regular, price := price_regular] 
save(hms_hw_prices, file = paste(output_dir, "/HMS-HW-Prices.RData", sep=""))

# ----------------------------------------------------------------------------------------------------#
# Create the combined hardware price panel
hms_hw_prices = hms_hw_prices[, .(retailer_code, series, week_end, price, price_regular)]
rms_hw_prices = rms_hw_prices[, .(retailer_code, series, week_end, price=price_mid, price_regular)]
hms_hw_prices[, in_rms:=0]
rms_hw_prices[, in_rms:=1]
hw_prices = rbindlist(list(hms_hw_prices, rms_hw_prices))
setkey(hw_prices, retailer_code, week_end, series)
save(hw_prices, file = paste(output_dir, "/HW-Prices.RData", sep=""))

# ----------------------------------------------------------------------------------------------------#

# Compute the probability of observing different price support for households given their trip data.
# Group prices into 5 dollar increments from 34.99 dollars to 259.99 dollars. 
hw_prices[, price_int:=as.numeric(NA)]
pseq = seq(34.99, 259.99, 5)
for (pval in pseq){
  hw_prices[abs(price-pval)<=2.5, price_int := pval]
}
hw_prices = hw_prices[week_end>="2009-07-11", ]

# Define the three stages of prices  -- four stages for price distribution by shopper type
# Stage 1: 2009-07-11 - 2010-02-20 - Warehouse shopper vs not
# Stage 2: 2010-02-27 - 2011-02-19 - Warehouse shopper vs not
# Stage 3: 2011-02-26 - 2012-01-28 - Warehouse shopper vs not
# Stage 4: 2012-02-04 - Onward - Warehouse shopper vs not
#hw_prices[week_end<="2010-02-20", stage:=1]
#hw_prices[week_end<="2011-02-19" & week_end>="2010-02-27", stage:=2]
#hw_prices[week_end<="2012-01-28" & week_end>="2011-02-26", stage:=3]
#hw_prices[week_end>="2012-02-04", stage:=4]

# Load channel information
setkey(retailers, retailer_code)
setkey(hw_prices, retailer_code)
hw_prices = hw_prices[retailers, nomatch=0L]

# Obtain the retailer trips information -- exposure to prices
trips = trips[purchase_date>="2008-01-01", ]
# Define week_end to be the saturday ending that week. 
first_week_end = as.Date("2008-01-05")
max_trip_date = trips[, max(purchase_date)]
trips[purchase_date<=first_week_end, week_end:=first_week_end]
cweek = first_week_end
while (cweek <= max_trip_date){
  cweek = cweek + 7
  trips[purchase_date<=cweek & is.na(week_end), week_end:=cweek]
}

# Obtain the probabilities of observing different prices during the last few weeks/months.
hw_prob = data.table(NULL)
week_list = unique(hw_prices[, as.character(week_end)])
for (wk in week_list){
  if (as.Date(wk)<=as.Date("2009-10-03")){
    start_week = as.Date(wk)
    end_week = as.Date("2009-10-03")
  } else {
    start_week = as.Date(wk) - 7*12
    end_week = as.Date(wk)
  }
  # Treating household trip as unique trip... 
  retailer_trips = trips[purchase_date>=start_week & purchase_date<=end_week, 
                     .(ntrips = .N), by = c("retailer_code")]
  hw_prices_temp = hw_prices[week_end>=start_week & week_end<=end_week, ]
  setkey(retailer_trips, retailer_code)
  setkey(hw_prices_temp, retailer_code)
  hw_prices_temp = hw_prices_temp[retailer_trips, nomatch=0L]
  hw_prices_temp[, ntrips:=ntrips/100000]
  
  # Create the data set for shoppers who don't shop at warehouse - 
  # create the probability weighted by consumer trips
  hw_prices_no_wh = hw_prices_temp[channel_type!="Warehouse Club"]
  hw_prob_no_wh = hw_prices_no_wh[, .(nobs = sum(ntrips)), by = c("series", "price_int")]
  hw_prob_no_wh[, prob:=nobs/sum(nobs), by = c("series")]
  setkey(hw_prob_no_wh, series, price_int)
  hw_prob_no_wh[, `:=`(week_end = as.Date(wk), warehouse = 0)]
  
  # Compute the probability of observing different prices for warehouse shoppers.
  hw_prob_wh = hw_prices_temp[, .(nobs = sum(ntrips)), by = c("series", "price_int")]
  hw_prob_wh[, prob:=nobs/sum(nobs), by = c("series")]
  setkey(hw_prob_wh, series, price_int)
  hw_prob_wh[, `:=`(week_end = as.Date(wk), warehouse = 1)]
  
  hw_prob = rbindlist(list(hw_prob, hw_prob_no_wh, hw_prob_wh))
  cat("Week", wk, "Finished!\n")
}
setkeyv(hw_prob, "week_end")
hw_prob[, tstate:=.GRP, by = "week_end"]
setkeyv(hw_prob, c("warehouse", "week_end", "series", "price_int"))
save(hw_prob, file = paste(output_dir, "/HW-Prices-State.RData", sep=""))
write.csv(hw_prob, file="HW-Prices-State.csv", row.names = FALSE)
# ----------------------------------------------------------------------------------------------------#

# Construct consumer panel of hardware purchase
# For each week, the consumer may either visit a store that carries the machine or not.
# If the consumer visits a store, the consumer may either decide to adopt the machine 
# or not to adopt the machine. 
# If the consumer doesn't visit a store, the consumer has decided to postpone the purchases

# Select household trips in hw price panel
hw_retailers = hw_prices[, unique(retailer_code)]
hw_trips = trips[retailer_code %in% hw_retailers, ]
hw_trips = hw_trips[week_end>=as.Date("2009-07-11"), ]

# Select the week_end for the panel year
setkey(hw_trips, household_code, panel_year)
setkey(hh, household_code, panel_year)
hw_trips = hw_trips[hh[,.(household_code, panel_year, dma_code, dma_descr)], nomatch=0L]
hw_trips = hw_trips[dma_code %in% top_dma_list, ]

# Merge in adoption date -- if ever adopted the platform
setkey(hw_trips, household_code)
setkey(hh_list, household_code)
hw_trips = hw_trips[hh_list[,.(household_code, hware, hseries, hfirst_date, hlast_date,
                               sfirst_date, slast_date, k_first_date)], nomatch=0L]

# Get rid of trips beyond adoption date
hw_trips = hw_trips[purchase_date<=hfirst_date | is.na(k_first_date) | 
                      (is.na(hfirst_date) & purchase_date<k_first_date), ]

# Next step is to merge in the prices and availability of series in the trip.
setkeyv(hw_prices, c("retailer_code", "week_end"))
setkeyv(hw_trips, c("retailer_code", "week_end"))
hw_trips = hw_trips[hw_prices[, .(retailer_code, week_end, series, price, price_regular, 
                                  in_rms, price_int, channel_type)], 
         nomatch=0L, allow.cartesian=TRUE]

# For those purchased the machine, merge in adoption price, and coupons
setkey(purchases, trip_code_uc) 
setkey(hw_trips, trip_code_uc) 
hw_trips = purchases[, .(trip_code_uc, series, price_paid, price)][hw_trips]
setnames(hw_trips,c("series", "price", "i.series", "i.price"), 
         c("selected_series", "selected_price", "series", "price"))

# Clean it up, and apply proportional coupons
hw_trips[, discount_rate:=price_paid/price]
hw_trips[!is.na(selected_price) & selected_series==series, price:=selected_price]
hw_trips[is.na(discount_rate), discount_rate:=1]
hw_trips[, price:=price*discount_rate]
hw_trips[is.na(selected_series), `:=`(selected_series = 0, selected_price = 0, price_paid=0)]
hw_trips[, purchased:=as.integer(selected_series==series)]
hw_trips[, `:=`(hfirst_date=NULL, hlast_date=NULL, sfirst_date=NULL, slast_date=NULL, k_first_date=NULL)]
setkey(hw_trips, week_end)
hw_trips[, tstate:=.GRP, by = "week_end"]
setkey(hw_trips, household_code, week_end, trip_code_uc)

# Add non-purchase to the trip data
hw_trips_nopurch = unique(hw_trips)
hw_trips_nopurch[, `:=`(series=0, price=0, price_regular=0, price_int=0)]
hw_trips = rbindlist(list(hw_trips, hw_trips_nopurch))
setkey(hw_trips, household_code, week_end, trip_code_uc, series)
hw_trips[, purchased_tot:=sum(purchased), by = c("household_code", "week_end", "trip_code_uc")]
hw_trips[purchased_tot==0&series==0, purchased:=1]
hw_trips[, purchased_tot:=NULL]
save(hw_trips, file = paste(output_dir, "/HH-HW-Trips.RData", sep=""))

# ----------------------------------------------------------------------------------------------------#
# Compute the adoption value by consumer and time period!
load(paste(output_dir, "/hh_trip_panel.RData", sep=""))
load(paste(output_dir, "/Retailer_Price_Panel.RData", sep=""))
load(paste(output_dir,"/NewYork.RData", sep=""))
load("Data/Bayes-MCMC/Normal-MCMC-45000.RData")

brands = c("CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GLORIA JEAN'S KEURIG", "GREEN MOUNTAIN KEURIG",
           "MAXWELL HOUSE", "MELITTA", "NEW ENGLAND", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS",
           "STARBUCKS KEURIG", "TULLY'S KEURIG")
xnames = c("0OTHER", "CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GLORIA JEAN'S KEURIG", "GREEN MOUNTAIN KEURIG",
           "MAXWELL HOUSE", "MELITTA", "NEW ENGLAND", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS",
           "STARBUCKS KEURIG", "TULLY'S KEURIG", "keurig", "brand_lag", "shadow_price", "budget")

# Obtain the trip probabilities
hh_panel[, quarter:= paste(format(week_end, "%y/"), 0, 
                           sub( "Q", "", quarters(week_end) ), sep = "")]
hh_trip_prob = hh_panel[, .(ntrips = .N), by = c("household_code", "panel_year", "quarter", "retailer_code")]
hh_trip_prob = hh_trip_prob[!is.na(retailer_code), ]
hh_trip_prob[, tot_trips := sum(ntrips), by = c("household_code", "panel_year", "quarter")]
# Missing Retailer Code -- no visit to any retailer store that generally sells coffee to consumers.
hh_trip_prob[, tprob:=ntrips/tot_trips] 
setkey(hh_trip_prob, household_code, panel_year)
hh_trip_prob = hh[,.(household_code, panel_year, dma_code)][hh_trip_prob, nomatch=0L]
warehouse_retailers = c(9101, 9103, 9104)
hh_trip_prob[, warehouse:=as.integer(retailer_code%in%warehouse_retailers)]
hh_trip_prob[, warehouse:=as.integer(any(warehouse==1)), by = c("household_code", "quarter")]
setkey(hh_trip_prob, household_code, dma_code, quarter, retailer_code)

# Impute the expected per-period adoption value.
retailer_panel = retailer_panel[dma_code %in% top_dma_list, ]
retailer_panel = retailer_panel[week_end>=as.Date("2009-07-11"), ]
retailer_panel = retailer_panel[brand_descr!="0NOTHING", ]
retailer_panel[, quarter:= paste(format(week_end, "%y/"), 0, 
                           sub( "Q", "", quarters(week_end) ), sep = "")]
retailer_panel[keurig==1,  brand_descr := paste(brand_descr, "KEURIG")]
retailer_panel[, `:=`(size1_amount = log(size1_amount))]
# Adapt the panel to create the X matrix
retailer_panel[, a1:=as.integer(!brand_descr%in%brands)]
alist = paste0("a", c(2:(length(brands)+1)))
i = 0
for (a in alist){
  i = i+1
  retailer_panel[, v:=as.integer(brand_descr==brands[i])]
  setnames(retailer_panel, "v", a)
}
retailer_panel[brand_descr=="CTL BR", brand_descr:=paste(brand_descr, retailer_code, sep="_")]
setkey(retailer_panel, dma_code, quarter, retailer_code)

# Compute the probability of making a purchase
# Predict probability of purchase once the consumer adopt Keurig
binomP <- function(form){
  binomreg = glm(form, family = "binomial")
  bprob = predict(binomreg, type = "response") 
  return(bprob)
}
hh_panel[, coffee_trip:=as.integer(trip_code_uc%in%coffee_trip_code_list)]
hh_panel = hh_panel[, .(coffee_trip = sum(coffee_trip, na.rm=TRUE), k_first_week_end = k_first_week_end[1], 
                        brand_lag = brand_lag[1], ptype_lag=ptype_lag[1], inv_type=inv_type[1]),
                    by = c("household_code", "week_end", "quarter")]
hh_panel[, `:=`(coffee_trip = as.integer(coffee_trip>=1), keurig_lag=as.integer(ptype_lag=="KEURIG"))]
hh_panel[is.na(keurig_lag), keurig_lag:=0]
hh_panel[is.na(inv_type), inv_type:=0]
hh_panel[, pprob := binomP(coffee_trip~inv_type*keurig_lag), by = c("household_code")]
hh_panel[as.integer(keurig_lag)==1,  brand_lag := paste(brand_lag, "KEURIG")]
setkey(hh_panel, household_code, week_end)

#Remove not needed datasets
rm(trips)
gc()

# Take the last 200 draws and treat as true effects
preferences = colMeans(bindv[30000:40000, ,], 2)
rownames(preferences) = unique(hh_market_prod[, household_code])
TBudget = preferences[,21]
preferences = preferences[, 1:20]
preferences[,20] = exp(preferences[, 20])

# Compute the adoption value consumer by consumer
xvars = c(paste0("a", 2:18), "keurig", "lag_ind", "incdiff")
hhValFun<-function(i){
  hh_prob_temp = hh_trip_prob[.(i), ]
  setkey(hh_prob_temp, dma_code, quarter, retailer_code)
  hh_retailers_temp = retailer_panel[hh_prob_temp, nomatch=0L]
  hh_retailers_temp[,  incdiff := log(TBudget[as.character(i)]-price)]
  hh_retailers_temp = hh_retailers_temp[!is.na(incdiff),] #Cannot afford those options.
  setkey(hh_retailers_temp, week_end)
  hh_lags = hh_panel[.(i), ]
  setkey(hh_lags, week_end)
  hh_retailers_temp = hh_retailers_temp[hh_lags[,.(week_end, brand_lag, keurig_lag, pprob)], nomatch=0L]
  hh_retailers_temp[, lag_ind:=as.integer(brand_descr==brand_lag & keurig==keurig_lag)]
  hh_retailers_temp[is.na(lag_ind), lag_ind:=0]
  xmat = as.matrix(hh_retailers_temp[, xvars, with=FALSE])
  # Compute the mean utility of product
  hh_retailers_temp[, u := xmat %*% preferences[as.character(i),] + size1_amount]
  hh_r_temp = hh_retailers_temp[, .(mu=max(u)), by = c("household_code", "week_end", 
                                                       "retailer_code", "brand_descr", "keurig", "tprob", "pprob")]
  hh_r_temp = hh_r_temp[, .(mu_all = log(sum(exp(mu))),
                            mu_ground = log(sum(exp(mu) * (1-keurig)))), 
                        by = c("household_code", "week_end", "retailer_code", "tprob", "pprob")]
  #If non-purchase receive the utility from budget
  hh_r_temp[is.infinite(mu_ground), mu_ground:= preferences[as.character(i), 20] * log(TBudget[as.character(i)])]
  hh_r_temp = hh_r_temp[, `:=`(mu_diff = mu_all - mu_ground, 
                               mu_val = log(exp(mu_all)-exp(mu_ground))/preferences[as.character(i), 20])]
  hh_r_temp[is.infinite(mu_val), mu_val:= 0] # If non-purchase receive the utility from budget
  hh_r_temp = hh_r_temp[, .(mu_all = sum(mu_all * tprob)/sum(tprob),
                            mu_ground = sum(mu_ground * tprob)/sum(tprob),
                            mu_diff = sum(mu_diff * tprob)/sum(tprob),
                            mu_val = sum(mu_val * tprob)/sum(tprob)), 
                        by = c("household_code", "week_end", "pprob")]
  CVal <- function(weekw, mu_val){
    hh_val_temp = hh_retailers_temp[.(weekw), ]
    implicitT<-function(x){
      hh_val_temp[,  incdiff := log(x-price)]
      hh_val_temp = hh_val_temp[!is.na(incdiff),] #Cannot afford those options.
      xmat = as.matrix(hh_val_temp[, xvars, with=FALSE])
      # Compute the mean utility of adoption
      hh_val_temp[, u := xmat %*% preferences[as.character(i),] + size1_amount]
      hh_val_temp = hh_val_temp[, .(mu=max(u)), by = c("household_code", "week_end", "retailer_code", 
                                                       "brand_descr", "keurig", "tprob", "pprob")]
      hh_val_temp = hh_val_temp[, .(mu_x = log(sum(exp(mu) * (1-keurig)))), 
                                by = c("household_code", "week_end", "retailer_code", "tprob", "pprob")]
      # If non-purchase receive the utility from budget
      hh_val_temp[is.infinite(mu_x), mu_x:= preferences[as.character(i), 20] * log(x)] 
      hh_val_temp = hh_val_temp[, .(mu_x = sum(mu_x * tprob)/sum(tprob))]
      return(mu_val - hh_val_temp$mu_x)
    }
    xstart = TBudget[as.character(i)]
    return(nleqslv(xstart, implicitT, jacobian=TRUE,control=list(btol=.01))$x - TBudget[as.character(i)])
  }
  hh_r_temp[, `:=`(mu_ev = CVal(week_end, mu_all)), by = c("household_code", "week_end")]
  #hh_r_temp[, `:=`(mu_ev = 0 )]
  hh_r_temp[, `:=`(mu_all = mu_all * pprob, mu_ground = mu_ground * pprob,
                   mu_ev = mu_ev * pprob, mu_diff = mu_diff * pprob, 
                   mu_val = mu_val*pprob), by = c("household_code", "week_end")]
  return(hh_r_temp)
}
hh_codes = unique(hh_market_prod$household_code)
clusterExport(cl, c('hh_trip_prob', 'retailer_panel', 'preferences', 
                    'TBudget', 'hh_panel', 'xvars', 'hhValFun'))
cval_list = parLapply(cl, hh_codes, hhValFun)
cval_list = rbindlist(cval_list)

# Merge consumption value back to the value function
setkey(cval_list, household_code, week_end)
setkey(hw_trips, household_code, week_end)
hw_market_trips = hw_trips[cval_list, nomatch=0L]

# Merge in warehouse shopping status
hw_market_trips[, quarter:= paste(format(week_end, "%y/"), 0, 
                                  sub( "Q", "", quarters(week_end) ), sep = "")]
hh_quarter_warehouse = unique(hh_trip_prob[, .(household_code, quarter, warehouse)])
setkey(hw_market_trips, household_code, quarter)
setkey(hh_quarter_warehouse, household_code, quarter)
hw_market_trips = hw_market_trips[hh_quarter_warehouse, nomatch=0L]
save(hw_market_trips, file = paste(output_dir, "/HH-HW-NewYork-Trips.RData", sep=""))

# Export Relevant Data sets to csv format, and use them for julia estimation
hw_market_trips[, `:=`(ntrip =.GRP), by = "trip_code_uc"]
setkey(hw_market_trips, ntrip)
write.csv(hw_market_trips[,.(household_code, warehouse, ntrip, t, series, price, 
                             price_regular, mu_val, purchased)],
          file = "Data/Machine-Adoption/HH-HW-NewYork-Trips.csv", row.names = FALSE)

# Record trip positions to facilitate fast computation.
hw_market_trips[, `:=`(row_i = 1:.N)]
hw_trip_pos = hw_market_trips[, .(start_i = min(row_i), end_i = max(row_i)), by = "ntrip"]
write.table(hw_trip_pos[,.(ntrip, start_i, end_i)],
          file = "Data/Machine-Adoption/HH-HW-NewYork-Pos.csv", row.names=F, col.names=F, sep=",")

# Just simple logit and let's see whether it works. 
hw_market_trips[series==0, mu_val:=0]
hw_market_trips[, `:=`(a1=as.integer(series==1), a2=as.integer(series==2),
                       a3=as.integer(series==3), a4=as.integer(series==4),
                       a5=as.integer(series==5))]
xmat = as.matrix(hw_market_trips[, .(a1, a2, a3, a4, a5, price, mu_val)])
ll<-function(b){
  hw_market_trips[, eu := exp(xmat%*%b)]
  hw_market_trips[, prob := eu/sum(eu), by = c("trip_code_uc")]
  return(-hw_market_trips[, sum(log(prob)*purchased)])
}

opt = optim(b0, ll, gr = NULL, method = c("BFGS"), hessian=TRUE)

# Estimate the parameter governing the evolution process of mu_val
# Both time and individual plays a very small role in determining the next period mu_val.
regout = cval_list[, lm(mu_val~mu_val_lag)]
summary(regout)

# The dataset I want would have
# household_code, time, brand purchase indicator, prices, availability, adoption status, 
# Flow Util of ground coffee (function of preference), Flow Util/W(1) (function of preference), 
# No of Brands, No of products, holidays indicator, cumulative adoption percentage... 

# Deal with projection factor? 
