#---------------------------------------------------------------------------------------------------#
# Load appropriate data
# Load HH file with flags
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))
load(paste(meta_dir, "/HH.RData", sep=""))

# Load HMS Trips Data
load(paste(HMS_trip_dir, "/Trips.RData", sep=""))

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
purchases=purchases[products[, .(upc, upc_ver_uc, upc_descr, ptype, 
                                 brand_descr, size1_amount, multi, series)], nomatch=0L]
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

# Obtain Household Warehouse shopping status
warehouse_retailers = c(9101, 9103, 9104)
trips[, quarter:= paste(format(purchase_date, "%y/"), 0, 
                        sub( "Q", "", quarters(purchase_date) ), sep = "")]
trips[, warehouse:=as.integer(retailer_code %in% warehouse_retailers)]
hh_ware_status = trips[, .(warehouse=as.integer(sum(warehouse==1)>=2)), 
                       by = c("household_code", "panel_year", "quarter")]
setkeyv(hh_ware_status, c("household_code", "panel_year", "quarter"))

# Merge in total spending
setkey(purchases, trip_code_uc)
setkey(trips, trip_code_uc)
purchases = purchases[trips[, .(trip_code_uc, total_spent, warehouse)], nomatch=0L]

# Obtain Household retailer shopping frequency
hh_rn_trips = trips[, .(ntrips = .N), by = c("household_code", "retailer_code", "panel_year")]
setkey(hh_rn_trips, household_code, retailer_code, panel_year)
setkey(purchases, household_code, retailer_code, panel_year)
purchases = purchases[hh_rn_trips, nomatch=0L]

# Check whether the trip is a "special" trip
purchases[, basket_share := (total_price_paid - coupon_value)/total_spent]
pdf(file=paste("Tabfigs/HW-Summary/figs/HW-Basket-Share-By-Retailer-Freq.pdf", sep=""), width=8, height=5)
par(mfrow=c(1,2))
purchases[total_spent<=800 & total_price_paid>=40 & ntrips>=2, 
          hist(basket_share, nclass=50, xlim=c(0, 1.0), ylim=c(1,8), 
               freq=F, xlab="Basket Share", main = "Shopped at Least Twice")]
abline(v=median(purchases[total_spent<=800 & total_price_paid>=40 & ntrips>=2, basket_share]), col="red")
purchases[total_spent<=800 & total_price_paid>=40 & ntrips==1, 
          hist(basket_share, nclass=50, xlim=c(0, 1.0), ylim=c(1,8), 
               freq=F, xlab="Basket Share", main = "Shopped Only Once")] # 10% of the purchases!
abline(v=median(purchases[total_spent<=800 & total_price_paid>=40 & ntrips==1, basket_share]), col="red")
par(mfrow=c(1,1))
dev.off()

# Merge back into trips data to get only the relevant trips.
trips = trips[household_code %in% hh_list[, household_code]]

# Only Trips Beyond 2008 matters 
trips = trips[panel_year>=2008, ]
setkeyv(trips, c("household_code", "purchase_date", "trip_code_uc"))

# Merge in Trip DMA information
setkey(trips, household_code, panel_year)
trips = trips[hh[, .(household_code, panel_year, dma_code)], nomatch=0L]

# Only keep trips in top dma list
trips = trips[dma_code %in% top_dma_list, ]
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
hms_prices = purchases[, .(price = median(price_paid)), by = c("retailer_code", "week_end", "series")]
setkeyv(hms_prices, c("retailer_code", "week_end", "series"))
r4401_hms_prices = hms_prices[retailer_code == 4401, ]
r9005_hms_prices = hms_prices[retailer_code == 9005, ]
setkeyv(r4401_hms_prices, c("week_end", "series"))
setkeyv(r9005_hms_prices, c("week_end", "series"))
r4401_hms_prices[r9005_hms_prices, nomatch=0L][series==4, cor(price, i.price)]


# Cross Market within retailer price correlation check
s3_hms_prices = hms_prices[retailer_code==6901 & series==3, ]
s4_hms_prices = hms_prices[retailer_code==6901 & series==4, ]
setkeyv(s3_hms_prices, c("week_end"))
setkeyv(s4_hms_prices, c("week_end"))
s3_hms_prices[s4_hms_prices, nomatch=0L][, cor(price, i.price)]

# Cross retailer price correlation check - within a market
hms_prices = purchases[, .(price = median(price_paid)), by = c("dma_code", "retailer_code", "week_end", "series")]
setkeyv(hms_prices, c("dma_code", "retailer_code", "week_end", "series"))
r4401_hms_prices = hms_prices[retailer_code == 4401 & dma_code==501, ]
r9005_hms_prices = hms_prices[retailer_code == 9005 & dma_code==501, ]
setkeyv(r4401_hms_prices, c("week_end", "series"))
setkeyv(r9005_hms_prices, c("week_end", "series"))
r4401_hms_prices[r9005_hms_prices, nomatch=0L][series==4, cor(price, i.price)]

# Cross Market within retailer price correlation check
chicago_hms_prices = hms_prices[dma_code == 602 & retailer_code==9005, ]
newyork_hms_prices = hms_prices[dma_code == 501 & retailer_code==9005, ]
setkeyv(chicago_hms_prices, c("week_end", "series"))
setkeyv(newyork_hms_prices, c("week_end", "series"))
chicago_hms_prices[newyork_hms_prices, nomatch=0L][series==4, cor(price, i.price)]
hms_prices = purchases[, .(price = median(price_paid)), by = c("retailer_code", "week_end", "series")]
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
series_sales = prod_sales[, .(np = sum(np), revenue = sum(revenue)), by = series]

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
store_exists_panel = store_exists_panel[stores[, .(store_code_uc, panel_year, dma_code, retailer_code)], nomatch=0L]
setkey(store_exists_panel, dma_code, retailer_code, store_code_uc, week_end)

# Filter out outliers
move[, pmedian:=median(price), by = c("upc", "upc_ver_uc")]
move = move[price<=2*pmedian & price>=(1/3 * pmedian), ]

# Obtain the average and median price by retailer.
rms_hw_prices = move[, .(price_avg=mean(price), price_mid = median(price), price_sd = sd(price), 
                         revenue = sum(units * price), units = sum(units), nstores = length(unique(store_code_uc))), 
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

# Check cross series price correlation
s3_rms_prices = rms_hw_prices[retailer_code==6901 & series==3, ]
s4_rms_prices = rms_hw_prices[retailer_code==6901 & series==4, ]
setkeyv(s3_rms_prices, c("week_end"))
setkeyv(s4_rms_prices, c("week_end"))
s3_rms_prices[s4_rms_prices, nomatch=0L][, cor(price_mid, (price_mid+i.price_mid))]
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

# The imputation is done by looking at the price series. Given scarcity of data, this is the best that I can do. 
# Imputation prices retailer by retailer
hms_hw_prices[, price_regular := as.numeric(NA)]
# Retailer 9005
hms_hw_prices[retailer_code==9005&series==1&week_end<="2009-08-22", price_regular:=99.99]
hms_hw_prices[retailer_code==9005&series==1&week_end>="2009-08-29", price_regular:=119.99]
hms_hw_prices[retailer_code==9005&series==2&week_end<="2011-08-27", price_regular:=114.99]
hms_hw_prices[retailer_code==9005&series==2&week_end>="2011-09-03", price_regular:=124.99]
hms_hw_prices[retailer_code==9005&series==3&week_end<="2011-02-19", price_regular:=129.99]
hms_hw_prices[retailer_code==9005&series==3&week_end>="2011-02-26"&week_end<="2011-09-03", price_regular:=139.99]
hms_hw_prices[retailer_code==9005&series==3&week_end>="2011-09-10", price_regular:=149.99]
hms_hw_prices[retailer_code==9005&series==4&week_end<="2011-02-19", price_regular:=169.99]
hms_hw_prices[retailer_code==9005&series==4&week_end>="2011-02-26"&week_end<="2011-10-01", price_regular:=179.99]
hms_hw_prices[retailer_code==9005&series==4&week_end>="2011-10-08", price_regular:=189.99]
hms_hw_prices[retailer_code==9005&series==5&week_end<="2011-02-19", price_regular:=209.99]
hms_hw_prices[retailer_code==9005&series==5&week_end>="2011-02-26"&week_end<="2011-10-01", price_regular:=219.99]
hms_hw_prices[retailer_code==9005&series==5&week_end>="2011-10-08", price_regular:=239.99]
# Retailer 4401
hms_hw_prices[retailer_code==4401&series==1&week_end<="2009-06-06", price_regular:=79.99]
hms_hw_prices[retailer_code==4401&series==1&week_end>="2009-06-13"&week_end<="2010-02-20", price_regular:=89.99]
hms_hw_prices[retailer_code==4401&series==1&week_end>="2010-02-27"&week_end<="2010-03-06", price_regular:=69.98]
hms_hw_prices[retailer_code==4401&series==1&week_end>="2010-03-13"&week_end<="2010-03-27", price_regular:=55.99]
hms_hw_prices[retailer_code==4401&series==1&week_end>="2010-04-03", price_regular:=49.99]
hms_hw_prices[retailer_code==4401&series==2, price_regular:=99.99]
hms_hw_prices[retailer_code==4401&series==3&week_end<="2011-02-26", price_regular:=109.99]
hms_hw_prices[retailer_code==4401&series==3&week_end>="2011-03-05", price_regular:=119.99]
hms_hw_prices[retailer_code==4401&series==4&week_end<="2009-03-14", price_regular:=149.99]
hms_hw_prices[retailer_code==4401&series==4&week_end>="2009-03-21"&week_end<="2010-02-20", price_regular:=129.99]
hms_hw_prices[retailer_code==4401&series==4&week_end>="2010-02-27"&week_end<="2011-02-19", price_regular:=139.99]
hms_hw_prices[retailer_code==4401&series==4&week_end>="2011-02-26", price_regular:=149.99]
hms_hw_prices[retailer_code==4401&series==5&week_end<="2011-02-19", price_regular:=169.99]
hms_hw_prices[retailer_code==4401&series==5&week_end>="2011-02-26", price_regular:=179.99]
# Retailer 6920
hms_hw_prices[retailer_code==6920&series==1&week_end<="2011-06-11", price_regular:=89.95]
hms_hw_prices[retailer_code==6920&series==1&week_end>="2011-06-18", price_regular:=99.99]
hms_hw_prices[retailer_code==6920&series==2, price_regular:=99.99]
hms_hw_prices[retailer_code==6920&series==3&week_end<="2010-02-20", price_regular:=99.96]
hms_hw_prices[retailer_code==6920&series==3&week_end>="2010-02-27"&week_end<="2011-01-22", price_regular:=109.96]
hms_hw_prices[retailer_code==6920&series==3&week_end>="2011-01-29"&week_end<="2012-11-03", price_regular:=119.95]
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
hms_hw_prices[retailer_code==9103&series==4&week_end>="2009-02-28"&week_end<="2009-08-15", price_regular:=119.99]
hms_hw_prices[retailer_code==9103&series==5, price_regular:=149.99]
# Retailer 9101
hms_hw_prices[retailer_code==9101&series==1, price_regular:=69.99]
hms_hw_prices[retailer_code==9101&series==2, price_regular:=79.98]
hms_hw_prices[retailer_code==9101&series==4&week_end<="2011-02-19", price_regular:=129.98]
hms_hw_prices[retailer_code==9101&series==4&week_end>="2011-02-26"&week_end<="2011-07-23", price_regular:=139.98]
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
hms_hw_prices[price>=price_regular, price := price_regular] # Likely measurement error, and only 44 observations.
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

# Check price -- whether my hardware price imputation is good
setkey(hw_prices, retailer_code, series, week_end)
setkey(purchases, retailer_code, series, week_end)
pcheck = purchases[hw_prices[,.(retailer_code, series, week_end, price, price_regular)], nomatch=0L]
pcheck[series==4, cor(price, i.price)]
# ----------------------------------------------------------------------------------------------------#

# Compute the probability of observing different price support for households given their trip data.
# Group prices into 5 dollar increments from 34.99 dollars to 259.99 dollars. 
hw_prices[, price_int:=as.numeric(NA)]
pseq = seq(34.99, 259.99, 5)
for (pval in pseq){
  hw_prices[abs(price-pval)<=2.5, price_int := pval]
}
hw_prices = hw_prices[week_end>="2009-07-11", ]

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

# Look at whether demographics and other characteristics predict adoption and series adopted
# Not very predictive...
setnames(hh_list, "adoption_panel_year", "panel_year")
setkey(hh_list, household_code, panel_year)
setkey(hh, household_code, panel_year)
setkey(hh_ware_status, household_code, panel_year)

hh_demo_series = hh_list[hseries!=0,.(household_code, panel_year, hseries, brate, bexpenditure)][
  hh[,.(household_code, panel_year, inc40, inc50, inc60, 
        inc70, hhsize2, hhsize3, hhsize5,
        onefamily, twofamily, fulltime, partime, head_age_numeric,
        presence_of_children, african_american, hispanic, cable, internet)], 
  nomatch=0L]
hh_demo_series = hh_demo_series[hh_ware_status, nomatch=0L]
hh_demo_series[, household_code:=NULL]
NAto0 <- function(x) ifelse(is.na(x), 0, x)
hh_demo_series = hh_demo_series[, lapply(.SD, NAto0)]
multireg = multinom(hseries ~ ., data = hh_demo_series, maxit = 300)
multiregs = stepAIC(multireg)
hh_demo_series[, pseries := predict(multireg)]
hh_demo_series[, table(pseries==hseries)] ## Not very predictive...
setnames(hh_list, "panel_year", "adoption_panel_year")
# ----------------------------------------------------------------------------------------------------#
# Construct Hardware price index within retailer - take the average for series 3 and 4, and average of 3, 4, 5 in warehouses.
# Obtain the series weight
# For creating price indexes later!
setkey(hw_prices, series)
setkey(series_sales, series)
hw_p_retailer = hw_prices[series_sales, nomatch=0L]
hw_p_retailer = hw_p_retailer[series==3 | series==4 | series==5, ]
hw_p_retailer[series==4, `:=`(price = price-30)]
hw_p_retailer[series==5, `:=`(price = price-60)]
hw_p_retailer[series==4, `:=`(price_regular = price_regular-30)]
hw_p_retailer[series==5, `:=`(price_regular = price_regular-60)]
hw_p_retailer = hw_p_retailer[, .(price = (price * revenue)/sum(revenue),
                                  price_regular = (price_regular * revenue)/sum(revenue)), 
                              by = c("retailer_code", "week_end")]

# -----------------------------------------------------------------------------------------------#
# For price checking later.
purchases[, warehouse := NULL]
purchases[, quarter:= paste(format(purchase_date, "%y/"), 0, 
                            sub( "Q", "", quarters(purchase_date) ), sep = "")]
setkey(purchases, household_code, panel_year, quarter)
setkey(hh_ware_status, household_code, panel_year, quarter)
purchases = purchases[hh_ware_status, nomatch=0L]

# Average purchases 
retailer_share = purchases[series==3|series==4|series==5, .(np = .N), 
                           by = c("retailer_code", "panel_year")]
hh_retailer_trips=trips[, .(ntrips = .N), 
                        by = c("household_code", "dma_code", 
                               "retailer_code", "panel_year")]

# create new weights
setkey(retailer_share, retailer_code, panel_year)
setkey(hh_retailer_trips, retailer_code, panel_year)
retailer_share = hh_retailer_trips[retailer_share, nomatch = 0L]
retailer_share[, np:=ifelse(np<=5, 0, np)]
retailer_share[, nweight := (np*ntrips)/10000]

# Merge in price
hw_prices[, `:=`(panel_year = year(week_end),
                 quarter = paste(format(week_end, "%y/"), 0, 
                                 sub( "Q", "", quarters(week_end)), sep = ""))]
setkey(retailer_share, retailer_code, panel_year)
setkey(hw_prices, retailer_code, panel_year)
hw_p_index = hw_prices[retailer_share, nomatch=0L, allow.cartesian=TRUE]
hw_p_index = hw_p_index[series==3 | series==4 | series==5, ]
hw_prices[series==3, `:=`(price = price + 30)]
hw_prices[series==5, `:=`(price = price - 30)]
hw_p_index0 = hw_p_index[!(retailer_code %in% warehouse_retailers),
                         .(price=sum(price*nweight)/sum(nweight),
                           price_regular=sum(price_regular*nweight)/sum(nweight),
                           warehouse=0), 
                         by = c("household_code", "series", "dma_code", "week_end")]
hw_p_index1 = hw_p_index[,.(price=sum(price*nweight)/sum(nweight),
                           price_regular=sum(price_regular*nweight)/sum(nweight),
                           warehouse=1), 
                         by = c("household_code", "series", "dma_code", "week_end")]
hw_p_index = rbindlist(list(hw_p_index0, hw_p_index1))
rm(hw_p_index0, hw_p_index1)

# Obtain the series weight
setkey(series_sales, series)
setkey(hw_p_index, series)
hw_p_index = hw_p_index[series_sales, nomatch=0L]

# Obtain the overall price environment
hw_p_index = hw_p_index[, .(price = sum(price*revenue)/sum(revenue), 
                            price_regular = sum(price_regular*revenue)/sum(revenue)), 
                        by = c("household_code", "dma_code", "warehouse", "week_end")]
setkey(hw_p_index, household_code, dma_code, warehouse, week_end)

# Drop Missing Observations
hw_p_index = hw_p_index[!is.na(price), ]

# Select household base on actual status 
hw_p_index[, quarter:= paste(format(week_end, "%y/"), 0, 
                             sub( "Q", "", quarters(week_end) ), sep = "")]
setkey(hw_p_index, household_code, quarter, warehouse)
setkey(hh_ware_status, household_code, quarter, warehouse)
hw_p_index = hw_p_index[hh_ware_status, nomatch=0L]

# Check how good the price variation is in the real data!
setkey(hw_p_index, household_code, dma_code, warehouse, week_end)
setkey(purchases, household_code, dma_code, warehouse, week_end)
price_check = purchases[hw_p_index[, .(household_code, dma_code, warehouse, week_end, price)], nomatch=0L]
price_check[series==3, cor(price, i.price, use = "pairwise.complete.obs")]
price_check[series==4, cor(price, i.price, use = "pairwise.complete.obs")]
price_check[series==5, cor(price, i.price, use = "pairwise.complete.obs")]

# Create price lags
hw_p_index[,`:=`(price_lag = c(NA, price[1:(length(price)-1)]), 
                 price_reg_lag = c(NA, price_regular[1:(length(price_regular)-1)])), 
           by = c("household_code")]

# Update based on best fit to data
fitw <- function(w){
  pr = exp(w)/(1+exp(w))
  hw_p_index[, price_avg:=mavg(price, pr), by = c("household_code")]
  preg = hw_p_index[, lm(price~price_avg)]
  return(sum(residuals(preg)^2))
}

popt = optim(0, fitw, method = c("Brent"), lower = -10, upper = 10, hessian=TRUE)
w = popt$par
pr = exp(w)/(1+exp(w))
hw_p_index[, price_avg:=mavg(price, pr), by = c("household_code")]
hw_p_index[, price_avgn:=mavgn(price, pr), by = c("household_code")]
preg = hw_p_index[, lm(price~price_avg)]

# Regress price on price moving averages.
summary(preg)
setkey(hw_p_index, household_code, dma_code, week_end)
save(hw_p_index, file = paste(output_dir, "/HW-P-Index.RData", sep=""))
write.csv(hw_p_index, file="HW-P-Index.csv", row.names = FALSE)

# ----------------------------------------------------------------------------------------------------#
# Construct consumer panel of hardware purchase
# For each week, the consumer aware of the price environment decide whether to purchase a Keurig machine. 

# Make the initial panel
hw_panel = data.table(expand.grid(household_code = unique(hh_list$household_code),
                                  week_end = unique(hw_prices$week_end)))

setkey(hw_panel, household_code)
setkey(hh_list, household_code)
hw_panel = hw_panel[hh_list[hware==1 | himputed==1 | kholder==0,
                            .(household_code, hware, hseries, hfirst_date, hlast_date,
                              sfirst_date, slast_date, k_first_date)], nomatch=0L]

# filter trips adoption date
setkey(hw_panel, household_code, week_end)
hw_panel[, wfilter := as.integer(week_end<=(hfirst_date+6) | is.na(k_first_date) | 
                                   (is.na(hfirst_date) & week_end<=(k_first_date+6)))]

# Get the dma code of the household
hw_panel[, panel_year := year(week_end)]
setkey(hw_panel, household_code, panel_year)
hw_panel = hw_panel[hh[, .(household_code, panel_year, dma_code)], nomatch=0L]
hw_panel[, quarter:= paste(format(week_end, "%y/"), 0, 
                           sub( "Q", "", quarters(week_end) ), sep = "")]

# Next step is to merge in the prices
setkeyv(hw_panel, c("household_code", "dma_code", "week_end",  "panel_year", "quarter"))
setkeyv(hw_p_index, c("household_code", "dma_code", "week_end", "panel_year", "quarter"))
hw_panel = hw_panel[hw_p_index, nomatch=0L]

# Flag Adoption date
setkey(hw_panel, week_end)
hw_panel[, t:=.GRP, by = "week_end"]
hw_panel[, purchased := as.integer(t==max(t)), by = c("household_code")]
hw_panel[is.na(k_first_date), purchased:=0]

# Setkey and save the data
setkey(hw_panel, household_code, week_end)
save(hw_panel, file = paste(output_dir, "/HH-HW-Panel.RData", sep=""))

# ----------------------------------------------------------------------------------------------------#
# Compute the adoption value by consumer and time period!
big_markets = c(501, 602, 803, 504, 539, 506, 524, 613, 623, 753) # Big Markets
load(paste(output_dir, "/hh_trip_panel.RData", sep=""))
load(paste(output_dir, "/Retailer_Price_Panel.RData", sep=""))
load(paste(output_dir,"/Coffee-Panel-Cond-Purchase.RData", sep=""))

brands = c("CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GLORIA JEAN'S KEURIG", "GREEN MOUNTAIN KEURIG",
           "MAXWELL HOUSE", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")
xnames = c("0OTHER", "CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GLORIA JEAN'S KEURIG", "GREEN MOUNTAIN KEURIG",
           "MAXWELL HOUSE", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS",
           "STARBUCKS KEURIG", "TULLY'S KEURIG", "keurig", "brand_lag_ground", "brand_lag_keurig", "price_coef", "budget")

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

# Take the last one thousand draws or so and treat as true effects
indx = seq(4000, 8000, 5)
preferences = NULL
TBudget = NULL
for (m_code in big_markets){
  mcmc_file = paste("Data/Bayes-MCMC/Normal-MCMC-", m_code, ".RData", sep ="")
  load(mcmc_file)
  pref_temp = colMeans(bindv[indx, ,], 2)
  if (length(setdiff(alist, bnames))!=0){
    m_list = setdiff(alist, bnames)
    for (i in 1:length(m_list)) {
      numk = as.integer(substr(m_list[i], 2, nchar(m_list[i])))-2
      pref_temp = cbind(pref_temp[, 1:numk], rep(0,nrow(pref_temp)), 
                        pref_temp[, (numk+1):ncol(pref_temp)])
    }
  }
  rownames(pref_temp) = unique(hh_market_prod[.(hh_index_list), household_code])
  pref_temp[,19] = exp(pref_temp[, 19])
  TBudget_temp = pref_temp[,20]
  pref_temp = pref_temp[, 1:19]
  preferences = rbind(preferences, pref_temp)
  TBudget = c(TBudget, TBudget_temp)
}
rm(hh_market_prod)
gc()

# Compute the adoption value consumer by consumer
xvars = c(paste0("a", 2:16), "keurig", "lag_ground", "lag_keurig", "incdiff")
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
  hh_retailers_temp[, `:=`(lag_ground=as.integer(brand_descr==brand_lag & keurig==keurig_lag)*(1-keurig),
                           lag_keurig=as.integer(brand_descr==brand_lag & keurig==keurig_lag)*keurig)]
  hh_retailers_temp[is.na(lag_ground), lag_ground:=0]
  hh_retailers_temp[is.na(lag_keurig), lag_keurig:=0]
  xmat = as.matrix(hh_retailers_temp[, xvars, with=FALSE])
  # Compute the mean utility of product
  hh_retailers_temp[, u := xmat %*% preferences[as.character(i),] + size1_amount]
  hh_r_temp = hh_retailers_temp[, .(mu=max(u)), by = c("household_code", "week_end", 
                                                       "retailer_code", "brand_descr", "keurig", "tprob", "pprob")]
  hh_r_temp[, mu_mean:=mean(mu), by = c("household_code", "week_end", 
                                        "retailer_code", "tprob", "pprob")]
  
  hh_r_temp = hh_r_temp[, .(mu_all = log(sum(exp(mu-mu_mean))) + mu_mean[1],
                            mu_ground = log(sum(exp(mu-mu_mean) * (1-keurig))) + mu_mean[1],
                            mu_mean = mu_mean[1]), 
                        by = c("household_code", "week_end", "retailer_code", "tprob", "pprob")]
  # If non-purchase receive the utility from budget
  hh_r_temp[is.infinite(mu_ground), mu_ground:= preferences[as.character(i), 19] * log(TBudget[as.character(i)])] 
  hh_r_temp[mu_all<mu_ground, mu_all:=mu_ground]
  hh_r_temp = hh_r_temp[, `:=`(mu_diff = mu_all - mu_ground, 
                               mu_val = (log(exp(mu_all-mu_mean)-exp(mu_ground-mu_mean))+mu_mean)/
                                 preferences[as.character(i), 19])]
  hh_r_temp[is.na(mu_val), mu_val := 0] # If produce 0, two are the same
  hh_r_temp[is.infinite(mu_val), mu_val := 0] # If produce 0, two are the same
  hh_r_temp = hh_r_temp[, .(mu_all = sum(mu_all * tprob)/sum(tprob),
                            mu_ground = sum(mu_ground * tprob)/sum(tprob),
                            mu_diff = sum(mu_diff * tprob)/sum(tprob),
                            mu_val = sum(mu_val * tprob)/sum(tprob)), 
                        by = c("household_code", "week_end", "pprob")]

  hh_r_temp[, `:=`(mu_all = mu_all * pprob, mu_ground = mu_ground * pprob,
                   mu_diff = mu_diff * pprob, mu_val = mu_val*pprob), 
            by = c("household_code", "week_end")]
  return(hh_r_temp)
}
hh_codes = as.integer(row.names(preferences))
clusterExport(cl, c('hh_trip_prob', 'retailer_panel', 'preferences', 'TBudget', 'hh_panel', 'xvars', 'hhValFun'))
cval_list = parLapply(cl, hh_codes, hhValFun)
cval_list = rbindlist(cval_list)
save(cval_list, file = paste(output_dir, "/HH-Util-Diff.RData", sep=""))

# Merge consumption value back to the value function
setkey(cval_list, household_code, week_end)
setkey(hw_panel, household_code, week_end)
hw_market_panel = hw_panel[cval_list, nomatch=0L]
hw_market_panel = hw_market_panel[wfilter==1, ]

# Get rid of outliers
hw_market_panel = hw_market_panel[mu_val<=20, ]

# Load market specific information.
load("Data/HMS-Summary/DMA-Panel.RData")
setkey(hw_market_panel, dma_code, week_end)
setkey(dma_panel, dma_code, week_end)
hw_market_panel = hw_market_panel[dma_panel[, .(dma_code, week_end, ashare, nbrand, thanksgiving,
                                                christmas, bchristmas, achristmas, mother, father)], nomatch=0L]
setkey(hw_market_panel, household_code, week_end)
save(hw_market_panel, file = paste(output_dir, "/HH-HW-Panel.RData", sep=""))

# Export Relevant Data sets to csv format, and use them for julia estimation
hw_market_panel[, `:=`(ntrip =.GRP), by = c("household_code", "week_end")]
setkey(hw_market_panel, ntrip)
write.csv(hw_market_panel[,.(household_code, hware, ntrip, t, price, 
                             price_avg, price_avgn, mu_diff, purchased, ashare, thanksgiving, christmas, 
                             bchristmas, achristmas, mother, father, nbrand)],
          file = "Data/Machine-Adoption/HH-HW-Panel.csv", row.names = FALSE)

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
hw_market_panel[, mu_diff_lag := c(NA, mu_diff[1:(length(mu_diff)-1)]), by = "household_code"]
hw_market_panel[, mu_diff_lag_2 := c(NA, mu_diff_lag[1:(length(mu_diff_lag)-1)]), by = "household_code"]
hw_market_panel[, nobs := .N, by = "household_code"]
# Get at the Markove assumption]
pval_lag1 <- function(form){
  regx = lm(form)
  return(anova(regx)$'Pr(>F)'[1])
}
pval_lag2 <- function(form){
  regx = lm(form)
  return(anova(regx)$'Pr(>F)'[2])
}
gamma1fun <- function(form){
  regx = lm(form)
  return(coef(regx)[1])
}
gamma2fun <- function(form){
  regx = lm(form)
  return(coef(regx)[2])
}

sigfun <- function(form){
  regx = lm(form)
  return(summary(regx)$sigma)
}

hw_markov_check = hw_market_panel[nobs>=10, .(p_lag1 = pval_lag1(mu_diff~mu_diff_lag+mu_diff_lag_2),
                                              p_lag2 = pval_lag2(mu_diff~mu_diff_lag+mu_diff_lag_2),
                                              gamma1 = gamma1fun(mu_diff~mu_diff_lag),
                                              gamma2 = gamma2fun(mu_diff~mu_diff_lag),
                                              sig = sigfun(mu_diff~mu_diff_lag),
                                              purchased = sum(purchased)), 
                                  by = "household_code"]
regout = hw_market_panel[, lm(mu_diff~mu_diff_lag)]
summary(regout)

# The dataset I want would have
# household_code, time, brand purchase indicator, prices, availability, adoption status, 
# Flow Util of ground coffee (function of preference), Flow Util/W(1) (function of preference), 
# No of Brands, No of products, holidays indicator, cumulative adoption percentage... 

# Deal with projection factor? 