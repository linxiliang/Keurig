#---------------------------------------------------------------------------------------------------#
# 
# This file imputes the price and availability panel at different retailers
# 
#---------------------------------------------------------------------------------------------------#
# Load appropriate data
# Load HH file with flags
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))
load(paste(meta_dir, "/HH.RData", sep=""))

# Load HMS Purchase data
load(paste(HMS_input_dir, "/HMS-Purchases-Coffee.RData", sep=""))

# Load Product information
load(paste(meta_dir, "/Products.RData", sep=""))

# Load Store information
load(paste(meta_dir, "/Stores.RData", sep=""))

# Load retailer channel information
load(paste(meta_dir, "/Retailers.RData", sep=""))
retailers[, channel_type:=as.character(channel_type)]
# Group department store and home furnishing as one channel.
retailers[channel_type == "Home Furnishings" | channel_type == "Department Store", 
          channel_type := "Specialty Channel"]

# Products multi pack adjustment
products[, size1_amount := size1_amount * multi]

# Make purchases trip, product unique
pnamelist = names(purchases)
pnamelist = setdiff(pnamelist, c("total_spent", "total_price_paid", "coupon_value", 
                                 "deal_flag_uc","quantity"))
purchases[upc=="009955504002" & as.integer(quantity) == 12, quantity:=1] # Fix data errors
purchases = purchases[, .(total_price_paid = sum(total_price_paid),
                          coupon_value = sum(coupon_value),
                          quantity = sum(quantity),
                          total_spent = total_spent[1]), by = pnamelist]

# Restrict purchases to specific software purchases
purchases = purchases[product_module_code %in% focal_module, ]

# Merge in the type information to the purchase data
setkeyv(purchases, c("upc", "upc_ver_uc"))
purchases=purchases[products[, .(upc, upc_ver_uc, ptype, brand_descr, size1_amount, multi, ptype, roast, flavored, 
                                 kona, colombian, sumatra, wb, lightR, mediumR, medDR, darkR)], nomatch=0L]
purchases[, month:=as.factor(format(purchase_date, '%Y-%m'))]

# Merge purchases with household dma information to locate stores
setkeyv(purchases, c("household_code", "panel_year"))
setkeyv(hh, c("household_code", "panel_year"))
purchases = purchases[hh[, .(household_code, panel_year, dma_code, dma_descr, region_code,
                             fips_county_code, fips_county_descr, panelist_zip_code)],
                      nomatch=0L]

# Only keep households in the top N DMAs in terms of adopters
# Households double count if moved. 
dma_rank_list = unique(hh_info_panel[as.integer(kholder)==1, .(household_code, dma_code)])
dma_rank_list = dma_rank_list[, .(nhh = .N), by = "dma_code"]
dma_rank_list = dma_rank_list[order(-nhh), ]
dma_rank_list[, cumshare := cumsum(nhh/sum(nhh))]
top_dma_list = dma_rank_list[1:30, dma_code]

# See the market share in ground coffee
dma_ground_list = unique(hh_info_panel[, .(household_code, dma_code)])
dma_ground_list = dma_ground_list[, .(nhh = .N), by = "dma_code"]
dma_ground_list = dma_ground_list[order(-nhh), ]
dma_ground_list[, share := nhh/sum(nhh)]
dma_ground_list[dma_code%in%top_dma_list, sum(share)]

# Now, keep only the households in those market. 
hh_info_panel = hh_info_panel[dma_code%in%top_dma_list, ]
hh_code_list = unique(hh_info_panel[, .(household_code)])
setkey(hh_code_list, household_code)
setkey(hh_list, household_code)
hh_list = hh_list[hh_code_list, nomatch=0L]

# Flag purchases from chosen household
setkey(purchases, household_code)
purchases = hh_list[,.(household_code, kholder)][purchases]
purchases[, selected := as.integer(!is.na(kholder))]
purchases[, kholder:=NULL]

# Merge in retailer information
setkey(purchases, retailer_code)
purchases = purchases[retailers, nomatch=0L]

# Obtain the list of retailers (retailers where the household shop for coffee).
setkey(purchases, NULL)
hh_retailer_list = unique(purchases[as.integer(selected)==1, .(retailer_code)])

# Obtain the list of online retailers -- they have nation wide pricing
online_retailers = retailers[channel_type=="Online Shopping", retailer_code]

# Change the purchases online retailer code to the online retailer code
purchases[retailer_code %in% online_retailers, retailer_code:=4849]

# Threshold for number of purchases within a DMA to count it as being present
np_threshold = 100
np_dma_retailer = purchases[, .(np = .N), by = c("dma_code", "retailer_code")]
setkeyv(np_dma_retailer, c("dma_code", "retailer_code"))
#---------------------------------------------------------------------------------------------------#
# First obtain the share of sales by brands and type - basis of brand to be included in estimation.
purchases[, keurig:=as.integer(ptype=="KEURIG")]
brand_type_sales = purchases[as.integer(selected)==1, .(brand_sales = sum(total_price_paid-coupon_value)), 
                             by = c("brand_descr", "keurig")]
brand_type_sales[, total_sales := sum(brand_sales), by = "keurig"]
brand_type_sales[, brand_share := brand_sales/total_sales]
brand_type_sales = brand_type_sales[order(-brand_share), ]
brand_type_sales[, cumshare := cumsum(brand_share), by = "keurig"]
brand_type_sales[, presence := .N, by = "brand_descr"]

# Obtain list of brands of interest
brand_type_sales[, cumshare := cumshare - brand_share]
selected_brand_list = unique(brand_type_sales[(as.integer(keurig)==1 & cumshare<=0.95) | 
                                                (as.integer(keurig)==0 & cumshare<=0.90), brand_descr]) 
top_keurig_brands = brand_type_sales[as.integer(keurig)==1 & brand_descr!="CTL BR" & brand_share>=0.05, brand_descr]
top_selling_brands = unique(brand_type_sales[brand_share>=0.05, brand_descr])
#---------------------------------------------------------------------------------------------------#
# Check price correlation across retailers of the same channel.
retailer_prices = purchases[as.integer(selected)==1, .(revenue = sum(total_price_paid - coupon_value),
                                                       quantity = sum(quantity)), 
                            by = c("retailer_code", "brand_descr", "size1_amount", "month")]
retailer_prices[, price:=revenue/quantity]
setkey(retailer_prices, brand_descr, size1_amount, month)

# Check correlation between 9005 and 9006 - both are department stores selling K-Cups
retailer_9005 = retailer_prices[as.integer(retailer_code)==9005, ]
retailer_9006 = retailer_prices[as.integer(retailer_code)==9006, ]
retailer_9099 = retailer_prices[as.integer(retailer_code)==9099, ]
retailer_9005_9006 = retailer_9005[retailer_9006, nomatch=0L]
retailer_9005_9006_9099 = retailer_9005[retailer_9006, nomatch=0L][retailer_9099, nomatch=0L]

# Check the regression and prices -- Looks like that size and month are most important explanatory force.
retailer_9005_9006[, summary(lm(price ~ i.price + factor(brand_descr) * factor(size1_amount) + factor(month)))]
retailer_9005_9006[, summary(lm(i.price ~ price + factor(brand_descr) * factor(size1_amount) + factor(month)))]

#---------------------------------------------------------------------------------------------------#
# Obtain the top retailers for Keurig and other ground coffees -- determine which retailers to keep, 
# and which are purely for state transition purposes.
retailer_type_sales = purchases[as.integer(selected)==1, .(retailer_sales = sum(total_price_paid-coupon_value),
                                                           npurch = .N), 
                                by = c("retailer_code", "keurig")]
retailer_type_sales[, total_sales := sum(retailer_sales), by = "keurig"]
retailer_type_sales[, retailer_share := retailer_sales/total_sales]
retailer_type_sales = retailer_type_sales[order(-retailer_share), ]
retailer_type_sales[, cumshare := cumsum(retailer_share), by = "keurig"]
retailer_type_sales[, presence := .N, by = "retailer_code"]
retailer_type_sales[, retailer_rank := 1:length(retailer_code), by = "keurig"]

# Obtain the list of retailers in RMS - parent_code and retailer_code
stores[, `:=`(nretailer_codes = length(na.omit(unique(retailer_code)))), by = "store_code_uc"]
stores[as.integer(nretailer_codes)==0, `:=`(retailer_code = parent_code)]
stores[as.integer(nretailer_codes)==1, `:=`(retailer_code = na.omit(retailer_code)[1]), by = "store_code_uc"]
# For the rest of the stores roll to the last
stores_temp = stores[!is.na(retailer_code), ]
setnames(stores_temp, "retailer_code", "retailer_code_temp")
setkeyv(stores_temp, c("store_code_uc", "parent_code", "panel_year"))
setkeyv(stores, c("store_code_uc", "parent_code", "panel_year"))
stores = stores_temp[, .(store_code_uc, parent_code, panel_year, 
                         retailer_code_temp)][stores, roll=TRUE, rollends=c(TRUE, TRUE)]
stores[is.na(retailer_code), retailer_code:=retailer_code_temp]
stores[, retailer_code_temp:=NULL]

store_codes = stores[, .(nretailer_codes = length(na.omit(unique(retailer_code))), 
                         retailer_code = na.omit(unique(retailer_code))[1]), 
                     by = c("store_code_uc", "parent_code")]
rms_retailer_list = unique(store_codes[as.integer(nretailer_codes)==1, .(retailer_code = as.integer(retailer_code))])
rms_retailer_list[, in_rms:=1]

# Merge in RMS information into purchases
setkey(rms_retailer_list, retailer_code)
setkey(retailer_type_sales, retailer_code)
retailer_type_sales = rms_retailer_list[retailer_type_sales]
retailer_type_sales[is.na(in_rms), in_rms:=0]
setkey(retailer_type_sales, keurig, retailer_rank)

# Obtain list of retailers -- either high in volume (top 25 in sales) or in RMS.
# The following retailers represent aggregate store types -- thus, difficult to measure price and related environment.
# Or the retailer represents no physical store visits such as mail order etc
exclude_retailers = c(5099, 9099, 9299, 9999)
# For top 25 retailers (either Keurig or ground) -- we impute the sales
impute_retailers = unique(retailer_type_sales[retailer_rank<=25 & !(retailer_code%in%exclude_retailers), 
                                              retailer_code])
# With this criteria, we obtain about 77-78% in both ground coffee and Keurig K-Cups.
retailer_type_sales[, selected := as.integer(retailer_rank<=25|in_rms==TRUE)]
retailer_type_sales[, selected := as.integer(sum(selected)>=1), by = "retailer_code"]
#exclude_retailers = c(5099, 9099, 9999)
retailer_type_sales[retailer_code %in% exclude_retailers, selected := 0]
selected_retailers = unique(retailer_type_sales[as.integer(selected)==1, retailer_code])
rc_temp = retailer_type_sales[as.integer(selected)==1 & retailer_rank<=25, retailer_code]
selected_channels = retailers[retailer_code%in%rc_temp, unique(channel_type)]
#---------------------------------------------------------------------------------------------------#
# Now build the retailer - week panel of prices and product availability.
# Define week_end to be the saturday ending that week.
first_week_end = as.Date("2004-01-03")
max_purchase_date = purchases[, max(purchase_date)]
purchases[purchase_date<=first_week_end, week_end:=first_week_end]
cweek = first_week_end
while (cweek < max_purchase_date){
  cweek = cweek + 7
  purchases[purchase_date<=cweek & is.na(week_end), week_end:=cweek]
}
# Purchases original brand name, and also brand_names for other smaller brands
purchases[, brand_descr_orig := brand_descr]
purchases[, brand_descr := ifelse(brand_descr%in%selected_brand_list, brand_descr, "OTHER")]
purchases[, price:=(total_price_paid-coupon_value)/quantity]

# Obtain the list of products and availability date in terms of brand, size, DMA and retailer
product_panel = purchases[channel_type%in%selected_channels&panel_year>=2006&
                            retailer_code%in%impute_retailers, 
                          .(first_week_end = min(week_end),
                            last_week_end = max(week_end),
                            revenue = sum(total_price_paid - coupon_value),
                            quantity = sum(quantity)),
                          by = c("retailer_code", "dma_code", "brand_descr", "keurig", "size1_amount", 
                                 "ptype", "roast", "flavored", "kona", "colombian", "sumatra", "wb")]
product_panel[, row_number := 1:.N]

# Initialize the complete retailer dma and product panel
product_panel_temp = data.table(expand.grid(row_number = 1:nrow(product_panel),
                                            week_end = unique(purchases[, week_end])))
setkey(product_panel, row_number)
setkey(product_panel_temp, row_number)
product_panel = product_panel[product_panel_temp]
product_panel = product_panel[week_end>=first_week_end & week_end<=last_week_end, ]
setkeyv(product_panel, c("retailer_code", "dma_code", "week_end", "brand_descr", "keurig", "size1_amount",
                         "ptype", "roast", "flavored", "kona", "colombian", "sumatra", "wb"))

# !!! Not needed unless the above panel was NOT established  at retailer-dma level
# # Initialize the panel for retailer information.
# # Obtain the unique list of store and product combination
# retailer_temp = purchases[, .(row_number=1), by = c("dma_code", "retailer_code", "channel_type", 
#                                  "brand_descr", "keurig", "size1_amount", "ptype", "roast", 
#                                  "flavored", "kona", "colombian", "sumatra", "wb")]
# retailer_temp[, row_number:=1:.N]
# 
# # Replicate it as many times as there are for weeks and dmas.
# retailer_panel_1 =  as.data.table(expand.grid(row_number = 1:nrow(retailer_temp), 
#                                               week_end = unique(purchases[, week_end])))
# setkey(retailer_temp, row_number)
# setkey(retailer_panel_1, row_number)
# retailer_panel_1 = retailer_temp[retailer_panel_1]
# retailer_panel_1[, row_number:=NULL]

# # Merge the panel with product selling span (product_panel)
# setkeyv(retailer_panel_1, c("retailer_code", "dma_code", "brand_descr", "keurig", "size1_amount", "ptype", 
#                             "roast", "flavored", "kona", "colombian", "sumatra", "wb", "week_end"))
# setkeyv(product_panel, c("retailer_code", "dma_code", "brand_descr", "keurig", "size1_amount", "ptype", 
#                          "roast", "flavored", "kona", "colombian", "sumatra", "wb", "week_end"))
# nameorder = names(retailer_panel_1)
# retailer_panel_1 = retailer_panel_1[product_panel[, .(retailer_code, dma_code, brand_descr, keurig, 
#                                                       size1_amount, ptype, roast, flavored, kona, colombian, 
#                                                       sumatra, wb, week_end)], nomatch=0L]
# setcolorder(retailer_panel_1, nameorder)

#!!! Only if the panel is at chain dma level
retailer_panel_1 = copy(product_panel)
setkeyv(retailer_panel_1, c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount",
                            "flavored", "kona", "colombian", "sumatra", "wb"))
retailer_panel_1[, month:=substr(as.character(week_end), 1, 7)]

# Check whether every retailers is operating for every week
unique(retailer_panel_1[, .(dma_code, retailer_code, week_end)])[, .N, by = c("dma_code", "retailer_code")][N<313, ]

# Given the initialized panel, the task is then to fill the panel with imputed prices.
# The most accurate price is the price from the retailer at the dma - so obtain that first
retailer_temp = purchases[, .(price_avg = sum(total_price_paid-coupon_value)/sum(quantity),
                            price_mid = median(price)),
                          by = c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount",
                                 "flavored", "kona", "colombian", "sumatra", "wb")]
retailer_temp[, cor(price_avg, price_mid)] # correlation is about >0.98, probably doesn't matter which price I use.
setkeyv(retailer_temp, c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount",
                         "flavored", "kona", "colombian", "sumatra", "wb"))
retailer_panel_1 = retailer_temp[retailer_panel_1]

# Get regional sales and prices - one dma may span two/three regions, so do this dma by dma
retailer_region_prices = as.list(top_dma_list)
i = 0
for (dma_n in top_dma_list){
  i = i+1
  dma_region_list = unique(purchases[as.integer(dma_code) == dma_n, unique(region_code)])
  retailer_temp = purchases[region_code %in% dma_region_list, 
                            .(region_price_avg = sum(total_price_paid - coupon_value)/sum(quantity), 
                              region_price_mid = median(price)),
                            by = c("retailer_code", "week_end", "brand_descr", "keurig", "size1_amount",
                                   "flavored", "kona", "colombian", "sumatra", "wb")]
  retailer_temp[, dma_code := dma_n]
  retailer_region_prices[[i]] = retailer_temp
}
retailer_region_prices = rbindlist(retailer_region_prices)
setkeyv(retailer_region_prices, c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount",
                                  "flavored", "kona", "colombian", "sumatra", "wb"))
retailer_panel_1 = retailer_region_prices[retailer_panel_1]

# Now, same chain -- national price 
retailer_temp = purchases[, .(natl_price_avg = sum(total_price_paid - coupon_value)/sum(quantity), 
                            natl_price_mid = median(price)),
                          by = c("retailer_code", "week_end", "brand_descr", "keurig", "size1_amount",
                                 "flavored", "kona", "colombian", "sumatra", "wb")]
setkeyv(retailer_temp, c("retailer_code", "week_end", "brand_descr", "keurig", "size1_amount",
                         "flavored", "kona", "colombian", "sumatra", "wb"))
setkeyv(retailer_panel_1, c("retailer_code", "week_end", "brand_descr", "keurig", "size1_amount",
                            "flavored", "kona", "colombian", "sumatra", "wb"))
retailer_panel_1 = retailer_temp[retailer_panel_1]

# Put these prices together
retailer_panel_1[, `:=`(price = price_avg, mprice = price_mid)]
retailer_panel_1[is.na(price), `:=`(price = region_price_avg, mprice = region_price_mid)]
retailer_panel_1[is.na(price), `:=`(price = natl_price_avg, mprice = natl_price_mid)]

# Now about 64% of the observations have been filled for the retailers we are interested in top DMAs
# Now, fill the remaining observations using time regressions within store
# size and week are the most scant information: A flexible time trend may help with this.
# The next is week and brand...
retailer_panel_1[, p_temp1 := mean(price, na.rm=TRUE), by = c("retailer_code", "month", "brand_descr", 
                                                              "keurig", "size1_amount")]
retailer_panel_1[, p_temp2 := mean(price, na.rm=TRUE), by = c("retailer_code", "week_end", "brand_descr", "keurig")]
retailer_panel_1[, p_temp3 := mean(price, na.rm=TRUE), by = c("retailer_code", "week_end", "keurig", "size1_amount")]
retailer_panel_1[, p_temp4 := mean(price, na.rm=TRUE), by = c("retailer_code", "month", "brand_descr", "keurig")]
retailer_panel_1[, p_temp5 := mean(price, na.rm=TRUE), by = c("retailer_code", "month", "keurig", "size1_amount")]
retailer_panel_1[, p_temp6 := mean(price, na.rm=TRUE), by = c("retailer_code", "week_end", "brand_descr")]
retailer_panel_1[, p_temp7 := mean(price, na.rm=TRUE), by = c("retailer_code", "week_end", "keurig")]
retailer_panel_1[, p_temp8 := mean(price, na.rm=TRUE), by = c("retailer_code", "month", "brand_descr")]
retailer_panel_1[, p_temp9 := mean(price, na.rm=TRUE), by = c("retailer_code", "month", "keurig")]
retailer_panel_1[, p_temp10 := mean(price, na.rm=TRUE), by = c("retailer_code", "brand_descr", "keurig", 
                                                               "size1_amount")]
retailer_panel_1[, p_temp11 := mean(price, na.rm=TRUE), by = c("retailer_code", "brand_descr", "keurig")]
retailer_panel_1[, p_temp12 := mean(price, na.rm=TRUE), by = c("retailer_code", "keurig", "size1_amount")]

# Filter the dmas to be only top DMAs -- give better reliability in imputation
retailer_panel_1 = retailer_panel_1[dma_code %in% top_dma_list, ]

# Filter DMA and chains so we don't mis-present a a chain when it's not available. 
setkey(retailer_panel_1, dma_code, retailer_code)
retailer_panel_1[np_dma_retailer, nomatch=0L][np>=np_threshold, ]

# Impute the prices retailer by retailer
retailer_R2 = as.data.table(expand.grid(retailer_code = impute_retailers, 
                                        model_code = 1:11))
retailer_R2[, rsquared := 0]
for (i in impute_retailers){
  dt_temp = retailer_panel_1[as.integer(retailer_code)==i, ] 
  regout = lm(price ~ p_temp1 + p_temp4 + p_temp5 + p_temp8 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end)+factor(roast)+kona+colombian+sumatra+wb, data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  regout$xlevels[["factor(roast)"]] <- union(regout$xlevels[["factor(roast)"]], levels(factor(dt_temp$roast)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_1 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==1, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp2 + p_temp4 + p_temp5 + p_temp8 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end)+factor(roast)+kona+colombian+sumatra+wb, data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  regout$xlevels[["factor(roast)"]] <- union(regout$xlevels[["factor(roast)"]], levels(factor(dt_temp$roast)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_2 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==2, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp2 + p_temp4 + p_temp8 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end)+factor(roast)+kona+colombian+sumatra+wb, data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  regout$xlevels[["factor(roast)"]] <- union(regout$xlevels[["factor(roast)"]], levels(factor(dt_temp$roast)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_3 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==3, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp3 + p_temp5 + p_temp7 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end)+factor(roast)+kona+colombian+sumatra+wb, data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  regout$xlevels[["factor(roast)"]] <- union(regout$xlevels[["factor(roast)"]], levels(factor(dt_temp$roast)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_4 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==4, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp4 + p_temp8 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end)+factor(roast)+kona+colombian+sumatra+wb, data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  regout$xlevels[["factor(roast)"]] <- union(regout$xlevels[["factor(roast)"]], levels(factor(dt_temp$roast)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_5 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==5, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp5 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end)+factor(roast)+kona+colombian+sumatra+wb, data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  regout$xlevels[["factor(roast)"]] <- union(regout$xlevels[["factor(roast)"]], levels(factor(dt_temp$roast)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_6 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==6, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp6 + p_temp8 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end)+factor(roast)+kona+colombian+sumatra+wb, data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  regout$xlevels[["factor(roast)"]] <- union(regout$xlevels[["factor(roast)"]], levels(factor(dt_temp$roast)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_7 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==7, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp7 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end)+factor(roast)+kona+colombian+sumatra+wb, data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  regout$xlevels[["factor(roast)"]] <- union(regout$xlevels[["factor(roast)"]], levels(factor(dt_temp$roast)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_8 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i &  as.integer(model_code)==8, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp8 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end)+factor(roast)+kona+colombian+sumatra+wb, data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  regout$xlevels[["factor(roast)"]] <- union(regout$xlevels[["factor(roast)"]], levels(factor(dt_temp$roast)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_9 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==9, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end)+factor(roast)+kona+colombian+sumatra+wb, data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  regout$xlevels[["factor(roast)"]] <- union(regout$xlevels[["factor(roast)"]], levels(factor(dt_temp$roast)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_10 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==10, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end)+factor(roast)+kona+colombian+sumatra+wb, data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  regout$xlevels[["factor(roast)"]] <- union(regout$xlevels[["factor(roast)"]], levels(factor(dt_temp$roast)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_11:= predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==11, rsquared := summary(regout)$r.squared]
  
  cat("Retailer", i, "finished processing.\n")
}

# Replace the price with imputed price whenver missing in the order of R-squared
retailer_R2 = retailer_R2[order(-rsquared), ]
setkey(retailer_R2, retailer_code)
for (i in 1:nrow(retailer_R2)){
  focal_model = paste("p_imputed_", retailer_R2[i, model_code], sep="")
  setnames(retailer_panel_1, focal_model, "focal_var")
  retailer_panel_1[as.integer(retailer_code)==retailer_R2[i, retailer_code]&is.na(price), price:=focal_var]
  setnames(retailer_panel_1, "focal_var", focal_model)
  cat("Row", i, "finished processing.\n")
}

# Remove the unneeded variables
retailer_panel_1[, `:=`(p_temp1=NULL, p_temp2=NULL, p_temp3=NULL, p_temp4=NULL, p_temp5=NULL, p_temp6=NULL, 
                        p_temp7=NULL, p_temp8=NULL, p_temp9=NULL, p_temp10=NULL,p_temp11=NULL, p_temp12=NULL, 
                        p_imputed_1=NULL, p_imputed_2=NULL, p_imputed_3=NULL, p_imputed_4=NULL, p_imputed_5=NULL, 
                        p_imputed_6=NULL, p_imputed_7=NULL, p_imputed_8=NULL, p_imputed_9=NULL, p_imputed_10=NULL, 
                        p_imputed_11=NULL)]
gc()
#save(retailer_panel_1, file = paste(output_dir, "/HMS_Imputed_Prices.RData", sep=""))
save(retailer_panel_1, file = paste(output_dir, "/HMS_Imputed_Prices_Region.RData", sep=""))

#---------------------------------------------------------------------------------------------------#
# Load RMS Movement and aggregate to get average price
load(paste(RMS_input_dir, "/1463.RData", sep=""))
move[, `:=`(feature=NULL, display=NULL)]
setkeyv(move, c("store_code_uc", "panel_year"))
setkeyv(stores, c("store_code_uc", "panel_year"))
move = move[stores[,.(store_code_uc, panel_year, retailer_code, dma_code)], nomatch=0L]
move = move[dma_code %in% top_dma_list, ]
setkeyv(move, c("upc", "upc_ver_uc"))
setkeyv(products, c("upc", "upc_ver_uc"))
products[, keurig:=as.integer(ptype=="KEURIG")]
move = move[products[,.(upc, upc_ver_uc, brand_descr, keurig, size1_amount,
                        ptype, roast, flavored, kona, colombian, sumatra, wb)], nomatch=0L]
move[, brand_descr := ifelse(brand_descr%in%selected_brand_list, brand_descr, "OTHER")]
move = move[, .(rms_price = mean(price), rms_units = sum(units*prmult), 
                rms_revenue = sum(price*units*prmult)),
            by = c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount",
                   "ptype", "roast", "flavored", "kona", "colombian", "sumatra", "wb")]
# Merge with original panel, and check the prices.
setkeyv(move, c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount",
                "ptype", "roast", "flavored", "kona", "colombian", "sumatra", "wb"))
setkeyv(retailer_panel_1, c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount",
                            "ptype", "roast", "flavored", "kona", "colombian", "sumatra", "wb"))
move[retailer_panel_1][retailer_code==6901, cor(price, rms_price, use="pairwise.complete.obs")]
save(move, file = paste(output_dir, "/RMS_Imputed_Prices.RData", sep=""))
move[retailer_panel_1, nomatch=0L][, cor(price, rms_price, use = "pairwise.complete.obs")]
move[retailer_panel_1, nomatch=0L][, cor(price_avg, rms_price, use = "pairwise.complete.obs")]
move[retailer_panel_1, nomatch=0L][, cor(price_avg, rms_revenue/rms_units, use = "pairwise.complete.obs")]
move[retailer_panel_1, nomatch=0L][, cor(natl_price_avg, rms_revenue/rms_units, use = "pairwise.complete.obs")]
move[retailer_panel_1, nomatch=0L][, cor(price, rms_revenue/rms_units, use = "pairwise.complete.obs")]
# The imputation does pretty well! # Appendix showing percent of price variation retained!
gc()
#---------------------------------------------------------------------------------------------------#
# Construct the price panel using HMS imputed prices and RMS prices. 
# Use RMS prices for RMS stores, and use HMS prices for other stores [top movement]
move = move[retailer_code %in% rms_retailer_list[, retailer_code], ]
move[, `:=`(price=rms_price, rms_price=NULL, rms_units=NULL, rms_revenue=NULL)]
retailer_panel_1[, `:=`(natl_price_avg=NULL, natl_price_mid=NULL, region_price_avg=NULL, region_price_mid=NULL,
                        price_avg=NULL, price_mid=NULL, channel_type=NULL, month=NULL, mprice=NULL, revenue=NULL, 
                        first_week_end=NULL, last_week_end=NULL, row_number=NULL, quantity=NULL)]
setcolorder(retailer_panel_1, names(move))
move[, in_rms:=1]
retailer_panel_1[, in_rms:=0]
retailer_panel = rbindlist(list(move, retailer_panel_1[!(retailer_code %in% rms_retailer_list[, retailer_code])]))

# Add no purchase option to the retailer panel
retailer_panel_no_purch = unique(retailer_panel[, .(dma_code, retailer_code, week_end, in_rms)])
retailer_panel_no_purch[, `:=`(brand_descr="0NOTHING", keurig=0, size1_amount=0, roast=0, flavored=0,
                               kona=0, colombian=0, sumatra=0, wb=0, price=0, ptype = "OTHER")]
setcolorder(retailer_panel_no_purch, names(retailer_panel))
retailer_panel = rbindlist(list(retailer_panel, retailer_panel_no_purch))
setkeyv(retailer_panel, c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount",
                          "ptype", "roast", "flavored", "kona", "colombian", "sumatra", "wb"))

# Save data for retailer_panel 
#save(retailer_panel, file = paste(output_dir, "/Retailer_Price_Panel.RData", sep=""))
save(retailer_panel, file = paste(output_dir, "/Retailer_Price_Panel_Region.RData", sep=""))

# Save the list of auxiliary data sets and use them in further processing of data
save(purchases, hh, hh_list, retailers, retailer_type_sales, selected_brand_list,
     top_dma_list, top_keurig_brands,
     file = paste(output_dir, "/Assist_Data_Sets_Retailer_Prices.RData", sep=""))
gc()

