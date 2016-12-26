#---------------------------------------------------------------------------------------------------#
# Load appropriate data
# Load HH file with flags
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))
load(paste(meta_dir, "/HH.RData", sep=""))

# Load HMS Trips Data
load(paste(HMS_trip_dir, "/Trips.RData", sep=""))

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

# Record trips that are coffee related 
coffee_related_trips = unique(purchases[, .(trip_code_uc)])

# Make purchases trip, product unique
pnamelist = names(purchases)
pnamelist = setdiff(pnamelist, c("total_spent", "total_price_paid", "coupon_value", "deal_flag_uc","quantity"))
purchases[upc=="009955504002" & as.integer(quantity) == 24, quantity:=2]
purchases[upc=="009955504002" & as.integer(quantity) == 12, quantity:=1]

purchases = purchases[, .(total_price_paid = sum(total_price_paid),
                          coupon_value = sum(coupon_value),
                          quantity = sum(quantity),
                          total_spent = total_spent[1]), by = pnamelist]

# Merge in the type information to the purchase data
setkeyv(purchases, c("upc", "upc_ver_uc"))
purchases=purchases[products[, .(upc, upc_ver_uc, ptype, brand_descr, size1_amount, multi)], nomatch=0L]
purchases[, month:=as.factor(format(purchase_date, '%Y-%m'))]

# Restrict purchases to specific software purchases
purchases = purchases[product_module_code %in% focal_module, ]

# Only Purchases Beyond 2008 matters 
purchases = purchases[panel_year>=2008, ]

# Merge purchases with household dma information to locate stores
setkeyv(purchases, c("household_code", "panel_year"))
setkeyv(hh, c("household_code", "panel_year"))
purchases = purchases[hh[, .(household_code, panel_year, dma_code, dma_descr, region_code,
                             fips_county_code, fips_county_descr, panelist_zip_code)], nomatch=0L]

# Only keep households in the top N DMAs in terms of adopters
dma_rank_list = unique(hh_info_panel[as.integer(kholder)==1, .(household_code, dma_code)]) # Households double count if moved. 
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

# Only purchases from the chosen households matter - make a select flag
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

# Merge back into trips data to get only the relevant trips.
setkey(hh_retailer_list, retailer_code)
setkey(trips, retailer_code)
trips = trips[hh_retailer_list, nomatch=0L]
trips = trips[household_code %in% hh_list[, household_code]]

# Only Trips Beyond 2008 matters 
trips = trips[panel_year>=2008, ]
setkeyv(trips, c("household_code", "purchase_date", "trip_code_uc"))

# Flag Trips with coffee purchases.
coffee_trips = unique(purchases[as.integer(selected)==1, trip_code_uc])
trips[, coffee_trip:=as.integer(trip_code_uc %in% coffee_trips)]
rm(coffee_trips)

# Merge in retailer channel information
setkey(trips, retailer_code)
trips = retailers[trips, nomatch=0L]
setkeyv(trips, c("household_code", "purchase_date", "trip_code_uc"))

# Obtain the list of online retailers -- they have nation wide pricing
online_retailers = retailers[channel_type=="Online Shopping", retailer_code]

# Change the purchases online retailer code to the online retailer code
purchases[retailer_code %in% online_retailers, retailer_code:=4849]
trips[retailer_code %in% online_retailers, retailer_code:=4849]

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
selected_brand_list = unique(brand_type_sales[(as.integer(keurig)==1 & cumshare<=0.96) | (as.integer(keurig)==0 & cumshare<=0.92), brand_descr]) 
top_keurig_brands = brand_type_sales[as.integer(keurig)==1 & brand_descr!="CTL BR" & brand_share>=0.04, brand_descr]
top_selling_brands = unique(brand_type_sales[brand_share>=0.04, brand_descr])
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
stores = stores_temp[, .(store_code_uc, parent_code, panel_year, retailer_code_temp)][stores, roll=TRUE, rollends=c(TRUE, TRUE)]
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

# Obtain list of retailers -- either high in volume (top 30 in sales) or in RMS. 
# With this criteria, we obtain about 77-78% in both ground coffee and Keurig K-Cups.
retailer_type_sales[, selected := as.integer((as.integer(keurig)==1&retailer_rank<=25)|in_rms==TRUE)]
retailer_type_sales[, selected := as.integer(sum(selected)>=1), by = "retailer_code"]
# The following retailers represent aggregate store types -- thus, difficult to measure price and related environment.
# Or the retailer represents no physical store visits such as mail order etc
exclude_retailers = c(5099, 9099, 9299, 9999)
#exclude_retailers = c(5099, 9099, 9999)
retailer_type_sales[retailer_code %in% exclude_retailers, selected := 0]
selected_retailers = unique(retailer_type_sales[as.integer(selected)==1, .(retailer_code)])
selected_channels = retailers[retailer_code%in%retailer_type_sales[as.integer(selected)==1 & retailer_rank<=25, retailer_code], 
                              unique(channel_type)]
#---------------------------------------------------------------------------------------------------#
# Now build the retailer - week panel of prices and product availability.
# Define week_end to be the saturday ending that week.
first_week_end = as.Date("2008-01-05")
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

# Obtain the list of products and availability date in terms of brand, size, DMA and channel
product_panel = purchases[channel_type %in% selected_channels, 
                          .(first_week_end = min(week_end),
                            last_week_end = max(week_end),
                            revenue = sum(total_price_paid - coupon_value),
                            quantity = sum(quantity)),
                          by = c("retailer_code", "brand_descr", "keurig", "size1_amount")]
product_panel[, row_number := 1:.N]

# Initialize the complete product panel
product_panel_temp = data.table(expand.grid(row_number = 1:nrow(product_panel),
                                            week_end = unique(purchases[, week_end])))
setkey(product_panel, row_number)
setkey(product_panel_temp, row_number)
product_panel = product_panel[product_panel_temp]
product_panel = product_panel[week_end>=first_week_end & week_end<=last_week_end, ]
setkeyv(product_panel, c("retailer_code", "week_end", "brand_descr", "keurig", "size1_amount"))

# Create the market, retailer, brand, size and week_end panel for top retailers (inclusive of RMS for testing)
top_retailers  = unique(retailer_type_sales[as.integer(selected)==1 & retailer_rank<=25, retailer_code])

# Initialize the panel for retailer information.
# Obtain the unique list of store and product combination
retailer_temp = purchases[retailer_code%in%top_retailers, .(row_number=1),
                          by = c("dma_code", "retailer_code", "channel_type", 
                                 "brand_descr", "keurig", "size1_amount")]
retailer_temp[, row_number:=1:.N]

# Replicate it as many times as there are for weeks and dmas.
retailer_panel_1 =  as.data.table(expand.grid(row_number = 1:nrow(retailer_temp), 
                                              week_end = unique(purchases[, week_end])))
setkey(retailer_temp, row_number)
setkey(retailer_panel_1, row_number)
retailer_panel_1 = retailer_temp[retailer_panel_1]
retailer_panel_1[, row_number:=NULL]

# Merge the panel with product selling span (product_panel)
setkeyv(retailer_panel_1, c("retailer_code", "brand_descr", "keurig", "size1_amount", "week_end"))
setkeyv(product_panel, c("retailer_code", "brand_descr", "keurig", "size1_amount", "week_end"))
nameorder = names(retailer_panel_1)
retailer_panel_1 = retailer_panel_1[product_panel[, .(retailer_code, brand_descr, keurig, 
                                                      size1_amount, week_end)], nomatch=0L]
setcolorder(retailer_panel_1, nameorder)
setkeyv(retailer_panel_1, c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount"))
retailer_panel_1[, month:=substr(as.character(week_end), 1, 7)]

# Check whether every retailers is operating for every week
unique(retailer_panel_1[, .(dma_code, retailer_code, week_end)])[, .N, by = c("dma_code", "retailer_code")][N<313, ]

# Given the initialized panel, the task is then to fill the panel with imputed prices.
# The most accurate price is the price from the retailer at the dma - so obtain that first
retailer_temp = purchases[dma_code%in%top_dma_list & retailer_code%in%top_retailers, 
                          .(price_avg = sum(total_price_paid-coupon_value)/sum(quantity),
                            price_mid = median(price)),
                          by = c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount")]
retailer_temp[, cor(price_avg, price_mid)] # correlation is about 0.9836, probably doesn't matter which price I use.
setkeyv(retailer_temp, c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount"))
retailer_panel_1 = retailer_temp[retailer_panel_1]

# Get regional sales and prices - one dma may span two/three regions, so this dma by dma
retailer_region_prices = as.list(top_dma_list)
i = 0
for (dma_n in top_dma_list){
  i = i+1
  dma_region_list = unique(purchases[as.integer(dma_code) == dma_n, unique(region_code)])
  retailer_temp = purchases[region_code %in% dma_region_list & retailer_code%in%top_retailers, 
                            .(region_price_avg = sum(total_price_paid - coupon_value)/sum(quantity), 
                              region_price_mid = median(price)),
                            by = c("retailer_code", "week_end", "brand_descr", "keurig", "size1_amount")]
  retailer_temp[, dma_code := dma_n]
  retailer_region_prices[[i]] = retailer_temp
}
retailer_region_prices = rbindlist(retailer_region_prices)
setkeyv(retailer_region_prices, c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount"))
retailer_panel_1 = retailer_region_prices[retailer_panel_1]

# Now, same chain -- national price 
retailer_temp = purchases[retailer_code%in%top_retailers, 
                          .(natl_price_avg = sum(total_price_paid - coupon_value)/sum(quantity), 
                            natl_price_mid = median(price)),
                          by = c("retailer_code", "week_end", "brand_descr", "keurig", "size1_amount")]
setkeyv(retailer_temp, c("retailer_code", "week_end", "brand_descr", "keurig", "size1_amount"))
setkeyv(retailer_panel_1, c("retailer_code", "week_end", "brand_descr", "keurig", "size1_amount"))
retailer_panel_1 = retailer_temp[retailer_panel_1]

# Put these prices together
retailer_panel_1[, `:=`(price = price_avg, mprice = price_mid)]
retailer_panel_1[is.na(price), `:=`(price = region_price_avg, mprice = region_price_mid)]
retailer_panel_1[is.na(price), `:=`(price = natl_price_avg, mprice = natl_price_mid)]

# Now about 40% of the observations have been filled. 
# Now, fill the remaining observations using time regressions within store
# size and week are the most scant information: A flexible time trend may help with this.
# The next is week and brand...
retailer_panel_1[, p_temp1 := mean(price, na.rm=TRUE), by = c("retailer_code", "month", "brand_descr", "keurig", "size1_amount")]
retailer_panel_1[, p_temp2 := mean(price, na.rm=TRUE), by = c("retailer_code", "week_end", "brand_descr", "keurig")]
retailer_panel_1[, p_temp3 := mean(price, na.rm=TRUE), by = c("retailer_code", "week_end", "keurig", "size1_amount")]
retailer_panel_1[, p_temp4 := mean(price, na.rm=TRUE), by = c("retailer_code", "month", "brand_descr", "keurig")]
retailer_panel_1[, p_temp5 := mean(price, na.rm=TRUE), by = c("retailer_code", "month", "keurig", "size1_amount")]
retailer_panel_1[, p_temp6 := mean(price, na.rm=TRUE), by = c("retailer_code", "week_end", "brand_descr")]
retailer_panel_1[, p_temp7 := mean(price, na.rm=TRUE), by = c("retailer_code", "week_end", "keurig")]
retailer_panel_1[, p_temp8 := mean(price, na.rm=TRUE), by = c("retailer_code", "month", "brand_descr")]
retailer_panel_1[, p_temp9 := mean(price, na.rm=TRUE), by = c("retailer_code", "month", "keurig")]
retailer_panel_1[, p_temp10 := mean(price, na.rm=TRUE), by = c("retailer_code", "brand_descr", "keurig", "size1_amount")]
retailer_panel_1[, p_temp11 := mean(price, na.rm=TRUE), by = c("retailer_code", "brand_descr", "keurig")]
retailer_panel_1[, p_temp12 := mean(price, na.rm=TRUE), by = c("retailer_code", "keurig", "size1_amount")]

# Impute the prices retailer by retailer
retailer_R2 = as.data.table(expand.grid(retailer_code = top_retailers, 
                                        model_code = 1:11))
retailer_R2[, rsquared := 0]
for (i in top_retailers){
  dt_temp = retailer_panel_1[as.integer(retailer_code)==i, ] 
  regout = lm(price ~ p_temp1 + p_temp4 + p_temp5 + p_temp8 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end), data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_1 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==1, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp2 + p_temp4 + p_temp5 + p_temp8 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end), data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_2 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==2, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp2 + p_temp4 + p_temp8 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end), data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_3 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==3, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp3 + p_temp5 + p_temp7 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end), data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_4 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==4, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp4 + p_temp8 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end), data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_5 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==5, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp5 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end), data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_6 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==6, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp6 + p_temp8 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end), data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_7 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==7, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp7 + p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end), data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_8 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i &  as.integer(model_code)==8, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp8 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end), data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_9 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==9, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp9 + p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end), data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
  retailer_panel_1[as.integer(retailer_code)==i, p_imputed_10 := predict(regout, dt_temp)]
  retailer_R2[as.integer(retailer_code)==i & as.integer(model_code)==10, rsquared := summary(regout)$r.squared]
  
  regout = lm(price ~ p_temp10 + p_temp11 + 
                p_temp12 + factor(dma_code) + keurig + factor(brand_descr) + factor(size1_amount) + 
                factor(week_end), data =dt_temp)
  regout$xlevels[["factor(week_end)"]] <- union(regout$xlevels[["factor(week_end)"]], levels(factor(dt_temp$week_end)))
  regout$xlevels[["factor(size1_amount)"]] <- union(regout$xlevels[["factor(size1_amount)"]], levels(factor(dt_temp$size1_amount)))
  regout$xlevels[["factor(brand_descr)"]] <- union(regout$xlevels[["factor(brand_descr)"]], levels(factor(dt_temp$brand_descr)))
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
save(retailer_panel_1, file = paste(output_dir, "/HMS_Imputed_Prices.RData", sep=""))
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
move = move[products[,.(upc, upc_ver_uc, brand_descr, keurig, size1_amount)], nomatch=0L]
move[!(brand_descr %in% selected_brand_list), brand_descr:="OTHER"]
move = move[, .(rms_price = mean(price), rms_units = sum(units*prmult), rms_revenue=sum(price*units*prmult)),
            by = c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount")]
# Merge with original panel, and check the prices.
setkeyv(move, c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount"))
setkeyv(retailer_panel_1, c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount"))
move[retailer_panel_1][as.integer(retailer_code)==6901, cor(price, rms_price, use="pairwise.complete.obs")]
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
                        price_avg=NULL, price_mid=NULL, channel_type=NULL, month=NULL, mprice=NULL)]
setcolorder(retailer_panel_1, names(move))
move[, in_rms:=1]
retailer_panel_1[, in_rms:=0]
retailer_panel = rbindlist(list(move, retailer_panel_1[!(retailer_code %in% rms_retailer_list[, retailer_code])]))

# Add no purchase option to the retailer panel
retailer_panel_no_purch = unique(retailer_panel[, .(dma_code, retailer_code, week_end, in_rms)])
retailer_panel_no_purch[, `:=`(brand_descr="0NOTHING", keurig=0, size1_amount=0, price=0)]
setcolorder(retailer_panel_no_purch, names(retailer_panel))
retailer_panel = rbindlist(list(retailer_panel, retailer_panel_no_purch))
setkeyv(retailer_panel, c("dma_code", "retailer_code", "week_end", "brand_descr", "keurig", "size1_amount"))

# Create the Ptype Variable
ptype <- function(x){
  return (ifelse(grepl("KEURIG VUE", x)|grepl("KEURIG V", x)|grepl("KEURG V", x)|grepl("KURG VU", x)|grepl("KVUE", x), "VUE", 
                 ifelse(grepl("KEURIG", x)|grepl("KCUP", x)|grepl("KURG", x)|grepl("KEURG",x), "KEURIG",
                        ifelse(grepl("TSSM", x)|grepl("TASSIMO",x), "TASSIMO",
                               ifelse(grepl("SENSEO", x),"SENSEO",
                                      ifelse(grepl("DOLCE GUSTO", x)|grepl("CAFE BONKA", x), "GUSTO", "OTHER"))))))
}

# Recreate the ptype variable for retailer_panel 
retailer_panel[, ptype := ptype(brand_descr)]
retailer_panel[as.integer(keurig)==1, ptype := "KEURIG"]
save(retailer_panel, file = paste(output_dir, "/Retailer_Price_Panel.RData", sep=""))

#---------------------------------------------------------------------------------------------------#
# Now prepare the consumer panel.

# Create week_end variable in trips
date_week_end = unique(purchases[, .(purchase_date, week_end)])
setkey(date_week_end, purchase_date)
setkey(trips, purchase_date)
trips = trips[date_week_end, nomatch=0L]

# Get rid of trips definitely not purchasing coffee - two criteria - top 99% coffee retailers or the household shopped 
# at the retailer for coffee at least twice
coffee_retailer_99 = unique(retailer_type_sales[cumshare<=0.9901, retailer_code])
coffee_retailer_95 = unique(retailer_type_sales[cumshare<=0.951, retailer_code])
trips[, top_99_retailer:=as.integer(retailer_code %in% coffee_retailer_99)]
trips[, top_95_retailer:=as.integer(retailer_code %in% coffee_retailer_95)]
hh_retailers = purchases[, .(npurch = .N), by = c("household_code", "retailer_code")]
setkeyv(trips, c("household_code", "retailer_code"))
setkeyv(hh_retailers, c("household_code", "retailer_code"))
trips = hh_retailers[trips]
trips[is.na(npurch), npurch:=0]
trips = trips[as.integer(top_95_retailer)==1 | as.integer(npurch)>=1, ]
trips[, ctrip := as.integer(as.integer(npurch)>=2 | as.integer(top_95_retailer)==1 | as.integer(coffee_trip)==1)]

# Obtain the trip code and retailer code in the trip to match with shopping environment later
trips[, `:=`(retailer_trips=as.integer(sum(coffee_trip)), ctrip = as.integer(sum(ctrip))), by = c("household_code", "week_end")]
trips[as.integer(retailer_trips)==0, `:=`(retailer_trips = ifelse(as.integer(ctrip)==0, as.integer(0), as.integer(1)),
                              retailer_filter = as.integer(npurch==max(npurch))), by = c("household_code", "week_end")]
trips[is.na(retailer_filter), retailer_filter:=0]

# Initialize the purchases panel.
hh_panel = hh_list[, .(household_code)]
hh_panel[, dummy:=1]
panel_year_week = unique(purchases[, .(panel_year, week_end)])
panel_year_week[, dummy:=1]
setkey(panel_year_week, dummy)
setkey(hh_panel, dummy)
hh_panel = hh_panel[panel_year_week, allow.cartesian=TRUE]
hh_panel[, dummy:=NULL]

# Obtain the existence of consumers in the panel
hh_panel_year = unique(hh[, .(household_code, panel_year, dma_code)])
setkeyv(hh_panel, c("household_code", "panel_year"))
setkeyv(hh_panel_year, c("household_code", "panel_year"))
# Only the years the consumers are making purchases matters.
hh_panel = hh_panel[hh_panel_year, nomatch=0L]
setkeyv(hh_panel, c("household_code", "week_end"))

# In case of multiple trips made in the week
# I use the stores the consumer made the most frequent purchases in case of no purchases
# Or the stores the consumer made the coffee purchases.
# Otherwise, I will be over stating the importance of Inventory in gauging non-purchases.
week_trips = trips[, .(retailer_trips=retailer_trips[1], coffee_trip=as.integer(sum(coffee_trip)), 
                       ctrip = ctrip[1]), by = c("household_code", "week_end")]
week_trips[as.integer(retailer_trips) == 0, retailer_trips:=1]
week_trips = week_trips[ctrip>=1, ]

# Merge in to the consumer panel to determine the number of trips to count for that trip
setkey(week_trips, household_code, week_end)
setkey(hh_panel, household_code, week_end)
hh_panel = week_trips[hh_panel]
hh_panel[is.na(retailer_trips), `:=`(retailer_trips=0, coffee_trip=0, ctrip=0)]

# Split hh_panel and replicate the multiple purchase case.
hh_panel_temp = hh_panel[retailer_trips>=2, ]
hh_panel_temp <- hh_panel_temp[rep(1:nrow(hh_panel_temp), retailer_trips), ]
hh_panel = rbindlist(list(hh_panel[retailer_trips<=1, ], hh_panel_temp))
hh_panel[, norder := 1:.N, by = c("household_code", "week_end")]
setkeyv(hh_panel, c("household_code", "week_end", "norder"))

# Merge trip information to the household panel
hh_retailer_trips = trips[(as.integer(retailer_trips)>=1) & (as.integer(retailer_filter)==1 | as.integer(coffee_trip)==1), ]
hh_retailer_trips[, retailer_filter:=1:.N, by = c("household_code", "week_end")]
hh_retailer_trips[as.integer(coffee_trip)==0, retailer_filter:=as.integer(retailer_filter==max(retailer_filter)),
                  by = c("household_code", "week_end")]
hh_retailer_trips = hh_retailer_trips[as.integer(coffee_trip)==1 | as.integer(retailer_filter)==1, ]
hh_retailer_trips[, norder:=1:.N, by = c("household_code", "week_end")]
hh_retailer_trips[, `:=`(retailer_trips=NULL, coffee_trip=NULL, panel_year=NULL,
                         top_99_retailer=NULL, top_95_retailer=NULL, ctrip=NULL,
                         retailer_filter=NULL, store_code_uc=NULL, store_zip3=NULL,
                         total_spent=NULL, npurch=NULL, channel_type=NULL)]

# Merge the hh retailer trips with hh panel 
setkeyv(hh_retailer_trips, c("household_code", "week_end", "norder"))
setkeyv(hh_panel, c("household_code", "week_end", "norder"))
hh_panel = hh_retailer_trips[hh_panel]

#---------------------------------------------------------------------------------------------------#
# Now impute the state dependence variables - last purchase and cumulative purchase without interruption
# Run this in parallel

# Obtain the relevant purchases
focal_purch = purchases[as.integer(selected)==1 | household_code %in% hh_list[, household_code], ]

# Aggregate purchases to household, week_end, purchase date, retailer, brand and size level. 
focal_purch = focal_purch[, .(quantity = sum(quantity), brand_descr = brand_descr[1], 
                              total_price_paid = sum(total_price_paid),
                              coupon_value = sum(coupon_value),
                              total_spent = total_spent[1],
                              panel_year = panel_year[1]), 
                          by = c("household_code", "dma_code", "week_end", "purchase_date", 
                                 "trip_code_uc", "retailer_code", "brand_descr_orig", "ptype", "keurig", "size1_amount")]

# Deal with CTL BR at different retailers.
focal_purch[, brand_descr_modified := brand_descr_orig] # Deal with CTL BR at different retailers.
focal_purch[brand_descr_orig == "CTL BR", brand_descr_modified := paste(brand_descr_orig, retailer_code, sep="_")] 

# Set the key for focal purchases to obtain the correct ordering
setkeyv(focal_purch, c("household_code", "week_end", "purchase_date", "trip_code_uc", "retailer_code"))
state_func <- function(hh_code){
  hpurch = focal_purch[.(hh_code), ]
  nobs = nrow(hpurch)
  ptype_lag = rep(as.character(NA), nobs)
  brand_lag = rep(as.character(NA), nobs)
  brand_cum = rep(0, nobs)
  date_diff = 0
  for (i in 1:nobs){
    if (i==1 | date_diff>=367) {
      ptype_lag[i]=as.character(NA)
      brand_lag[i]=as.character(NA)
      brand_cum[i]=0
      ptype_state = hpurch[i,]$ptype
      brand_state = hpurch[i,]$brand_descr_modified
      trip_state = hpurch[i,]$trip_code_uc
      cum_state = hpurch[i, quantity*size1_amount]
    } else{
      cptype_state = hpurch[i,]$ptype
      cbrand_state = hpurch[i,]$brand_descr_modified
      ctrip_state = hpurch[i,]$trip_code_uc
      if (ctrip_state != trip_state){
        ptype_lag[i] = ptype_state
        brand_lag[i] = brand_state
        brand_cum[i] = cum_state
        if (brand_state == cbrand_state & ptype_state == cptype_state){
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
        if (brand_state == cbrand_state & ptype_state == cptype_state){
          cum_state = cum_state + hpurch[i, quantity*size1_amount]
        } else{
          cum_state = hpurch[i, quantity*size1_amount]
        }
        ptype_state = cptype_state
        brand_state = cbrand_state
      }
    }
    if (i < nobs) date_diff = as.integer(hpurch[i+1, purchase_date] - hpurch[i, purchase_date])
  }
  return(data.table(ptype_lag = ptype_lag, brand_lag = brand_lag, brand_cum = brand_cum))
}

# Apply the function in parallel to each household
hh_codes = unique(focal_purch$household_code)
clusterExport(cl, c('focal_purch', 'state_func'))
state_list = parLapply(cl, hh_codes, state_func)
state_list = rbindlist(state_list)
focal_purch = cbind(focal_purch, state_list)
rm(state_list)

# Clean up the states by collapse them to household, week_end, retailer level observations. 
focal_purch_trip = focal_purch[, .(is_lag = as.integer(sum(brand_descr_modified==brand_lag)),
                                   brand_lag = brand_lag[1], brand_cum = brand_cum[1], 
                                   ptype_lag = ptype_lag[1], tot_units = sum(quantity * size1_amount)),
                               by =  c("trip_code_uc")]

# Roll down to the end of the year in case of last purchases 
# -- first obtain the last purchase
focal_purch[, plast := 1:.N, by = "household_code"]
focal_purch[, plast := as.integer(plast==max(plast)), by = "household_code"]
focal_purch_last = focal_purch[as.integer(plast)==1, ]
# Obtain the last dates by panel_year
year_last_week = focal_purch[, .(last_week_end = max(week_end)), by = "panel_year"]
setkey(year_last_week, panel_year)
setkey(focal_purch_last, panel_year)
focal_purch_last = year_last_week[focal_purch_last]
# If last purchase week coincide with the last week of the year, drop them (they are in the original purchase data)
focal_purch_last = focal_purch_last[week_end!=last_week_end]
focal_purch_last[, week_end:=last_week_end]
# Generate the states
focal_purch_last[brand_lag==brand_descr_modified & ptype_lag==ptype, brand_cum := brand_cum + quantity * size1_amount]
focal_purch_last[brand_lag!=brand_descr_modified | ptype_lag!=ptype, brand_cum := quantity * size1_amount]
focal_purch_last[, `:=`(brand_lag = brand_descr_modified, ptype_lag = ptype, plast=NULL)]
focal_purch_last = focal_purch_last[, .(household_code, week_end, ptype_lag, brand_lag, brand_cum, last_week_end)]
focal_purch_last[, is_lag := as.integer(0)]

# Merge these states to the household panel
cnames = union(names(hh_panel), names(focal_purch_trip))
setkey(hh_panel, trip_code_uc)
setkey(focal_purch_trip, trip_code_uc)
hh_panel = focal_purch_trip[hh_panel]
setcolorder(hh_panel, cnames)
setkeyv(hh_panel, c("household_code", "week_end", "norder"))

# Merge last week states to the household panel
setkeyv(hh_panel, c("household_code", "week_end"))
setkeyv(focal_purch_last, c("household_code", "week_end"))
hh_panel = focal_purch_last[hh_panel]
hh_panel[week_end==last_week_end, `:=`(i.is_lag = is_lag, i.brand_lag = brand_lag, 
                                       i.brand_cum = brand_cum, i.ptype_lag = ptype_lag)]
hh_panel[, `:=`(is_lag=NULL, brand_lag=NULL, brand_cum=NULL, ptype_lag=NULL, last_week_end=NULL)] 
setnames(hh_panel, c("i.is_lag", "i.brand_lag", "i.brand_cum", "i.ptype_lag"),
         c("is_lag", "brand_lag", "brand_cum", "ptype_lag"))
setcolorder(hh_panel, cnames)

# Roll up to create the states when nothing is purchased
hh_panel_temp = hh_panel[as.integer(norder)==1 & (!is.na(is_lag) | as.integer(coffee_trip)==1), 
                         .(household_code, norder, week_end, brand_lag, brand_cum, ptype_lag)]
setnames(hh_panel_temp, c("brand_lag", "brand_cum", "ptype_lag"),
         c("brand_lag_temp", "brand_cum_temp", "ptype_lag_temp"))
setkeyv(hh_panel, c("household_code", "norder", "week_end"))
setkeyv(hh_panel_temp, c("household_code", "norder", "week_end"))
hh_panel = hh_panel_temp[hh_panel, roll=-Inf, rollends=c(TRUE, FALSE)]
hh_panel[is.na(is_lag), `:=`(brand_lag = brand_lag_temp, brand_cum = brand_cum_temp, ptype_lag = ptype_lag_temp)]
hh_panel[,`:=`(brand_lag_temp=NULL, brand_cum_temp=NULL, ptype_lag_temp=NULL)]

# Replace missing is_lag and tot_units to zero
hh_panel[is.na(is_lag), is_lag:=0]
hh_panel[is.na(tot_units), tot_units:=0]

#---------------------------------------------------------------------------------------------------#
# Impute the inventory
hh_inventory = hh_panel[, .(tot_units=sum(tot_units), coffee_trip=sum(coffee_trip),
                            ptype_lag = ptype_lag[1], brand_lag = brand_lag[1]), 
                        by = c("household_code", "week_end", "panel_year")]
# Record the first purchase in the panel_year
max_week_end = max(hh_inventory[, week_end])
hh_inventory[coffee_trip>=1, first_panel_purch_week := week_end]
hh_inventory[is.na(first_panel_purch_week), first_panel_purch_week := max_week_end]
hh_inventory[, first_panel_purch_week := min(first_panel_purch_week), by = c("household_code", "panel_year")]

# Get the average consumption rate by gound coffee and Keurig-Kcups
hh_rate = purchases[, .(quantity = sum(quantity * size1_amount)), by = .(household_code, purchase_date, ptype)]
setkeyv(hh_rate, c("household_code", "purchase_date", "ptype"))
hh_rate[, norder:=1:.N, by = c("household_code", "purchase_date")] # Ignore cases where multiple purchases are made in the same day.
hh_rate = hh_rate[as.integer(norder)==1, ]
hh_rate[, norder:=.N, by = c("household_code")] # Ignore households with one purchase
hh_rate[, next_purch_date := c(purchase_date[2:length(ptype)], as.Date(NA)), by = c("household_code")]
hh_rate[, `:=`(periods=as.integer(next_purch_date-purchase_date)/7)]
# Ignore the ones that are over a year... Probably measurement error, or the household doesn't exist in that period
hh_rate = hh_rate[periods<=26, .(quantity = sum(quantity), periods = sum(periods), cases = .N),
                  by = c("household_code", "ptype")]
hh_rate[, crate := quantity/periods]
hh_rate = hh_rate[cases>=3, ] # Not reliable if less than 3.
setnames(hh_rate, "ptype", "ptype_lag")
setkey(hh_rate, household_code, ptype_lag)

# Merge in consumption rate, adoption date, and other information
setkey(hh_inventory, household_code)
setkey(hh_list, household_code)
hh_inventory = hh_inventory[hh_list[, .(household_code, k_first_week_end, brate, arate, overall_rate)], nomatch=0L]
hh_inventory[is.na(ptype_lag) & week_end>=k_first_week_end, ptype_lag:="KEURIG"]
hh_inventory[is.na(ptype_lag) & is.na(k_first_week_end), ptype_lag:="OTHER"]
hh_inventory[is.na(ptype_lag) & week_end<k_first_week_end, ptype_lag:="OTHER"]
setkey(hh_rate, household_code, ptype_lag)
setkey(hh_inventory, household_code, ptype_lag)
hh_inventory = hh_rate[,.(household_code, ptype_lag, crate)][hh_inventory]
hh_inventory[is.na(crate) & week_end>=k_first_week_end, crate:=arate]
hh_inventory[is.na(crate) & is.na(k_first_week_end), crate:=brate]
hh_inventory[is.na(crate) & week_end<k_first_week_end, crate:=brate]
hh_inventory[is.na(crate) & !is.na(arate), `:=`(crate=arate, ptype_lag="KEURIG")] # First recorded purchase is a keurig, and flagged as keurig rate! 

# Create lag periods for testing absence from the panel
setkeyv(hh_inventory, c("household_code", "week_end"))
hh_inventory[, `:=`(week_end_lag = c(as.Date(NA), week_end[1:(length(week_end)-1)])), by = "household_code"]
hh_inventory[, `:=`(lag_periods = as.integer(week_end - week_end_lag))]

inventory_func <- function(hh_code){
  hpurch = hh_inventory[.(hh_code), ]
  nobs = nrow(hpurch)
  inv_all = rep(NA, nobs)
  inv_type = rep(NA, nobs)
  inv_brand_type = rep(NA, nobs)
  date_diff = 0
  for (i in 1:nobs){
    if (i==1 | date_diff>=52) {
      if (date_diff > 26){
        inv_all[i] = 0
        inv_type[i] = 0
        inv_brand_type[i] = 0
      } else{
        inv_all[i] = hpurch[i, crate] * (as.numeric((hpurch[i, first_panel_purch_week] - hpurch[i, week_end]))/7)
        inv_type[i] = hpurch[i, crate] * (as.numeric((hpurch[i, first_panel_purch_week] - hpurch[i, week_end]))/7)
        inv_brand_type[i] = hpurch[i, crate] * (as.numeric((hpurch[i, first_panel_purch_week] - hpurch[i, week_end]))/7)
      }
    } else{
      inv_all[i] = max(inv_all[i-1] - hpurch[i, crate] +  hpurch[i-1, tot_units], 0)
      if (is.na(hpurch[i-1, brand_lag]) | is.na(hpurch[i, brand_lag])){
        inv_type[i] = max(inv_type[i-1] - hpurch[i, crate] +  hpurch[i-1, tot_units], 0)
        inv_brand_type[i] = max(inv_brand_type[i-1] - hpurch[i, crate] +  hpurch[i-1, tot_units], 0)
      } else{
        if (hpurch[i-1, ptype_lag]==hpurch[i, ptype_lag]){
          inv_type[i] = max(inv_type[i-1] - hpurch[i, crate] + hpurch[i-1, tot_units], 0)
        } else{
          inv_type[i] = max(hpurch[i-1, tot_units] - hpurch[i, crate], 0)
        }
        if (hpurch[i-1, brand_lag]==hpurch[i, brand_lag] & hpurch[i-1, ptype_lag]==hpurch[i, ptype_lag]){
          inv_brand_type[i] = max(inv_brand_type[i-1] - hpurch[i, crate] + hpurch[i-1, tot_units], 0)
        } else{
          inv_brand_type[i] = max(hpurch[i-1, tot_units] - hpurch[i, crate], 0)
        }
      }
    }
    if (i < nobs) date_diff = (as.integer(hpurch[i+1, week_end] - hpurch[i, week_end]))/7
  }
  return(data.table(inv_all = inv_all, inv_type = inv_type, inv_brand_type = inv_brand_type))
}
hh_codes = unique(hh_inventory$household_code)
clusterExport(cl, c('hh_inventory', 'inventory_func'))
inventory_list = parLapply(cl, hh_codes, inventory_func)
inventory_list = rbindlist(inventory_list)
hh_inventory = cbind(hh_inventory, inventory_list)
hh_inventory = hh_inventory[, .(household_code, week_end, inv_all, inv_type, inv_brand_type, crate)]

# Merge inventory data back into the household panel
setkeyv(hh_inventory, c("household_code", "week_end"))
setkeyv(hh_panel, c("household_code", "week_end"))
hh_panel = hh_panel[hh_inventory, nomatch=0L]

# Merge in machine holding status in this panel
setkey(hh_panel, household_code)
onames = c(names(hh_panel), "k_first_week_end")
hh_panel = hh_list[, .(household_code, k_first_week_end)][hh_panel]
hh_panel[, kholding:= as.integer(week_end>=k_first_week_end)]
hh_panel[is.na(k_first_week_end), kholding:=0]

setkeyv(hh_panel, c("household_code", "week_end", "norder"))
save(hh_panel, file = paste(output_dir, "/hh_trip_panel.RData", sep=""))

#---------------------------------------------------------------------------------------------------#
# Shopping environment not as clear in 2008, and use it as burnin period. 
hh_panel = hh_panel[panel_year!=2008, ]
hh_panel[!(brand_lag %in% selected_brand_list) & !grepl("CTL BR", brand_lag) & !is.na(brand_lag), brand_lag:="OTHER"]

# Get rid of observations where the last brand is not observed -- not dealing with initial condition for now.
# - less than 12% of data
hh_panel = hh_panel[!is.na(brand_lag), ]

# Get rid of observations where the inventory doesn't seem to make sense --
# e.g. 600 units of inventory (600 kcups or 18.75 pound of coffee), or 100 times of consumption rate (2 years). 
hh_panel = hh_panel[!(inv_type>600 | crate<=inv_type/100), ]

# Get rid of variables not needed for now.
hh_panel = hh_panel[, .(dma_code, retailer_code, household_code, kholding, week_end, norder, trip_code_uc, 
                        coffee_trip, is_lag, brand_lag, brand_cum, ptype_lag, inv_all, inv_type, inv_brand_type)]

# Now use the trip data to create purchase panel for mixed logit demand estimation
setkeyv(hh_panel, c("dma_code", "retailer_code", "week_end"))
setkeyv(retailer_panel, c("dma_code", "retailer_code", "week_end"))
hh_prod_panel = hh_panel[retailer_panel, allow.cartesian=TRUE, nomatch=0L]

# Get rid of Keurig products when the consumer doesn't hold keurig machine
hh_prod_panel = hh_prod_panel[as.integer(kholding)==1 | (as.integer(kholding) == 0 & as.integer(keurig) == 0), ]

# Merge in the purchase data to locate the purchased product
setkey(focal_purch, trip_code_uc, brand_descr, keurig, size1_amount) 
setkey(hh_prod_panel, trip_code_uc, brand_descr, keurig, size1_amount) 
hh_prod_panel = focal_purch[, .(trip_code_uc, brand_descr, keurig, size1_amount, quantity, total_price_paid, coupon_value, total_spent)][hh_prod_panel]
hh_prod_panel[is.na(quantity), quantity := 0]
# Treat purchases of multiple packs as purchase of a single large pack, consider consumer coupon values
hh_prod_panel[quantity>=2, size1_amount:=quantity*size1_amount]
hh_prod_panel[quantity>=2, price:=total_price_paid-coupon_value]
hh_prod_panel[, sum_quantity:=sum(quantity), by ="trip_code_uc"]
hh_prod_panel[as.integer(sum_quantity)==0&brand_descr=="0NOTHING", quantity:=1]

# Still, for some shopping occasions, the consumer could be choosing from over 90 alternatives, which is not realistic.
# So, I make some assumptions about consumer's consideration set. 
# I define the product to be in the consideration set if the brand size combinations is in the top 80% in the market (dma+keurig)
# Or the brand size combintation is in the top 95% of the consumer's consumption set
# Then, product is retained if it meets either of the above criteria or chosen at the choice occasion. 
brand_size_type_sales = purchases[, .(revenue = sum(total_price_paid-coupon_value)), 
                                  by = c("dma_code", "keurig", "brand_descr", "size1_amount")]
brand_size_type_sales[, rshare:=revenue/sum(revenue), by=c("dma_code", "keurig")]
brand_size_type_sales = brand_size_type_sales[order(-rshare), ]
setkeyv(brand_size_type_sales, c("dma_code", "keurig"))
brand_size_type_sales[, rcumshare:=cumsum(rshare), by=c("dma_code", "keurig")]
brand_size_type_sales[, rcumshare:=rcumshare-rshare]
brand_size_type_sales = brand_size_type_sales[rcumshare<=0.8, .(dma_code, keurig, brand_descr, size1_amount, rshare)]
hh_brand_size_sales = purchases[, .(revenue = sum(total_price_paid-coupon_value)), 
                                by = c("household_code", "keurig", "brand_descr", "size1_amount")]
hh_brand_size_sales[, rshare:=revenue/sum(revenue), by=c("household_code", "keurig")]
hh_brand_size_sales = hh_brand_size_sales[order(-rshare), ]
setkeyv(hh_brand_size_sales, c("household_code", "keurig"))
hh_brand_size_sales[, rcumshare:=cumsum(rshare), by=c("household_code", "keurig")]
hh_brand_size_sales[, rcumshare:=rcumshare-rshare]
hh_brand_size_sales = hh_brand_size_sales[rcumshare<=0.95, .(household_code, keurig, brand_descr, size1_amount, rshare)]

# Merge the criteria into the hh_prod_panel
onames = names(hh_prod_panel)
setkeyv(brand_size_type_sales, c("dma_code", "keurig", "brand_descr", "size1_amount"))
setkeyv(hh_prod_panel, c("dma_code", "keurig", "brand_descr", "size1_amount"))
hh_prod_panel = brand_size_type_sales[hh_prod_panel]
setkeyv(hh_brand_size_sales, c("household_code", "keurig", "brand_descr", "size1_amount"))
setkeyv(hh_prod_panel, c("household_code", "keurig", "brand_descr", "size1_amount"))
hh_prod_panel = hh_brand_size_sales[hh_prod_panel]
hh_prod_panel = hh_prod_panel[!is.na(rshare) | !is.na(i.rshare) | as.integer(quantity)>=1 | brand_descr=="0NOTHING", ]
hh_prod_panel[,`:=`(rshare=NULL, i.rshare=NULL)]
setcolorder(hh_prod_panel, onames)
setkeyv(hh_prod_panel, c("household_code", "week_end", "norder", "brand_descr"))

# Modify brand_descr to take into account private label issues
hh_prod_panel[brand_descr=="CTL BR", brand_descr_modified := paste(brand_descr, retailer_code, sep="_")]
hh_prod_panel[is.na(brand_descr_modified), brand_descr_modified := brand_descr]
hh_prod_panel[brand_lag==brand_descr_modified & ptype_lag==ptype, `:=`(is_lag = 1)]
hh_prod_panel[brand_lag!=brand_descr_modified | ptype_lag!=ptype, `:=`(is_lag = 0, brand_cum = 0)]

# Hassle cost of purchase to rationalize nonpurchase as well
hh_prod_panel[, `:=`(hassle = as.integer(brand_descr!="0NOTHING"))]

# Input the number of periods of consumption, and impute the new inventory...
# Method 1 - Inventory as a way to gauge purchases only! 
hh_prod_panel[, keurig_lag := as.integer(ptype_lag=="KEURIG")]
hh_prod_panel[, `:=`(sinventory = size1_amount)]
hh_prod_panel[brand_descr=="0NOTHING", `:=`(sinventory = inv_type)]
hh_prod_panel[brand_descr!="0NOTHING", `:=`(inv_type=0)]
hh_prod_panel[, kinventory := keurig_lag * inv_type]
hh_prod_panel[, `:=`(brand_descr_modified=NULL)]

# Obtain size and inventory.
hh_prod_panel[grepl("CTL BR", brand_lag), brand_lag:="CTL BR"]
hh_prod_panel[, `:=`(brand_descr_c = ifelse(brand_descr=="0NOTHING" & sinventory>=1, brand_lag, brand_descr),
                     keurig_c = ifelse(brand_descr=="0NOTHING" & sinventory>=1, as.integer(ptype_lag=="KEURIG"), keurig))]
hh_prod_panel[as.integer(keurig_c)==1 & brand_descr=="0NOTHING", ptype := "KEURIG"]
hh_prod_panel[as.integer(keurig_c)==0 & ptype_lag=="SENSEO" & brand_descr=="0NOTHING", ptype := "SENSEO"]
hh_prod_panel[as.integer(keurig_c)==0 & ptype_lag=="GUSTO" & brand_descr=="0NOTHING", ptype := "GUSTO"]
hh_prod_panel[as.integer(keurig_c)==0 & ptype_lag=="TASSIMO" & brand_descr=="0NOTHING", ptype := "TASSIMO"]
hh_prod_panel[as.integer(keurig_c)==0 & ptype_lag=="VUE" & brand_descr=="0NOTHING", ptype := "VUE"]

# Merge in consumption rate by household and product type.
hh_rate[, ptype:=ptype_lag]
setkey(hh_prod_panel, household_code, ptype)
setkey(hh_rate, household_code, ptype)
hh_prod_panel = hh_rate[,.(household_code, ptype, crate)][hh_prod_panel]
# Input the before/after rate in case of missing.
hh_prod_panel = hh_list[, .(household_code, brate, arate, k_first_week_end)][hh_prod_panel]
hh_prod_panel[is.na(crate) & ptype=="KEURIG", crate := arate]
hh_prod_panel[is.na(crate) & ptype!="KEURIG", crate := brate]
hh_prod_panel[is.na(crate), crate := arate] # Never observe purchasing ground!
hh_prod_panel[is.na(crate), crate := brate] # Purchased Keurig only once/twice!
hh_prod_panel[, `:=`(arate=NULL, brate=NULL, k_first_week_end=NULL)]

# Given the inventory and product size - compute the relevant cofficients
dbeta = 0.995
hh_prod_panel[, Tj:=sinventory/crate]
hh_prod_panel[, `:=`(coef1 = (1 - dbeta^Tj)/(1 - dbeta), 
                     coef2 = sinventory * (1 - dbeta^Tj)/(1 - dbeta) + 
                       crate*((Tj * dbeta^Tj)/(1-dbeta) - (1 - dbeta^Tj)/(1 - dbeta)^2))]

# Get rid of households having less than 10 choice occassions (less than 1% of data, or 8% of the flagged households)
hh_prod_panel[, sum_quantity:=sum(quantity), by ="household_code"]
hh_prod_panel = hh_prod_panel[sum_quantity>=10, ]

# Save the panel
setkeyv(hh_prod_panel, c("household_code", "week_end", "norder", "trip_code_uc", "brand_descr", "keurig", "size1_amount"))
save(hh_prod_panel, file = paste(output_dir, "/hh_product_panel.RData", sep=""))

#---------------------------------------------------------------------------------------------------#
# Now only take New York data for our estimation purpose. This is because we have too much data...
# It's computationally infeasible to estimate such a model.
dma_hh_list = unique(hh_prod_panel[as.integer(dma_code) %in% top_dma_list, household_code])
hh_market_prod = hh_prod_panel[household_code %in% dma_hh_list, ]
hh_market_prod[,`:=`(retailer_code=NULL, coffee_trip=NULL, brand_lag=NULL, 
                     keurig=NULL, ptype_lag=NULL, sum_quantity=NULL, inventory = inv_type)]
hh_market_prod[brand_descr=="0NOTHING", `:=`(brand_descr_c="0NOTHING", keurig_c=0)] # Trip - quantity choice

# Drop trips with multiple purchases - not discrete choice
#hh_market_prod[, npurch:=sum(as.integer(quantity>=1)), by = "trip_code_uc"]
#hh_market_prod = hh_market_prod[as.integer(npurch)==1, ]

# Drop trips with very high expenditure on coffee over 100.
hh_market_prod[, total_price_paid:=sum(total_price_paid, na.rm=TRUE), by = "trip_code_uc"]
hh_market_prod = hh_market_prod[total_price_paid<=100, ]

# Modify the brand to create idiosyncratic match values for top Keurig Brands
hh_market_prod[, brand_descr_temp:=brand_descr_c]
hh_market_prod[as.integer(keurig_c)==1 & brand_descr_temp %in% top_keurig_brands, 
               brand_descr_c := paste(brand_descr_c, "KEURIG")]
hh_market_prod[, brand_descr:=brand_descr_c]

# Flag brands not often purchased, and put them in the other category.
bpurch = hh_market_prod[quantity>=1, .(nb=.N), by = "brand_descr_c"]
top_brands = bpurch[(nb>=4000 & grepl("KEURIG", brand_descr_c)) | (nb>=10000) , brand_descr_c]

# Given the numerous brands, it's not feasible to estimate the brand intercepts for all brands.
# I constraint small brands to have the same brand intercept.
hh_market_prod[(!(brand_descr_c %in% top_brands)) & brand_descr_c!="0NOTHING", brand_descr_c:="OTHER"]
hh_market_prod[brand_descr_c=="OTHER", brand_descr_c:="0OTHER"]

# Only keep the occassions with purchases - Conditional on purchasing coffee
hh_market_prod = hh_market_prod[brand_descr != "0NOTHING", ]
hh_market_prod[, npurch:=sum(as.integer(quantity>=1)), by = "trip_code_uc"]
hh_market_prod = hh_market_prod[as.integer(npurch)==1, ]

# Filter Households with less than 3 Purchases
hh_market_prod[, npurch:=sum(as.integer(quantity>=1)), by = "household_code"]
hh_market_prod = hh_market_prod[as.integer(npurch)>=3, ]
hh_market_prod[, `:=`(total_spent = mean(total_spent, na.rm=TRUE)), by = "trip_code_uc"]

# Create new household id, and try to get the brand code etc...
setkey(hh_market_prod, household_code)
hh_market_prod[, hh := .GRP, by = "household_code"]
setkey(hh_market_prod, brand_descr_c)
hh_market_prod[, brand := .GRP, by = "brand_descr_c"]
setkeyv(hh_market_prod, c("hh", "week_end", "norder", "brand_descr", "keurig_c", "size1_amount"))

# Obtain the list of corresponding household demographics
hh_household_code = unique(hh_market_prod[, .(household_code, hh)])
setkey(hh_household_code, household_code)
hh_household_code[, one:=1]
# Obtain the demographic data at household level using the most recent information available.
hh_info_panel[, year_filter := as.integer(panel_year==max(panel_year)), by = "household_code"]
hh_info_panel = hh_info_panel[as.integer(year_filter)==1, ]
setkey(hh_info_panel, household_code)
hh_demo = hh_household_code[hh_info_panel[,.(household_code, dma_code, overall_rate, inc25, inc40, inc50, inc60, inc70,
                                             hhsize1, hhsize2, hhsize3, hhsize5, onefamily, twofamily,
                                             threefamily, fulltime, partime, notemployed, head_age_numeric,
                                             presence_of_children, african_american, hispanic, cable, internet)], nomatch=0L]
#hh_demo[, household_code:=NULL]
setkey(hh_demo, hh)

# Create the brand dummies - and prepare data
br_list = sort(hh_market_prod[, unique(brand)])
bvars = paste0("a", br_list)
dbt = data.table(model.matrix(~factor(hh_market_prod$brand)-1))
setnames(dbt, names(dbt), bvars)
hh_market_prod = cbind(hh_market_prod, dbt)
hh_market_prod[, t:=.GRP, by = "trip_code_uc"]
hh_market_prod[, `:=`(brand_lag = is_lag, purchased = as.integer(quantity>=1), keurig=keurig_c)]

# Make brand_lag different if the last purchase is K-Cup versus Ground Coffee
hh_market_prod[, `:=`(brand_lag_ground = brand_lag * (1-keurig),
                      brand_lag_keurig = brand_lag * keurig)]

# Scale price by 10, and inventory by 100
hh_market_prod[, `:=`(inventory = inventory/10, kinventory = kinventory/10, sinventory = sinventory/10,
                      coef1 = coef1/10, coef2 = pmax(coef2/100, 0), brand_cum_log = log(brand_cum+1), 
                      Tj = Tj/30, brand_cum = brand_cum/100)]
hh_market_prod[, `:=`(invsq = inventory^2, sinvsq = sinventory^2, Tjsq = Tj^2, brand_cum_sq = brand_cum^2)]
name_order = c("household_code", "trip_code_uc", "dma_code", "hh", "t", "brand_descr", "brand", bvars, "keurig", 
               "price", "size1_amount", "brand_lag", "brand_lag_ground", "brand_lag_keurig", "brand_cum", 
               "brand_cum_sq", "brand_cum_log", "total_spent", "purchased", "hassle")
hh_market_prod = hh_market_prod[, name_order, with = FALSE]

save(hh_market_prod, hh_demo, file = paste(output_dir,"/Coffee-Panel-Cond-Purchase.RData", sep=""))
stopxxx


# Use the Bvars and keurig to multiply the correct coefficients
bvars = c(bvars, "keurig")
for (v in bvars[2:length(bvars)]){
  setnames(hh_market_prod, v, "v")
  hh_market_prod[, v:=v*coef1]
  setnames(hh_market_prod, "v", v)
}

# Make panel balanced to facilitate computation
hh_market_prod[, product:=1:(.N), by = c("t")]
hh_julia = as.data.table(expand.grid(t = unique(hh_market_prod$t),
                                     product = 1:max(hh_market_prod$product)))
setkeyv(hh_market_prod, c("t", "product"))
setkeyv(hh_julia, c("t", "product"))
hh_julia = hh_market_prod[hh_julia]
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
write.csv(hh_julia, file = paste(output_dir,"/coffee_julia.csv", sep=""), row.names=FALSE)
write.csv(hh_demo, file = paste(output_dir,"/demo_julia.csv", sep=""), row.names=FALSE)

# Make a sample to facilitate initial guess  -- sample 20000 purchase occassions, about 10% of purchases
hh_market_prod_samp = hh_market_prod[t%in%sample(1:max(hh_market_prod$t), 20000), ]
setkey(hh_market_prod_samp, hh, t, brand)
hh_market_prod_samp[, t:=.GRP, by = "t"]

# Make panel balanced to facilitate computation
hh_market_prod_samp[, product:=1:(.N), by = c("t")]
hh_julia_samp = as.data.table(expand.grid(t = unique(hh_market_prod_samp$t),
                                          product = 1:max(hh_market_prod_samp$product)))
setkeyv(hh_market_prod_samp, c("t", "product"))
setkeyv(hh_julia_samp, c("t", "product"))
hh_julia_samp = hh_market_prod_samp[hh_julia_samp]
hh_julia_samp[, hh := as.integer(mean(hh, na.rm=TRUE)), by ="t"]
nlist = names(hh_julia_samp)
nlist = setdiff(nlist, c("hh","t","product"))
hh_julia_samp[, avail := as.numeric(!is.na(brand))]
for (v in nlist){
  setnames(hh_julia_samp, v, "vx")
  hh_julia_samp[is.na(vx), vx:=0]
  setnames(hh_julia_samp, "vx", v)
}

# Export/save data
write.csv(hh_julia_samp, file = paste(output_dir,"/coffee_julia_samp.csv", sep=""), row.names=FALSE)
