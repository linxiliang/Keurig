#---------------------------------------------------------------------------------------------------#
# Load appropriate data
# Load HH file with flags
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))

# Load HMS Purchase data
load(paste(HMS_input_dir, "/HMS-Purchases-Coffee.RData", sep=""))

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

# Make purchases trip, product unique
pnamelist = names(purchases)
pnamelist = setdiff(pnamelist, c("total_spent", "total_price_paid", "coupon_value", "deal_flag_uc","quantity"))
# correct purchase amount - obvious error
purchases[upc=="009955504002" & quantity == 24, quantity:=2] 
purchases[upc=="009955504002" & quantity == 12, quantity:=1]

# Make purchase trip-product unique
purchases = purchases[, .(total_price_paid = sum(total_price_paid),
                          coupon_value = sum(coupon_value),
                          quantity = sum(quantity)), by = pnamelist]

# Merge in the type information to the purchase data
setkeyv(purchases, c("upc", "upc_ver_uc"))
purchases=purchases[products[, .(upc, upc_ver_uc, upc_descr, ptype, brand_descr, size1_amount, multi, series)], nomatch=0L]
purchases[, month:=as.factor(format(purchase_date, '%Y-%m'))]

# Restrict purchases to specific software purchases
purchases = purchases[product_module_code %in% maker_modules, ]

#---------------------------------------------------------------------------------------------------#
# Focus on the platform of choice
hh_list = hh_list[ptype==platform, ]
hh_list[, ptype:=NULL]
purchases = purchases[ptype=="KEURIG"|ptype=="OTHER", ]

# Select only purchases from the selected households
setkeyv(purchases, c("household_code"))
setkey(hh_list, household_code)

# Select only purchases made by the selected households
purchases = purchases[hh_list[, .(household_code, sfirst_date, slast_date, dma_code)], nomatch=0L]

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

# Only consider products ever bought
prod_list = unique(purchases[ptype=="KEURIG"&brand_descr!="MR COFFEE", .(upc, upc_ver_uc)])

# ----------------------------------------------------------------------------------------------------#
# Obtain RMS Shopping Environment
rms_hw_prices = data.table(NULL)
for (mod in maker_modules){
  load(paste(RMS_input_dir, "/", mod, ".RData", sep=""))
  move[, `:=`(feature=NULL, display=NULL)]
  setkeyv(move, c("store_code_uc", "week_end"))
  rms_hw_prices = rbindlist(list(rms_hw_prices, move))
  rm(move)
  gc()
}

# Merge to movement data 
setkeyv(rms_hw_prices, c("upc", "upc_ver_uc"))
setkeyv(prod_list, c("upc", "upc_ver_uc"))
rms_hw_prices = rms_hw_prices[prod_list, nomatch=0L]

# Obtain the available products
setkeyv(rms_hw_prices, c("store_code_uc", "panel_year"))
setkeyv(stores, c("store_code_uc", "panel_year"))
rms_hw_prices = rms_hw_prices[stores[, .(store_code_uc, panel_year, dma_code, parent_code)], nomatch=0L]
rms_hw_prices = rms_hw_prices[, .(price = mean(price), units = sum(units*prmult)),
                              by = c("dma_code", "upc", "upc_ver_uc", "week_end")]
# ----------------------------------------------------------------------------------------------------#
# Use Homescan sales as weights to obtain a price index.
prod_sales = purchases[ptype=="KEURIG"&brand_descr!="MR COFFEE", .(revenue = sum(price_paid), np = .N),
                       by = c("upc", "upc_ver_uc", "upc_descr", "brand_descr", "series")]
prod_sales[, `:=`(rshare = revenue/sum(revenue), pshare = np/sum(np),
                  aprice = revenue/np)]

# To illustrate the type of data we have including market share, and the similarity of prices within a series.
prod_sales[order(-pshare), `:=`(crshare = cumsum(rshare), cpshare = cumsum(pshare))]
prod_sales[order(-pshare)]
prod_sales[order(series), .(upc, upc_ver_uc, upc_descr, series, aprice, revenue, np, pshare)]
prod_sales[, sum(pshare), by = c("series")]

# Merge product sales to rms_hw_prices create price index
setkeyv(rms_hw_prices, c("upc", "upc_ver_uc"))
setkeyv(prod_sales, c("upc", "upc_ver_uc"))
rms_hw_prices = rms_hw_prices[prod_sales[, .(upc, upc_ver_uc, series, pshare)], nomatch=0L]
rms_hw_prices = rms_hw_prices[, .(price = weighted.mean(price, pshare), units = sum(units)), by = c("series", "dma_code", "week_end")]

# Drop the VUE machines
rms_hw_prices = rms_hw_prices[!is.na(series)] 

# Save the rms price and sales data
save(rms_hw_prices, file = paste(output_dir, "/rms_hw.RData", sep=""))

# Plot some price path to visualize
setkeyv(rms_hw_prices, c("series", "dma_code", "week_end"))
rms_hw_prices[dma_code==501 & series==3, plot(week_end, price, type="l")]
rms_hw_prices[dma_code==602 & series==1, plot(week_end, price, type="l")]
rms_hw_prices[dma_code==602 & series==2, plot(week_end, price, type="l")]
rms_hw_prices[dma_code==602 & series==3, plot(week_end, price, type="l")]
rms_hw_prices[dma_code==602 & series==4, plot(week_end, price, type="l")]
rms_hw_prices[dma_code==602 & series==5, plot(week_end, price, type="l")]

rms_hw_prices[dma_code==501 & series==1, plot(week_end, price, type="l")]
rms_hw_prices[dma_code==501 & series==2, plot(week_end, price, type="l")]
rms_hw_prices[dma_code==501 & series==3, plot(week_end, price, type="l")]
rms_hw_prices[dma_code==501 & series==4, plot(week_end, price, type="l")]
rms_hw_prices[dma_code==501 & series==5, plot(week_end, price, type="l")]

# ----------------------------------------------------------------------------------------------------#
# Now create the hardware price panel for adoption problem. 
kpurch = purchases[ptype=="KEURIG"&brand_descr!="MR COFFEE", ]
kpurch[,  dma_code:=as.integer(as.character(dma_code))]

# Get rid of purchases of Vue machine
kpurch = kpurch[!is.na(series), ]

# Only use the first purchase - (not considering gifts or upgrading decision (unobserved))
setkeyv(kpurch, c("household_code", "purchase_date", "trip_code_uc"))
kpurch[, hporder := c(1 : .N), by = c("household_code")]
kpurch = kpurch[hporder==1, ]
kpurch[, hporder:=NULL]
kpurch[, `:=`(first_adoption_week = week_end,
              adopted_series = series)]

# Make the initial panel
hw_panel = data.table(expand.grid(household_code = unique(kpurch$household_code), 
                                  week_end = unique(rms_hw_prices$week_end),
                                  series = unique(rms_hw_prices$series)))
setkey(hw_panel, "household_code")
setkey(kpurch, "household_code")
hw_panel = hw_panel[kpurch[, .(household_code, dma_code, first_adoption_week, adopted_series)], nomatch=0L]
hw_panel = hw_panel[week_end<=first_adoption_week, ]

# Merge in DMA availability and prices
setkeyv(rms_hw_prices, c("dma_code", "week_end", "series"))
setkeyv(hw_panel, c("dma_code", "week_end", "series"))
hw_panel = hw_panel[rms_hw_prices, nomatch=0L]

# Flag adoption week, and flag adoption decision
hw_panel[, `:=`(adoption_week = as.integer(first_adoption_week == week_end),
                adopted = as.integer(first_adoption_week == week_end & adopted_series==series),
                adopted_series = NULL)]

# 
# so the refined adoption problem need to account for price of software and availability... !
# I may also need to run this at zip 3 level rather than national or other levels! 
# Shall I use aggregate level data for adoption? or individual level data? 


# Imputing adoption of series may need to be at consumer level. 


# The dataset I want would have
# household_code, time, brand purchase indicator, prices, availability, adoption status, 
# Flow Util of ground coffee (function of preference), Flow Util/W(1) (function of preference), 
# No of Brands, No of products, holidays indicator, cumulative adoption percentage... 

# Deal with projection factor? 
