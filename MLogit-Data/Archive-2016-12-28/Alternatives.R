#---------------------------------------------------------------------------------------------------#
# Consideration Set

# Still, for some shopping occasions, the consumer could be choosing from over 90 alternatives, which is not realistic.
# So, I make some assumptions about consumer's consideration set. 

# Method 1
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
hh_prod_panel = hh_prod_panel[!is.na(rshare) | !is.na(i.rshare) | quantity>=1 | brand_descr=="0NOTHING", ]
hh_prod_panel[,`:=`(rshare=NULL, i.rshare=NULL)]
setcolorder(hh_prod_panel, onames)

# Method 2
# Only brand and size combinations ever purchased is in the consideration set of the consumer...
hh_brand_size_sales = purchases[, .(revenue = sum(total_price_paid-coupon_value)), 
                                by = c("household_code", "keurig", "brand_descr", "size1_amount")]
onames = names(hh_prod_panel)
setkeyv(hh_brand_size_sales, c("household_code", "keurig", "brand_descr", "size1_amount"))
setkeyv(hh_prod_panel, c("household_code", "keurig", "brand_descr", "size1_amount"))
hh_prod_panel = hh_brand_size_sales[hh_prod_panel]
hh_prod_panel = hh_prod_panel[!is.na(revenue) | brand_descr=="0NOTHING", ]
hh_prod_panel[,`:=`(revenue=NULL)]
setcolorder(hh_prod_panel, onames)

#---------------------------------------------------------------------------------------------------#
# Inventory Switching Methods

# Method 2 - Inventory as a tax/benfit to product
hh_prod_panel[, keurig_lag := as.integer(ptype_lag=="KEURIG")]
hh_prod_panel[, `:=`(sinventory = size1_amount)]
hh_prod_panel[brand_descr=="0NOTHING", `:=`(sinventory=inv_type, inv_type=0)]
hh_prod_panel[, kinventory := keurig_lag * inv_type]
hh_prod_panel[, `:=`(brand_descr_modified=NULL)]

# Method 1 - Inventory as a way to gauge purchases only! 
hh_prod_panel[, keurig_lag := as.integer(ptype_lag=="KEURIG")]
hh_prod_panel[, `:=`(sinventory = size1_amount)]
hh_prod_panel[brand_descr!="0NOTHING", `:=`(inv_type=0)]
hh_prod_panel[, kinventory := keurig_lag * inv_type]
hh_prod_panel[, `:=`(brand_descr_modified=NULL)]

# Method 3 - Receive a scrape value for the inventory at hand... (can also pay a value to convert the inventory).
hh_prod_panel[, keurig_lag := as.integer(ptype_lag=="KEURIG")]
hh_prod_panel[brand_lag==brand_descr_modified & keurig_lag == keurig, 
              `:=`(sinventory = size1_amount + inv_type, inv_type = 0)]
hh_prod_panel[brand_lag!=brand_descr_modified & keurig_lag == keurig, 
              `:=`(sinventory = size1_amount + inv_type)]
hh_prod_panel[brand_lag!=brand_descr_modified & keurig_lag != keurig, 
              `:=`(sinventory = size1_amount)]
hh_prod_panel[brand_descr=="0NOTHING", `:=`(sinventory=inv_type, inv_type=0)]
hh_prod_panel[, kinventory := keurig_lag * inv_type]
hh_prod_panel[, inv_type := (1-keurig_lag) * inv_type]
hh_prod_panel[keurig_lag != keurig, `:=`(kinventory = 0, inv_type = 0)]
hh_prod_panel[, `:=`(brand_descr_modified=NULL)]

unique(hh_market_prod[,.(brand, brand_descr_c)])[order(brand)]

#---------------------------------------------------------------------------------------------------#
# Constrain trips to coffee related trips! 

# Method 1: Drop trips where no related purchases are made...
setkey(coffee_related_trips, trip_code_uc)
setkey(hh_market_prod, trip_code_uc)
hh_market_prod = coffee_related_trips[hh_market_prod, nomatch=0L]

#---------------------------------------------------------------------------------------------------#
# Constrain Inertia/brand to be within brand and type
hh_prod_panel[brand_lag==brand_descr_modified & ptype_lag==ptype, `:=`(is_lag = 1)]
hh_prod_panel[brand_lag!=brand_descr_modified | ptype_lag!=ptype, `:=`(is_lag = 0, brand_cum = 0)]

#---------------------------------------------------------------------------------------------------#
# Constrain Inertia/brand to be within brand and type
# Per Unit Price and put size and size squared as covariates, and hassle cost... 
hh_market_prod[brand!=1, `:=`(price = price/sinventory)]

#---------------------------------------------------------------------------------------------------#
# Drop trips with multiple purchases - not discrete choice
hh_market_prod[, npurch:=sum(as.integer(quantity>=1)), by = "trip_code_uc"]
hh_market_prod = hh_market_prod[as.integer(npurch)==1, ]

# Or Drop trips with purchases of more than 1 unit! 
# Drop trips with multiple purchases - not discrete choice
hh_market_prod[, npurch:=sum(quantity), by = "trip_code_uc"]
hh_market_prod = hh_market_prod[as.integer(npurch)==1, ]

# Only keep the occassions with purchases - Conditional on purchasing coffee
hh_market_prod = hh_market_prod[brand_descr != "0NOTHING", ]
hh_market_prod[, npurch:=sum(as.integer(quantity>=1)), by = "trip_code_uc"]
hh_market_prod = hh_market_prod[as.integer(npurch)==1, ]

# Filter Households with less than 3 Purchases
hh_market_prod[, npurch:=sum(as.integer(quantity>=1)), by = "household_code"]
hh_market_prod = hh_market_prod[as.integer(npurch)>=3, ]
hh_market_prod[, `:=`(total_spent = mean(total_spent, na.rm=TRUE)), by = "trip_code_uc"]

# Filter Households with less than 10 weeks
hh_market_prod[, npurch:=sum(as.integer(quantity>=1)), by = "household_code"]
hh_market_prod = hh_market_prod[as.integer(npurch)>=10, ]
hh_market_prod[, `:=`(total_spent = mean(total_spent, na.rm=TRUE)), by = "trip_code_uc"]

#---------------------------------------------------------------------------------------------------#
# For each brand, only keep the pack sizes that are closest to the chosen size
# In cases where no purchase is made, only select the brand size that is closest to the median size purchased. 
hh_market_prod[brand_descr != "0NOTHING" & quantity>=1, 
               `:=`(purchased_size = size1_amount, purchased_quantity = quantity)]
hh_market_prod[, `:=`(purchased_size = na.omit(purchased_size), purchased_quantity = na.omit(purchased_quantity)),
               by = c("trip_code_uc")]
hh_market_prod[purchased_quantity>=1, `:=`(size_diff = abs(size1_amount - purchased_size))]
hh_market_prod[purchased_quantity>=1, `:=`(sfilter = as.integer(size_diff == min(size_diff))), 
               by = c("trip_code_uc", "brand_descr", "keurig_c")]
hh_pack_size = purchases[, .(msize = median(size1_amount, na.rm=TRUE)), by = "household_code"]
setkey(hh_pack_size, household_code)
setkey(hh_market_prod, household_code)
hh_market_prod = hh_pack_size[hh_market_prod]
hh_market_prod[is.na(purchased_quantity), `:=`(size_diff = abs(size1_amount-msize))]
hh_market_prod[is.na(purchased_quantity), `:=`(sfilter = as.integer(size_diff==min(size_diff))), 
               by = c("trip_code_uc", "brand_descr", "keurig_c")]
hh_market_prod = hh_market_prod[sfilter>=1 | brand_descr=="0NOTHING", ]

#---------------------------------------------------------------------------------------------------#
# The emax way --- very difficult to compute the compensating value.
# Compute the adoption value consumer by consumer
xvars = c(paste0("a", 2:17), "keurig", "size1_amount", "lag_ind", "incdiff")
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
  # Compute the mean utility of adoption
  hh_retailers_temp[, u := xmat %*% preferences[as.character(i), ]]
  hh_retailers_temp = hh_retailers_temp[, .(mu=max(u)), by = c("household_code", "week_end", 
                                                               "retailer_code", "brand_descr", "keurig", "tprob", "pprob")]
  hh_retailers_temp = hh_retailers_temp[, .(mu_all = log(sum(exp(mu))),
                                            mu_ground = log(sum(exp(mu) * (1-keurig)))), 
                                        by = c("household_code", "week_end", "retailer_code", "tprob", "pprob")]
  hh_retailers_temp[is.infinite(mu_ground), mu_ground:= preferences[as.character(i), 20] * log(TBudget[as.character(i)])] # If non-purchase receive the utility from budget
  hh_retailers_temp = hh_retailers_temp[, `:=`(mu_diff = mu_all - mu_ground)]
  hh_retailers_temp = hh_retailers_temp[, .(mu_all = sum(mu_all * tprob)/sum(tprob),
                                            mu_ground = sum(mu_ground * tprob)/sum(tprob),
                                            mu_diff = sum(mu_diff * tprob)/sum(tprob)), 
                                        by = c("household_code", "week_end", "pprob")]
  hh_retailers_temp[, `:=`(mu_all = mu_all * pprob, mu_ground = mu_ground * pprob,
                           mu_diff = mu_diff * pprob), by = c("household_code", "week_end")]
  hh_retailers_temp[, `:=`(mu_val = exp(mu_diff/preferences[as.character(i), 20])), by = c("household_code", "week_end")]
  return(hh_retailers_temp)
}


#---------------------------------------------------------------------------------------------------#
# Market Level prices
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

# Obtain the retailer weight
retailer_trips = trips[, .(ntrips = .N), by = c("dma_code", "retailer_code", "week_end")]
setkey(retailer_trips, retailer_code, week_end)
setkey(hw_prices, retailer_code, week_end)
hw_prices = hw_prices[retailer_trips, nomatch=0L]
hw_prices[, ntrips:=ntrips/1000]
hw_p_index0 = hw_prices[channel_type!="Warehouse Club", 
                        .(price=sum(price*ntrips)/sum(ntrips),
                          price_regular=sum(price_regular*ntrips)/sum(ntrips),
                          warehouse=0), 
                        by = c("dma_code", "series", "week_end")]
hw_p_index1 = hw_prices[,.(price=sum(price*ntrips)/sum(ntrips),
                           price_regular=sum(price_regular*ntrips)/sum(ntrips),
                           warehouse=1), 
                        by = c("dma_code", "series", "week_end")]
hw_p_index = rbindlist(list(hw_p_index0, hw_p_index1))
rm(hw_p_index0, hw_p_index1)

# Obtain the series weight
setkey(series_sales, series)
setkey(hw_p_index, series)
hw_p_index = hw_p_index[series_sales, nomatch=0L]

# Obtain the overall price environment
hw_p_index = hw_p_index[, .(price = sum(price*revenue)/sum(revenue), 
                            price_regular = sum(price_regular*revenue)/sum(revenue)), 
                        by = c("dma_code", "warehouse", "week_end")]
setkey(hw_p_index, dma_code, warehouse, week_end)

# Create Price lags
hw_p_index[,`:=`(price_lag = c(NA, price[1:(length(price)-1)]), 
                 price_reg_lag = c(NA, price_regular[1:(length(price_regular)-1)])), by = c("dma_code", "warehouse")]

# Create the six week moving average price
#hw_p_index[,`:=`(price_lag_2 = c(NA, price_lag[1:(length(price_lag)-1)])), by = c("dma_code", "warehouse")]
#hw_p_index[,`:=`(price_lag_3 = c(NA, price_lag_2[1:(length(price_lag_2)-1)])), by = c("dma_code", "warehouse")]
#hw_p_index[,`:=`(price_lag_4 = c(NA, price_lag_3[1:(length(price_lag_3)-1)])), by = c("dma_code", "warehouse")]
#hw_p_index[,`:=`(price_lag_5 = c(NA, price_lag_4[1:(length(price_lag_4)-1)])), by = c("dma_code", "warehouse")]
#hw_p_index[,`:=`(price_lag_6 = c(NA, price_lag_5[1:(length(price_lag_5)-1)])), by = c("dma_code", "warehouse")]
#hw_p_index[, price_avg := rowMeans(cbind(price_lag, price_lag_2, price_lag_3, price_lag_4, price_lag_5, price_lag_6), 
#                                    na.rm=TRUE)]
#hw_p_index[is.na(price_avg), price_avg := price]
#hw_p_index[, `:=`(price_lag_2=NULL, price_lag_3=NULL, price_lag_4=NULL, price_lag_5=NULL, price_lag_6=NULL)]

# Update based on best fit to data
fitw <- function(w){
  pr = exp(w)/(1+exp(w))
  hw_p_index[, price_avg:=mavg(price, pr), by = c("dma_code", "warehouse")]
  preg = hw_p_index[, lm(price~price_avg)]
  return(sum(residuals(preg)^2))
}

popt = optim(0, fitw, gr = NULL, method = c("BFGS"), hessian=TRUE)
w = popt$par
pr = exp(w)/(1+exp(w))
hw_p_index[, price_avg:=mavg(price, pr), by = c("dma_code", "warehouse")]
hw_p_index[, price_avgn:=mavgn(price, pr), by = c("dma_code", "warehouse")]
preg = hw_p_index[, lm(price~price_avg)]

# Regress price on price moving averages.
hw_p_index[, summary(lm(price~price_avg))]
setkey(hw_p_index, dma_code, warehouse, week_end)
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
hw_panel = hw_panel[hh_list[,.(household_code, hware, hseries, hfirst_date, hlast_date,
                               sfirst_date, slast_date, k_first_date)], nomatch=0L]

# Get rid of trips beyond adoption date
setkey(hw_panel, household_code, week_end)
hw_panel = hw_panel[week_end<=(hfirst_date+6) | is.na(k_first_date) | (is.na(hfirst_date) & week_end<=(k_first_date+6)), ]

# Get the dma code of the household
hw_panel[, panel_year := year(week_end)]
setkey(hw_panel, household_code, panel_year)
hw_panel = hw_panel[hh[, .(household_code, panel_year, dma_code)], nomatch=0L]
hw_panel[, quarter:= paste(format(week_end, "%y/"), 0, 
                           sub( "Q", "", quarters(week_end) ), sep = "")]

# Obtain Household Warehouse shopping status
trips[, quarter:= paste(format(week_end, "%y/"), 0, 
                        sub( "Q", "", quarters(week_end) ), sep = "")]
warehouse_retailers = c(9101, 9103, 9104)
trips[, warehouse:=as.integer(retailer_code %in% warehouse_retailers)]
hh_ware_status = trips[, .(warehouse=as.integer(any(warehouse==1))), by = c("household_code", "quarter")]
setkey(hh_ware_status, household_code, quarter)
setkey(hw_panel, household_code, quarter)
hw_panel = hw_panel[hh_ware_status, nomatch=0L]

# Next step is to merge in the prices
setkeyv(hw_panel, c("dma_code", "week_end", "warehouse"))
setkeyv(hw_p_index, c("dma_code", "week_end", "warehouse"))
hw_panel = hw_panel[hw_p_index, nomatch=0L]

# Flag Adoption date
setkey(hw_panel, week_end)
hw_panel[, t:=.GRP, by = "week_end"]
hw_panel[, purchased := as.integer(t==max(t)), by = c("household_code")]
hw_panel[is.na(k_first_date), purchased:=0]

# Setkey and save the data
setkey(hw_panel, household_code, week_end)
save(hw_panel, file = paste(output_dir, "/HH-HW-Panel.RData", sep=""))
#---------------------------------------------------------------------------------------------------#
# Market Level prices

# Average purchases 
retailer_share = purchases[, .(np = .N), by = c("retailer_code", "panel_year")]
retailer_trips = trips[, .(ntrips = .N), by = c("retailer_code", "dma_code", "panel_year", "week_end")]

# create new weights
setkey(retailer_share, retailer_code, panel_year)
setkey(retailer_trips, retailer_code, panel_year)
retailer_share = retailer_trips[retailer_share, nomatch = 0L]
retailer_share[, nweight := np*ntrips/10000]

# Merge in price
hw_prices[, panel_year := year(week_end)]
setkey(retailer_share, retailer_code, panel_year, week_end)
setkey(hw_prices, retailer_code, panel_year, week_end)
hw_p_index = hw_prices[retailer_share, nomatch=0L, allow.cartesian=TRUE]
hw_p_index = hw_p_index[series==3 | series==4, ]
hw_p_index0 = hw_p_index[!(retailer_code %in% warehouse_retailers),
                         .(price=sum(price*nweight)/sum(nweight),
                           price_regular=sum(price_regular*nweight)/sum(nweight),
                           warehouse=0), 
                         by = c("series", "dma_code", "week_end")]
hw_p_index1 = hw_p_index[retailer_code %in% warehouse_retailers, 
                         .(price=sum(price*np)/sum(np),
                           price_regular=sum(price_regular*np)/sum(np),
                           warehouse=1), 
                         by = c("series", "dma_code", "week_end")]
hw_p_index = rbindlist(list(hw_p_index0, hw_p_index1))
rm(hw_p_index0, hw_p_index1)

# Obtain the series weight
setkey(series_sales, series)
setkey(hw_p_index, series)
hw_p_index = hw_p_index[series_sales, nomatch=0L]

# Obtain the overall price environment
hw_p_index = hw_p_index[, .(price = sum(price*revenue)/sum(revenue), 
                            price_regular = sum(price_regular*revenue)/sum(revenue)), 
                        by = c("dma_code", "warehouse", "week_end")]
setkey(hw_p_index, dma_code, warehouse, week_end)

# Check how good the price variation is in the real data! 
setkey(hw_p_index, dma_code, warehouse, week_end)
setkey(purchases, dma_code, warehouse, week_end)
price_check = purchases[hw_p_index[, .(dma_code, warehouse, week_end, price)], nomatch=0L]
price_check[series==3, cor(price, i.price)]

# Create Price lags
hw_p_index[,`:=`(price_lag = c(NA, price[1:(length(price)-1)]), 
                 price_reg_lag = c(NA, price_regular[1:(length(price_regular)-1)])), 
           by = c("warehouse")]

# Update based on best fit to data
fitw <- function(w){
  pr = exp(w)/(1+exp(w))
  hw_p_index[, price_avg:=mavg(price, pr), by = c("household_code", "dma_code")]
  preg = hw_p_index[, lm(price~price_avg)]
  return(sum(residuals(preg)^2))
}

popt = optim(0, fitw, method = c("Brent"), lower = -10, upper = 10, hessian=TRUE)
w = popt$par
pr = exp(w)/(1+exp(w))
hw_p_index[, price_avg:=mavg(price, pr), by = c("household_code", "dma_code")]
hw_p_index[, price_avgn:=mavgn(price, pr), by = c("household_code", "dma_code")]
preg = hw_p_index[, lm(price~price_avg)]

# Regress price on price moving averages.
summary(preg)
setkey(hw_p_index, household_code, dma_code, week_end)
save(hw_p_index, file = paste(output_dir, "/HW-P-Index.RData", sep=""))
write.csv(hw_p_index, file="HW-P-Index.csv", row.names = FALSE)

############
# Compute CV

# Compute the adoption value consumer by consumer
xvars = c(paste0("a", 2:18), "keurig", "lag_ground", "lag_keurig", "incdiff")
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
  hh_retailers_temp[is.na(lag_ind), lag_ind:=0]
  xmat = as.matrix(hh_retailers_temp[, xvars, with=FALSE])
  # Compute the mean utility of product
  hh_retailers_temp[, u := xmat %*% preferences[as.character(i),] + size1_amount]
  hh_r_temp = hh_retailers_temp[, .(mu=max(u)), by = c("household_code", "week_end", 
                                                       "retailer_code", "brand_descr", "keurig", "tprob", "pprob")]
  hh_r_temp = hh_r_temp[, .(mu_all = log(sum(exp(mu))),
                            mu_ground = log(sum(exp(mu) * (1-keurig)))), 
                        by = c("household_code", "week_end", "retailer_code", "tprob", "pprob")]
  hh_r_temp[is.infinite(mu_ground), mu_ground:= preferences[as.character(i), 20] * log(TBudget[as.character(i)])] 
  # If non-purchase receive the utility from budget
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
      hh_val_temp = hh_val_temp[, .(mu=max(u)), by = c("household_code", "week_end", 
                                                       "retailer_code", "brand_descr", "keurig", "tprob", "pprob")]
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
                   mu_ev = mu_ev * pprob, mu_diff = mu_diff * pprob, mu_val = mu_val*pprob), 
            by = c("household_code", "week_end")]
  return(hh_r_temp)
}
