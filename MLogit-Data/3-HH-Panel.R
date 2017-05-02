#---------------------------------------------------------------------------------------------------#
# Load appropriate data

# Load HMS Trips 
load(paste(meta_dir, "/Trips.RData", sep=""))

# Load Appropriate Auxiliary data sets generated in Price Imputation Algorithm
load(paste(output_dir, "/Assist_Data_Sets_Retailer_Prices.RData", sep=""))

# Load Imputed Price and Availability Data - this is based on both RMS and Homescan
load(paste(output_dir, "/Retailer_Price_Panel.RData", sep=""))
#load(paste(output_dir, "/Retailer_Price_Panel_Region.RData", sep=""))

# Only trips by chosen household
trips = trips[household_code %in% hh_list[, household_code], ]
setkeyv(trips, c("household_code", "purchase_date", "trip_code_uc"))

# Flag trips with coffee purchases.
coffee_trip_list = unique(purchases[selected==1, trip_code_uc])
trips[, coffee_trip:=as.integer(trip_code_uc %in% coffee_trip_list)]
rm(coffee_trip_list)

# Merge in retailer channel information
setkey(trips, retailer_code)
trips = retailers[trips, nomatch=0L]
setkeyv(trips, c("household_code", "purchase_date", "trip_code_uc"))

# Change the purchases online retailer code to the online retailer code
trips[channel_type=="Online Shopping", retailer_code:=4849]

# Year threshold - purchases/trips before which year should be dropped 
year_threshold = 2006

#---------------------------------------------------------------------------------------------------#
# Now prepare the consumer panel.

# Create week_end variable in trips
date_week_end = unique(purchases[, .(purchase_date, week_end)])
setkey(date_week_end, purchase_date)
setkey(trips, purchase_date)
trips = trips[date_week_end, nomatch=0L]
gc()

# Aggregate spending to weekly level
trips[, `:=`(total_spent_week = sum(total_spent), coff = sum(coff), hot = sum(hot), caf = sum(caf), 
             drink = sum(drink), grocery = sum(grocery), rest = sum(rest)), by = c("household_code", "week_end")]

# Get rid of trips definitely not purchasing coffee based on two criteria
# (a) Top 90% coffee retailers and purchased coffee there at least once.
# (b) The household purchased coffee at the retailer at least twice
# (c) The trip doesn't have coffee purchase
coffee_retailer_95 = unique(retailer_type_sales[cumshare<=0.9501, retailer_code])
coffee_retailer_90 = unique(retailer_type_sales[cumshare<=0.9001, retailer_code])
trips[, top_95_retailer:=as.integer(retailer_code %in% coffee_retailer_95)]
trips[, top_90_retailer:=as.integer(retailer_code %in% coffee_retailer_90)]
hh_retailers = purchases[, .(npurch = .N), by = c("household_code", "retailer_code")]
setkeyv(trips, c("household_code", "retailer_code"))
setkeyv(hh_retailers, c("household_code", "retailer_code"))
trips = hh_retailers[trips]
trips[is.na(npurch), npurch:=0]
trips = trips[as.integer(top_90_retailer)==1 | as.integer(npurch)>=1, ]
trips[, potential_trip:=as.integer(npurch>=2|(top_90_retailer==1&npurch>=1)|coffee_trip==1)]
trips = trips[potential_trip==1, ]

# Obtain the number of coffee purchasing trips made per week
trips[, `:=`(coffee_purch_trips=sum(coffee_trip)), by = c("household_code", "week_end")]

# Keep coffee purchasing trips or trips when no coffee purchase is made
trips = trips[(coffee_purch_trips>=1&coffee_trip==1)|(coffee_purch_trips==0), ]

# Given the same retailer has similar retailing environment -- 
# Aggregate non-coffee purchasing trips to week retailer level.
setkey(trips, household_code, week_end, purchase_date, trip_code_uc, retailer_code)
trips[, `:=`(retailer_week_ntrip = 1:.N), by = c("household_code", "week_end", "retailer_code")]
trips = trips[retailer_week_ntrip==1|coffee_trip==1, ]

# Define trip filter as coffee purchasing trip or
# trip to a random retailer drawn from the multinomial distribution with probability proportional to coffee visits
trips[, pr_retailer := npurch/sum(npurch), by = c("household_code", "week_end")]
trips[, `:=`(purch_retailer = as.vector(rmultinom(1, size = 1, prob = pr_retailer))), 
      by = c("household_code", "week_end")]
trips = trips[purch_retailer==1|coffee_trip==1, ]

# Get rid of unnecessary variables
trips[, `:=`(potential_trip = NULL, retailer_week_ntrip=NULL, purch_retailer=NULL, pr_retailer=NULL)]

# Initialize the purchases panel.
hh_panel_year = unique(hh[household_code%in%hh_list$"household_code", 
                          .(household_code, panel_year, dma_code)])
panel_year_week = unique(purchases[, .(panel_year, week_end)])
setkey(panel_year_week, panel_year)
setkey(hh_panel_year, panel_year)
hh_panel = hh_panel_year[panel_year_week, allow.cartesian=TRUE]
setkeyv(hh_panel, c("household_code", "week_end"))

# Merge in trip information
setkey(trips, household_code, panel_year, week_end)
setkey(hh_panel, household_code, panel_year, week_end)
hh_panel = trips[hh_panel, allow.cartesian=T]
hh_panel[, `:=`(retailer_trip=ifelse(is.na(coffee_trip), 0, 1), 
                coffee_purch_trips=ifelse(is.na(coffee_trip), 0, coffee_purch_trips),
                coffee_trip=ifelse(is.na(coffee_trip), 0, coffee_trip),
                store_code_uc=NULL, top_95_retailer=NULL, top_90_retailer=NULL,
                store_zip3 = NULL)]
setkey(hh_panel, household_code, week_end, purchase_date, trip_code_uc)
hh_panel[, `:=`(norder = 1:.N), by = c("household_code", "week_end")]

#---------------------------------------------------------------------------------------------------#
# Now impute the state dependence variables - last purchase and cumulative purchase without interruption
# Run this in parallel

# Obtain the relevant purchases
focal_purch = purchases[household_code %in% hh_list[, household_code], ]

# Aggregate purchases to household, week_end, purchase date, retailer, brand and size level. 
focal_purch = focal_purch[, .(quantity = sum(quantity), brand_descr = brand_descr[1], 
                              total_price_paid = sum(total_price_paid),
                              coupon_value = sum(coupon_value),
                              total_spent = total_spent[1],
                              panel_year = panel_year[1]), 
                          by = c("household_code", "dma_code", "week_end", "purchase_date", 
                                 "trip_code_uc", "retailer_code", "brand_descr_orig", "ptype", 
                                 "keurig", "size1_amount", "roast", "flavored", "kona", 
                                 "colombian", "sumatra", "wb")]

# Deal with CTL BR at different retailers.
focal_purch[, brand_type_modified := brand_descr_orig] # Deal with CTL BR at different retailers.
focal_purch[brand_descr_orig == "CTL BR", brand_type_modified:=paste(brand_descr_orig, retailer_code, sep="_")] 
focal_purch[, `:=`(brand_type_modified = paste(brand_type_modified, ptype, sep = "_"))]
focal_purch[, `:=`(n_brand=length(unique(brand_type_modified)), n_type=length(unique(ptype))), 
            by = c("household_code", "purchase_date", "trip_code_uc")]

# Generate lagged brand and type
purch_state = focal_purch[, .(brand_type_current=paste(brand_type_modified, collapse = " --- "),
                              ptype_current=paste(ptype, collapse = " --- "),
                              units = sum(quantity*size1_amount),
                              n_brand=length(unique(brand_type_modified)), 
                              n_type=length(unique(ptype))), 
                          by = c("household_code", "week_end", "purchase_date", "trip_code_uc")]
purch_state[, np_hh:=.N, by = "household_code"]
purch_state = purch_state[np_hh >=2, ]
purch_state[, np_hh:=NULL]
setkey(purch_state, household_code, purchase_date, trip_code_uc)
purch_state[, `:=`(brand_type_lag = c(NA, brand_type_current[1:(.N-1)]),
                   ptype_lag = c(NA, ptype_current[1:(.N-1)]),
                   purchase_date_lag = c(NA, purchase_date[1:(.N-1)]),
                   brand_type_lag2 = c(NA, NA, brand_type_current[1:(.N-2)]),
                   ptype_lag2 = c(NA, NA, ptype_current[1:(.N-2)]),
                   purchase_date_lag2 = c(NA, NA, purchase_date[1:(.N-2)])), 
            by = c("household_code")]
purch_state[, `:=`(days_since_last_purch=as.integer(purchase_date-purchase_date_lag),
                   days_since_last_purch2=as.integer(purchase_date-purchase_date_lag),
                   purchase_date_lag=as.Date(purchase_date_lag, origin="1970-01-01"),
                   purchase_date_lag2=as.Date(purchase_date_lag2, origin="1970-01-01"))]
purch_state[days_since_last_purch>=366, `:=`(brand_type_lag=NA, purchase_date_lag=NA,
                                             ptype_lag=NA, days_since_last_purch=NA,
                                             brand_type_lag2=NA, ptype_lag2=NA, 
                                             purchase_date_lag2=NA, days_since_last_purch2=NA)]

# Set the key for focal purchases to obtain the correct ordering
setkeyv(purch_state, c("household_code", "week_end", "purchase_date", "trip_code_uc"))
state_func <- function(hh_code){
  hpurch = purch_state[.(hh_code), ]
  nobs = nrow(hpurch)
  brand_cum = rep(NA, nobs)
  if (nobs==1) return(data.table(brand_cum = brand_cum))
  for (i in 1:nobs){
    if (i==1 | is.na(hpurch[i, days_since_last_purch])) {
      cum_state = hpurch[i, units]
    } else{
      if (hpurch[i, trip_code_uc]==hpurch[i-1, trip_code_uc]){
        brand_cum[i] = brand_cum[i-1]
      } else{
        brand_cum[i] = cum_state
      }
      if (hpurch[i, grepl(brand_type_current, brand_type_lag)] & hpurch[i, n_brand]==1){
        cum_state = cum_state + hpurch[i, units]
      } else{
        if (hpurch[i, n_brand]>=2){
          cum_state = 0
        } else{
          cum_state = hpurch[i, units]
        }
      }
    }
  }
  return(data.table(brand_cum = brand_cum))
}

# Apply the function in parallel to each household
hh_codes = unique(purch_state$household_code)
clusterExport(cl, c('purch_state', 'state_func'))
state_list = parLapply(cl, hh_codes, state_func)
state_list = rbindlist(state_list)
purch_state = cbind(purch_state, state_list)
rm(state_list)

# Change brand_descr current
purch_state[, `:=`(cum_temp=ifelse(n_brand>=2, 0, ifelse(brand_type_current == brand_type_lag, 
                                                         units+brand_cum, units)))]
# Match hh_panel and focal_purch, and roll states 
setkey(hh_panel, household_code, week_end, purchase_date, trip_code_uc)
hh_panel[, temp_id:=.GRP, by = c("household_code", "week_end", "purchase_date", "trip_code_uc")]
setkey(purch_state, trip_code_uc)
setkey(hh_panel, trip_code_uc)
purch_state = purch_state[hh_panel[,.(trip_code_uc, temp_id)], nomatch=0L]
setkey(purch_state, household_code, temp_id)
setkey(hh_panel, household_code, temp_id)
# Match panel with purchases
hh_panel = purch_state[,.(household_code, temp_id, brand_type_lag, purchase_date_lag, 
                           ptype_lag, brand_cum)][hh_panel, roll = -Inf, rollends = c(T, F)]
hh_panel = purch_state[,.(household_code, temp_id, brand_type_current, purchase_date, 
                           ptype_current, cum_temp)][hh_panel, roll = Inf, rollends = c(F, T)]
hh_panel = purch_state[,.(household_code, temp_id, units)][hh_panel]

hh_panel[is.na(brand_type_lag), `:=`(brand_type_lag = brand_type_current, 
                                     ptype_lag = ptype_current, 
                                     brand_cum = cum_temp,
                                     purchase_date_lag = purchase_date)]
hh_panel[, `:=`(days_to_last_purch=ifelse(is.na(i.purchase_date), week_end-purchase_date_lag, 
                                          i.purchase_date-purchase_date_lag),
                purchase_date = i.purchase_date, i.purchase_date=NULL, brand_type_current=NULL,
                ptype_current=NULL, cum_temp=NULL, temp_id=NULL, units=ifelse(is.na(units),0,units))]

#---------------------------------------------------------------------------------------------------#
# Impute the inventory
hh_inventory = hh_panel[, .(tot_units=sum(units), coffee_trips=sum(coffee_trip),
                            ptype_lag = ptype_lag[1], brand_lag = brand_type_lag[1]), 
                        by = c("household_code", "week_end", "panel_year")]
# Record the first purchase in the panel_year
max_week_end = max(hh_inventory[, week_end])
hh_inventory[coffee_trips>=1, first_panel_purch_week := week_end]
hh_inventory[is.na(first_panel_purch_week), first_panel_purch_week := max_week_end]
hh_inventory[, first_panel_purch_week := min(first_panel_purch_week), by = c("household_code", "panel_year")]
# Modfiy ptype_lag to be a unique one value variable - take the mode [not concatenated]
hh_inventory[, ptype_lag:=names(sort(-table(strsplit(ptype_lag, " --- "))))[1], 
             by = c("household_code", "week_end", "panel_year")]

# Get the average consumption rate by gound coffee and Keurig-Kcups
hh_rate = purchases[, .(quantity = sum(quantity * size1_amount)), by = .(household_code, purchase_date, ptype)]
setkeyv(hh_rate, c("household_code", "purchase_date", "ptype"))
hh_rate[, norder:=1:.N, by = c("household_code", "purchase_date")] # Ignore cases where multiple purchases are made in the same day.
hh_rate = hh_rate[as.integer(norder)==1, ]
hh_rate[, norder:=.N, by = c("household_code")] 
hh_rate = hh_rate[norder!=1, ] # Ignore households with one purchase
hh_rate[, next_purch_date:=c(purchase_date[2:length(ptype)], as.Date(NA)), by = c("household_code")]
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
# First recorded purchase is a keurig, and flagged as keurig rate!
hh_inventory[is.na(crate) & !is.na(arate), `:=`(crate=arate, ptype_lag="KEURIG")] 

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

# Remove unneeded data sets and clear ram
rm(trips)
gc()

setkeyv(hh_panel, c("household_code", "week_end", "norder"))
save(hh_panel, focal_purch, hh_rate, file = paste(output_dir, "/hh_trip_panel.RData", sep=""))

#---------------------------------------------------------------------------------------------------#
# Restrict household panel to purchases made on 2006 or later
# and get rid of observations where the last brand is not observed -- 
# not dealing with initial condition for now.
hh_panel = hh_panel[panel_year>=year_threshold & !is.na(brand_type_lag), ]

# Get rid of variables not needed for now.
hh_panel = hh_panel[, .(dma_code, retailer_code, household_code, kholding, week_end, panel_year, norder, trip_code_uc, 
                        coffee_trip, brand_type_lag, brand_cum, ptype_lag, inv_all, inv_type, inv_brand_type)]

# Now use the trip data to create purchase panel for mixed logit demand estimation
# Constrain both to be in the top DMAs
hh_panel = hh_panel[dma_code%in%top_dma_list, ]
retailer_panel = retailer_panel[dma_code%in%top_dma_list, ]
setkeyv(hh_panel, c("dma_code", "retailer_code", "week_end"))
setkeyv(retailer_panel, c("dma_code", "retailer_code", "week_end"))

# Still, for some shopping occasions, the consumer could be choosing from over 90 alternatives, 
# which is not realistic. So, I make some assumptions about consumer's consideration set. 
# I define the product to be in the consideration set if the brand size combinations is in the 
# top 80% in the market (dma+keurig) Or the brand size combintation is in the top 95% of the 
# consumer's consumption set.
# Then, product is retained if it meets either of the above criteria or chosen at the choice occasion. 
brand_size_type_sales = purchases[, .(revenue = sum(total_price_paid-coupon_value)), 
                                  by = c("dma_code", "ptype", "keurig", "brand_descr", "size1_amount",
                                         "roast", "flavored", "kona", "colombian", "sumatra", "wb")]
brand_size_type_sales[, rshare:=revenue/sum(revenue), by=c("dma_code", "ptype", "keurig")]
brand_size_type_sales = brand_size_type_sales[order(-rshare), ]
setkeyv(brand_size_type_sales, c("dma_code", "ptype", "keurig"))
brand_size_type_sales[, rcumshare:=cumsum(rshare), by=c("dma_code", "ptype", "keurig")]
brand_size_type_sales[, rcumshare:=rcumshare-rshare]
brand_size_type_sales=brand_size_type_sales[rcumshare<=0.8, .(dma_code, ptype, keurig, brand_descr, size1_amount, 
                                                              roast, flavored, kona, colombian, sumatra, wb, rshare)]
hh_brand_size_sales = purchases[, .(revenue = sum(total_price_paid-coupon_value)), 
                                by = c("household_code", "ptype", "keurig", "brand_descr", "size1_amount",
                                       "roast", "flavored", "kona", "colombian", "sumatra", "wb")]
hh_brand_size_sales[, rshare:=revenue/sum(revenue), by=c("household_code", "keurig")]
hh_brand_size_sales = hh_brand_size_sales[order(-rshare), ]
setkeyv(hh_brand_size_sales, c("household_code", "ptype", "keurig"))
hh_brand_size_sales[, rcumshare:=cumsum(rshare), by=c("household_code", "ptype", "keurig")]
hh_brand_size_sales[, rcumshare:=rcumshare-rshare]
hh_brand_size_sales=hh_brand_size_sales[rcumshare<=0.95, .(household_code, ptype, keurig, brand_descr, size1_amount, 
                                                           roast, flavored, kona, colombian, sumatra, wb, rshare)]

# Loop over year to create panel -- this is to avoid memory overloading
year_list = hh_panel[, unique(panel_year)]
hh_prod_panel = data.table(NULL)
for (yr in year_list){
  hh_year = hh_panel[panel_year==yr, ]
  hh_prod_temp = hh_year[retailer_panel, allow.cartesian=TRUE, nomatch=0L]

  # Get rid of Keurig products when the consumer doesn't hold keurig machine
  hh_prod_temp = hh_prod_temp[kholding==1 | (kholding == 0 & keurig == 0), ]
  
  # Merge in the purchase data to locate the purchased product
  setkey(focal_purch, trip_code_uc, brand_descr, keurig, ptype, size1_amount, 
         roast, flavored, kona, colombian, sumatra, wb)
  setkey(hh_prod_temp, trip_code_uc, brand_descr, keurig, ptype, size1_amount, 
         roast, flavored, kona, colombian, sumatra, wb)
  hh_prod_temp = focal_purch[, .(trip_code_uc, brand_descr, brand_descr_orig, keurig, ptype, 
                                 size1_amount, roast, flavored, kona, colombian, sumatra, wb,
                                 quantity, total_price_paid, coupon_value)][hh_prod_temp]
  hh_prod_temp[is.na(quantity), quantity := 0]
  # Treat purchases of multiple packs as purchase of a single large pack, consider consumer coupon values
  hh_prod_temp[quantity>=2, size1_amount:=quantity*size1_amount]
  hh_prod_temp[quantity>=2, price:=total_price_paid-coupon_value]
  hh_prod_temp[, sum_quantity:=as.integer(sum(quantity)), by ="trip_code_uc"]
  hh_prod_temp[sum_quantity==0&brand_descr=="0NOTHING", quantity:=1]
  
  # Merge the criteria into the hh_prod_temp
  onames = names(hh_prod_temp)
  setkeyv(brand_size_type_sales, c("dma_code", "ptype", "keurig", "brand_descr", "size1_amount",
                                   "roast", "flavored", "kona", "colombian", "sumatra", "wb"))
  setkeyv(hh_prod_temp, c("dma_code", "ptype", "keurig", "brand_descr", "size1_amount", 
                          "roast", "flavored", "kona", "colombian", "sumatra", "wb"))
  hh_prod_temp = brand_size_type_sales[hh_prod_temp]
  setkeyv(hh_brand_size_sales, c("household_code", "ptype", "keurig", "brand_descr", "size1_amount",  
                                 "roast", "flavored", "kona", "colombian", "sumatra", "wb"))
  setkeyv(hh_prod_temp, c("household_code", "ptype", "keurig", "brand_descr", "size1_amount", 
                          "roast", "flavored", "kona", "colombian", "sumatra", "wb"))
  hh_prod_temp = hh_brand_size_sales[hh_prod_temp]
  hh_prod_temp = hh_prod_temp[!is.na(rshare) | !is.na(i.rshare) | as.integer(quantity)>=1 | brand_descr=="0NOTHING", ]
  hh_prod_temp[,`:=`(rshare=NULL, i.rshare=NULL)]
  setcolorder(hh_prod_temp, onames)
  
  gc()
  hh_prod_panel = rbindlist(list(hh_prod_panel, hh_prod_temp))
  cat("Panel year", yr, "Completed!\n\n")
}

# Get rid of households having less than 10 choice occassions, very small amount of data
hh_prod_panel[, sum_quantity:=sum(quantity), by ="household_code"]
hh_prod_panel = hh_prod_panel[sum_quantity>=10, ]
hh_prod_panel[, sum_quantity:=NULL]

# Save the panel
setkeyv(hh_prod_panel, c("household_code", "week_end", "norder", "trip_code_uc", "brand_descr", "keurig", 
                         "size1_amount", "roast", "flavored", "kona", "colombian", "sumatra", "wb"))
save(hh_prod_panel, hh_rate, top_dma_list, top_keurig_brands, 
     file = paste(output_dir, "/hh_product_panel.RData", sep=""))

