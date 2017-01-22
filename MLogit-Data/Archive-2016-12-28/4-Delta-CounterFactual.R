# ----------------------------------------------------------------------------------------------------#
# Compute the adoption value by consumer and time period!
big_markets = c(501, 602, 803, 504, 539, 506, 524, 613, 623, 753) # Big Markets
load(paste(output_dir, "/hh_trip_panel.RData", sep=""))
load(paste(output_dir, "/Retailer_Price_Panel.RData", sep=""))
load(paste(output_dir, "/Coffee-Panel-Cond-Purchase.RData", sep=""))
load(paste(output_dir, "/HH-HW-Panel.RData", sep=""))
# Load HH file with flags
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))
load(paste(meta_dir, "/HH.RData", sep=""))
setkey(hh, household_code, panel_year)
# Load HMS Purchase data
load(paste(HMS_input_dir, "/HMS-Purchases-Coffee.RData", sep=""))
# coffee trip code list
coffee_trip_code_list = unique(purchases[as.integer(product_module_code)==1463, trip_code_uc])

brands = c("CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GLORIA JEAN'S KEURIG", "GREEN MOUNTAIN KEURIG",
           "MAXWELL HOUSE", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")
xnames = c("0OTHER", "CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GLORIA JEAN'S KEURIG", "GREEN MOUNTAIN KEURIG",
           "MAXWELL HOUSE", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS",
           "STARBUCKS KEURIG", "TULLY'S KEURIG", "keurig", "brand_lag_ground", 
           "brand_lag_keurig", "price_coef", "budget")

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
retailer_panel = retailer_panel[dma_code %in% big_markets, ]
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

#Remove not needed datasets and garbage collection
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
  # How do I deal with expenditure???? This is a key question that needs to be answered!
  
  hh_retailers_temp[, u := xmat %*% preferences[as.character(i),] + size1_amount]
  hh_r_temp = hh_retailers_temp[, .(mu=max(u)), by = c("household_code", "week_end", 
                                                       "retailer_code", "brand_descr", "keurig", "tprob", "pprob")]
  hh_r_temp[, mu_mean:=mean(mu), by = c("household_code", "week_end", 
                                        "retailer_code", "tprob", "pprob")]
  
  # For each brand generate separate utility difference - removing the following brands
  b_r_list = c("NOTHING", "CARIBOU KEURIG", "DONUT HOUSE KEURIG", "FOLGERS KEURIG", 
               "GLORIA JEAN'S KEURIG", "GREEN MOUNTAIN KEURIG", "NEWMAN'S OWN ORGANICS KEURIG", 
               "STARBUCKS KEURIG", "TULLY'S KEURIG")
  hh_r_agg_all = data.table(NULL)
  bn = 0
  for (br in b_r_list){
    bn = bn+1
    hh_r_temp[, in_br := as.integer(brand_descr!=br)]
    hh_r_agg = hh_r_temp[, .(mu_all = log(sum(exp(mu-mu_mean)*in_br)) + mu_mean[1],
                             mu_ground = log(sum(exp(mu-mu_mean)*(1-keurig)*in_br)) + mu_mean[1]), 
                         by = c("household_code", "week_end", "retailer_code", "tprob", "pprob")]
    # If non-purchase receive the utility from budget
    hh_r_agg[is.infinite(mu_ground), mu_ground:= preferences[as.character(i), 19] * log(TBudget[as.character(i)])] 
    # If less, only possibility is to spend on outside option
    hh_r_agg[mu_all<mu_ground, mu_all:=mu_ground]
    hh_r_agg[, `:=`(mu_diff = mu_all - mu_ground)]
    hh_r_agg = hh_r_agg[, .(mu_diff = sum(mu_diff * tprob)/sum(tprob)), 
                          by = c("household_code", "week_end", "pprob")]
    hh_r_agg = hh_r_agg[, .(mu_diff = mu_diff * pprob), 
                        by = c("household_code", "week_end")]
    hh_r_agg[, `:=`(brand_descr= br, bnum = bn)]
    hh_r_agg_all = rbindlist(list(hh_r_agg_all, hh_r_agg))
  }
  return(hh_r_agg_all)
}
hh_codes = as.integer(row.names(preferences))
clusterExport(cl, c('hh_trip_prob', 'retailer_panel', 'preferences', 'TBudget', 'hh_panel', 'xvars', 'hhValFun'))
cval_list = parLapply(cl, hh_codes, hhValFun)
cval_list = rbindlist(cval_list)
save(cval_list, file = paste(output_dir, "/HH-Util-Diff-By-Brand.RData", sep=""))

load( paste(output_dir, "/HH-Util-Diff-By-Brand.RData", sep=""))
# Merge consumption value back to the value function
setkey(cval_list, household_code, week_end)
setkey(hw_panel, household_code, week_end)
hw_br_panel = hw_panel[cval_list, nomatch=0L]

# Export Relevant Data sets to csv format, and use them for julia estimation
hw_br_panel[, `:=`(ntrip =.GRP), by = c("household_code", "week_end")]
setkey(hw_br_panel, ntrip)
write.csv(hw_br_panel[,.(household_code, hware, ntrip, t, bnum, price, 
                         price_avg, price_avgn, mu_diff)],
          file = "Data/Machine-Adoption/HH-HW-Brand-Panel.csv", row.names = FALSE)
save(hw_br_panel, file = "Data/Machine-Adoption/HH-HW-Brand-Panel.RData")
