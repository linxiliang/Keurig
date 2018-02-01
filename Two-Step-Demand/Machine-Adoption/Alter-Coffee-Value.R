###########################################################################################

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
