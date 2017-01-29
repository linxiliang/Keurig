#####################################################################################################
#
# Take Consumer Tastes and Compute the Expected Utility Gain
# Xiliang Lin
# December, 2016
#
#####################################################################################################

# Settings
rm(list = ls())

coffee_modules      = c(1463)
maker_modules       = 7755
big_markets = c(501, 506, 504, 602, 803, 511, 539, 623, 618, 505, 
                613, 819, 524, 534, 533, 753, 510, 508, 514, 512, 
                517, 807, 751, 862, 535, 521, 548, 609, 566, 641) 

# Load Necessary Packages
library(parallel)
library(data.table)
setNumericRounding(0)
library(nloptr)

# Set Working Folder Path Here
setwd("~/Keurig")
meta_dir  = "Data/Meta-Data"
HMS_input_dir = "Data/HMS-Transactions"
mlogit_dir = "Data/MLogit-Data"
output_dir = "Data/Machine-Adoption"

# Set seed
RNGkind("L'Ecuyer-CMRG")
set.seed(12345)

# Source function
source('Scripts/Two-Step-Demand/Machine-Adoption/UMax-functions.R')

#---------------------------------------------------------------------------------------------------#
# Initialize Parallel Execution Environment
cores = detectCores(logical=TRUE)
cl = makeCluster(cores)
s = .Random.seed
for (i in 1:length(cl)) {
  s = nextRNGStream(s)
  clusterExport(cl[i], c('s'))
  # send s to worker i as .Random.seed
}
invisible(clusterEvalQ(cl, .Random.seed <- s))
#---------------------------------------------------------------------------------------------------#

# Put relevant packages and functions on cluster
invisible(clusterEvalQ(cl, library(data.table)))
invisible(clusterEvalQ(cl, setNumericRounding(0)))
invisible(clusterEvalQ(cl, library(nloptr)))
invisible(clusterEvalQ(cl, setwd('~/Keurig')))
invisible(clusterEvalQ(cl, source('Scripts/Two-Step-Demand/Machine-Adoption/UMax-functions.R')))

#---------------------------------------------------------------------------------------------------#
# Load Datasets

# Load Household information data
load(paste(meta_dir, "/HH.RData", sep=""))
setkey(hh, household_code, panel_year)

# Load household coffee purchasing trips data
load(paste(mlogit_dir, "/hh_trip_panel.RData", sep=""))

# Load retailer price data panel
load(paste(mlogit_dir, "/Retailer_Brand_Price_Panel.RData", sep=""))

# Load purchase data to obtain the estimates for budget 
load(paste(HMS_input_dir, "/HMS-Purchases-Coffee.RData", sep=""))

#---------------------------------------------------------------------------------------------------#
# Four Steps
# 1. Compute the probability of visiting each retailer or no visit
# 2. Probability of purchase given inventory and inventory type
# 3. Compute the distribution of coffee spending -- assuming lognormal spending distribution.
# 3. Compute the utility gain at each retailer in each week for each household before adoption


# Brand Descriptions 
brands = c("CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GREEN MOUNTAIN KEURIG", "MAXWELL HOUSE", 
           "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")
xnames = c("0OTHER", brands, "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
           "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")

# Obtain the trip probabilities
hh_panel[, quarter:= paste(format(week_end, "%y/"), 0, 
                           sub( "Q", "", quarters(week_end) ), sep = "")]
hh_trip_prob = hh_panel[, .(ntrips = .N), by = c("household_code", "panel_year", 
                                                 "quarter", "retailer_code")]
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
retailer_panel = retailer_panel[brand_descr!="0NOTHING", ]
retailer_panel[, quarter:= paste(format(week_end, "%y/"), 0, 
                                 sub( "Q", "", quarters(week_end) ), sep = "")]
retailer_panel[keurig==1,  brand_descr := paste(brand_descr, "KEURIG")]
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
retailer_panel[, `:=`(lightR = as.integer(roast==1),
                      mediumR = as.integer(roast==2),
                      medDR = as.integer(roast==3),
                      darkR = as.integer(roast==4),
                      assorted = as.integer(roast==0))]
# Brand types
owned_brands = c("KEURIG KEURIG", "GREEN MOUNTAIN KEURIG")
licensed_brands = c("CARIBOU KEURIG", "NEWMAN'S OWN ORGANICS KEURIG", "EIGHT O'CLOCK KEURIG")
thirdp_brands = setdiff(unique(retailer_panel[keurig==1, brand_descr]), 
                        c(owned_brands, licensed_brands))
retailer_panel[, `:=`(owned=as.integer(brand_descr%in%owned_brands),
                      licensed=as.integer(brand_descr%in%licensed_brands),
                      thirdp=as.integer(brand_descr%in%thirdp_brands))]
setkey(retailer_panel, dma_code, quarter, retailer_code, week_end)

# Compute the probability of making a purchase
# Predict probability of purchase once the consumer adopt Keurig
binomP <- function(form){
  binomreg = glm(form, family = "binomial")
  bprob = predict(binomreg, type = "response") 
  return(bprob)
}
hh_panel = hh_panel[, .(coffee_trip = sum(coffee_trip, na.rm=TRUE), k_first_week_end = k_first_week_end[1], 
                        brand_type_lag = brand_type_lag[1], ptype_lag=ptype_lag[1], inv_type=inv_type[1]),
                    by = c("household_code", "week_end", "quarter", "panel_year")]
hh_panel[, `:=`(coffee_trip = as.integer(coffee_trip>=1), 
                keurig_lag = as.integer(grepl("KEURIG",ptype_lag)))]
hh_panel[is.na(keurig_lag), keurig_lag:=0]
hh_panel[is.na(inv_type), inv_type:=0]
hh_panel[, pprob := binomP(coffee_trip~inv_type*keurig_lag), by = c("household_code")]
setkey(hh_panel, household_code, week_end)

#Remove not needed datasets and garbage collection
gc()

# Take the last part of markov chain to estimate consumer preferences
indx = seq(1001, 2500, 1)
load('Data/Bayes-MCMC/MDCEV-MCMC-All-30000.RData')
bindv[, ,27:28] = exp(bindv[, ,27:28])/(1+exp(bindv[, ,27:28]))
pref = colMeans(bindv[indx, ,], 2)
rownames(pref) = hh_code_list[, household_code]

# Obtain mean and variance of spending in coffee for each trip -- 
# caveat (consumer spend more after adoption !!!)
hh_spent = purchases[product_module_code==1463, .(total_price_paid = sum(total_price_paid), 
                                                  coupon_value = sum(coupon_value)),
                     by = c("household_code", "trip_code_uc")]
hh_spent = hh_spent[total_price_paid>0.01, ]
hh_spent = hh_spent[, .(E = mean(log(total_price_paid)), Esd = sd(log(total_price_paid))), 
                    by = "household_code"]
hh_spent = hh_spent[household_code%in%hh_code_list[, household_code], ]
hh_spent = hh_spent[, cbind(E, Esd, household_code)]
rownames(hh_spent) = hh_spent[, 3]
hh_spent = hh_spent[,1:2]
hh_spent = hh_spent[order(row.names(hh_spent)), ]
pref = pref[order(row.names(pref)), ]
pref = cbind(pref, hh_spent)

# Remove household panels with extremely high inventory
setkey(hh_panel, household_code, panel_year)
hh_panel = hh_panel[hh[, .(household_code, panel_year, dma_code)], nomatch=0L]
hh_panel = hh_panel[(is.na(k_first_week_end) | week_end<=k_first_week_end), ]
hh_panel = hh_panel[inv_type<=600&week_end>="2007-01-01"&dma_code%in%big_markets, ]
# Drop households who didn't adopt in top markets
hh_panel[, ftemp := sum(is.na(k_first_week_end)|k_first_week_end==week_end), by ="household_code"]
hh_panel = hh_panel[ftemp>=1, ]
hh_panel[, `:=`(ftemp=NULL, dma_code=NULL)]

#Obtain the list of correct households
hh_code_1 = as.integer(row.names(pref))
hh_code_2 = hh_panel[, unique(household_code)]
hh_codes = intersect(hh_code_1, hh_code_2)

# Filter data sets
hh_panel = hh_panel[household_code %in% hh_codes, ]
setkey(hh_panel, household_code, week_end)
hh_trip_prob = hh_trip_prob[panel_year>=2007, ]
hh_trip_prob = hh_trip_prob[household_code %in% hh_codes, ]
setkey(hh_trip_prob, household_code, dma_code, quarter, retailer_code)
retailer_panel = retailer_panel[week_end>="2007-01-01", ]
setkey(retailer_panel, dma_code, quarter, retailer_code, week_end)
gc()
save(hh_trip_prob, retailer_panel, hh_panel, pref, file = "Data/Machine-Adoption/MU-Diff-Asist.RData")

# Compute the adoption value consumer by consumer
xvars = c(paste0("a", 2:15), "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
          "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")
xnames = c("0OTHER", brands, "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
           "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")

hhValFun<-function(i){
  hh_prob_temp = hh_trip_prob[.(i), ]
  setkey(hh_prob_temp, dma_code, quarter, retailer_code)
  hh_retailers_temp = retailer_panel[hh_prob_temp, nomatch=0L]
  if (nrow(hh_retailers_temp)==0) return(data.table(NULL))
  setkey(hh_retailers_temp, week_end)
  hh_lags = hh_panel[.(i), ]
  setkey(hh_lags, week_end)
  hh_retailers_temp = hh_retailers_temp[hh_lags[,.(week_end, brand_type_lag, keurig_lag, pprob)], nomatch=0L]
  if (nrow(hh_retailers_temp)==0) return(data.table(NULL))
  hh_retailers_temp[, `:=`(brand_lag=as.integer(brand_descr%in%brand_type_lag & keurig==keurig_lag),
                           brand_lag_keurig=as.integer(brand_descr%in%brand_type_lag & keurig==keurig_lag)*keurig)]
  hh_retailers_temp[is.na(brand_lag), brand_lag:=0]
  hh_retailers_temp[is.na(brand_lag_keurig), brand_lag_keurig:=0]
  xmat = as.matrix(hh_retailers_temp[, xvars, with=FALSE])
  kmat = as.matrix(hh_retailers_temp[, cbind(1-keurig, keurig)])
  
  # Compute the quality of product
  hh_retailers_temp[, zb := xmat %*% pref[as.character(i),1:26]]
  # Compute the alpha of the product
  hh_retailers_temp[, alpha := kmat %*% pref[as.character(i),27:28]]
  nobs = nrow(hh_retailers_temp)
  
  # Number of retailer and week pair 
  hh_retailers_temp[, idx:=.GRP, by = .(dma_code, retailer_code, week_end)]
  setkey(hh_retailers_temp, idx)
  nw = hh_retailers_temp[, max(idx)]
  uall_cum = rep(0, nw)
  ugrd_cum = rep(0, nw)
  
  # HH week and retailer information 
  hh_agg = hh_retailers_temp[, .(household_code=household_code[1],retailer_code=retailer_code[1],
                                 week_end=week_end[1],tprob=tprob[1],pprob=pprob[1]), by = c("idx")]
  setkey(hh_agg, idx)
  
  # Monte Carlo Integration
  # Vectorization is the key
  starttime <- proc.time()
  for (j in 1:1000){
    # Simulate eps
    hh_retailers_temp[, eps:=-log(-log(runif(nobs)))]
    # Simulate E
    E = exp(rnorm(1, mean=pref[as.character(i), 29], sd= pref[as.character(i), 30]))
    
    # Compute the utility in the scenario
    hh_retailers_temp[, `:=`(UE = 1/price * exp(zb + eps)*(E/price+1)^(alpha-1),
                             U0 = 1/price * exp(zb + eps))]
    hh_retailers_temp[, `:=`(fil0 = as.integer(U0>max(UE))), by = .(idx)]
    dt_v0 = hh_retailers_temp[fil0>=0.9, .(ubar = log(eu2(min(UE-0.00001), max(U0+0.00001), 
                                                      alpha, zb, price, eps, E))), by = .(idx)]
    uall_cum[dt_v0$idx] = uall_cum[dt_v0$idx] + dt_v0$ubar
    hh_retailers_temp[, `:=`(fil1 = as.integer(U0>max(UE))), by = .(idx, keurig)]
    if (nrow(hh_retailers_temp[fil1>=0.9&keurig<=0.1,])==0){
      ugrd_cum = 0 #no ground, get no value
    } else{
      dt_v1 = hh_retailers_temp[fil1>=0.9&keurig<=0.1,
                                .(ubar=log(eu2(min(UE-0.00001), max(U0+0.00001), 
                                           alpha, zb, price, eps, E))), by = .(idx)]
      ugrd_cum[dt_v1$idx] = ugrd_cum[dt_v1$idx] + dt_v1$ubar
      ugrd_cum[-dt_v1$idx] = ugrd_cum[-dt_v1$idx] + 0 #no ground, get no value
    }
    #cat("Processed", j, "after", proc.time()-starttime, "seconds.\n\n")
  }
  hh_agg[, `:=`(uall=uall_cum/1000, ugrd=ugrd_cum/1000, idx=NULL)]
  hh_agg[, `:=`(mu_diff=uall-ugrd)]
  hh_agg = hh_agg[, .(mu_diff = sum(mu_diff * tprob)/sum(tprob),
                      uall = sum(uall * tprob)/sum(tprob),
                      ugrd = sum(ugrd * tprob)/sum(tprob)), 
                      by = c("household_code", "week_end", "pprob")]
  hh_agg = hh_agg[, .(mu_diff = mu_diff * pprob, uall = uall*pprob, ugrd=ugrd*pprob), 
                      by = c("household_code", "week_end")]
  return(hh_agg)
}
invisible(clusterEvalQ(cl, load('Data/Machine-Adoption/MU-Diff-Asist.RData')))
clusterExport(cl, c('pref', 'xvars', 'hhValFun'))
cval_list = parLapply(cl, hh_codes, hhValFun)
cval_list = rbindlist(cval_list)
save(cval_list, file = paste(output_dir, "/HH-Util-Diff.RData", sep=""))

load(paste(output_dir, "/HH-Util-Diff.RData", sep=""))
load(paste(output_dir, "/HH-HW-Panel.RData", sep=""))
# Merge consumption value back to the value function
setkey(cval_list, household_code, week_end)
setkey(hw_panel, household_code, week_end)
hw_market_panel = hw_panel[cval_list, nomatch=0L]

# Load market specific information.
load("Data/HMS-Summary/DMA-Panel.RData")
setkey(hw_market_panel, dma_code, week_end)
setkey(dma_panel, dma_code, week_end)
hw_market_panel = hw_market_panel[dma_panel[, .(dma_code, week_end, ashare, nbrand, thanksgiving,
                                                christmas, bchristmas, achristmas, mother, father)], nomatch=0L]
hw_market_panel = hw_market_panel[mu_diff>=0, ]
setkey(hw_market_panel, household_code, week_end)
save(hw_market_panel, file = paste(output_dir, "/HW-MU-Panel.RData", sep=""))

# Export Relevant Data sets to csv format, and use them for julia estimation
hw_market_panel[, `:=`(ntrip =.GRP), by = c("household_code", "week_end")]
setkey(hw_market_panel, ntrip)
write.csv(hw_market_panel[,.(household_code, hware, ntrip, t, price, price_avg, price_avgn,
                             mu_diff, purchased, ashare, thanksgiving, christmas, 
                             bchristmas, achristmas, mother, father, nbrand)],
          file = "Data/Machine-Adoption/HW-MU-Panel.csv", row.names = FALSE)

# Estimate the parameter governing the evolution process of mu_diff
# Both time and individual plays a very small role in determining the next period mu_diff.
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
