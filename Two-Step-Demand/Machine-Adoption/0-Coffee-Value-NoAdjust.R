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
library(bayesm)
library(MASS)
setNumericRounding(0)
library(nloptr)
library(plm)

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
Rcpp::sourceCpp('Scripts/Two-Step-Demand/Machine-Adoption/CoffeeValue.cpp')

#---------------------------------------------------------------------------------------------------#
# Initialize Parallel Execution Environment
primary <- 'bushgcn30'
machineAddresses <- list(
  list(host=primary, user='xlin0',
       ncore=28),
  list(host='bushgcn31',user='xlin0',
       ncore=28),
  list(host='bushgcn32',user='xlin0',
       ncore=28)
)

spec <- lapply(machineAddresses,
               function(machine) {
                 rep(list(list(host=machine$host,
                               user=machine$user)),
                     machine$ncore)
               })
spec <- unlist(spec,recursive=FALSE)
cl <- makeCluster(type='PSOCK', master=primary, spec=spec)
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
invisible(clusterEvalQ(cl, library(bayesm)))
invisible(clusterEvalQ(cl, library(MASS)))
invisible(clusterEvalQ(cl, library(nloptr)))
invisible(clusterEvalQ(cl, library(plm)))
invisible(clusterEvalQ(cl, library(Rcpp)))
invisible(clusterEvalQ(cl, library(RcppArmadillo)))
invisible(clusterEvalQ(cl, setwd('~/Keurig')))
invisible(clusterEvalQ(cl, source('Scripts/Two-Step-Demand/Machine-Adoption/UMax-functions.R')))
invisible(clusterEvalQ(cl, Rcpp::sourceCpp('Scripts/Two-Step-Demand/Machine-Adoption/CoffeeValue.cpp')))
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

# load demographics
load('Data/Bayes-MCMC/Demographics.RData')
#---------------------------------------------------------------------------------------------------#
# Four Steps
# 1. Compute the probability of visiting each retailer or no visit
# 2. Probability of purchase given inventory and inventory type
# 3. Compute the distribution of coffee spending -- assuming lognormal spending distribution.
# 4. Compute the utility gain at each retailer in each week for each household before adoption

# Brand Descriptions 
brands = c("CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GREEN MOUNTAIN KEURIG", "MAXWELL HOUSE", 
           "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")
k_specific = c("CARIBOU", "DONUT HOUSE", "FOLGERS", "GREEN MOUNTAIN", "NEWMAN'S OWN ORGANICS", 
               "STARBUCKS", "TULLY'S")
xnames = c("0NOTHING", brands, "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
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
retailer_panel = retailer_panel[((!is.na(price)) & price>0.02 & price<2.00)|brand_descr=="0NOTHING",  ]
retailer_panel[, quarter:= paste(format(week_end, "%y/"), 0, 
                                 sub( "Q", "", quarters(week_end) ), sep = "")]
retailer_panel[, brand_descr:=brand_descr_orig]
retailer_panel[keurig==1 & brand_descr_orig%in%k_specific, brand_descr := paste(brand_descr_orig, "KEURIG")]
# Adapt the panel to create the X matrix
retailer_panel[, a2:=as.integer(!brand_descr%in%brands)]
alist = paste0("a", c(3:(length(brands)+2)))
i = 0
for (a in alist){
  i = i+1
  retailer_panel[, v:=as.integer(brand_descr==brands[i])]
  setnames(retailer_panel, "v", a)
}
retailer_panel[, `:=`(lightR = as.integer(roast==1),
                      mediumR = as.integer(roast==2),
                      medDR = as.integer(roast==3),
                      darkR = as.integer(roast==4),
                      assorted = as.integer(roast==0),
                      nbrand = log(nbrand))]
setkey(retailer_panel, dma_code, quarter, retailer_code, week_end)
# retailer_panel[grepl(" KEURIG", brand_descr), keurig:=0]

# Brand types
retailer_panel[keurig==1 & !(brand_descr_orig%in%k_specific), brand_descr := paste(brand_descr_orig, "KEURIG")]
owned_brands = c("KEURIG KEURIG", "GREEN MOUNTAIN KEURIG")
licensed_brands = c("CARIBOU KEURIG", "NEWMAN'S OWN ORGANICS KEURIG", "EIGHT O'CLOCK KEURIG")
thirdp_brands = setdiff(unique(retailer_panel[keurig==1|grepl("KEURIG", brand_descr), brand_descr]), 
                        c(owned_brands, licensed_brands))
retailer_panel[, `:=`(owned=as.integer(brand_descr%in%owned_brands),
                      licensed=as.integer(brand_descr%in%licensed_brands),
                      thirdp=as.integer(brand_descr%in%thirdp_brands))]
retailer_panel[brand_descr_orig=="CTL BR", brand_type := paste(brand_descr_orig, retailer_code, sep="_")]
retailer_panel[brand_descr_orig!="CTL BR", brand_type := brand_descr_orig]
retailer_panel[, `:=`(brand_type = paste(brand_type, ptype, sep = "_"), nobs = 1:.N)]

# Compute the probability of making a purchase
# Predict probability of purchase once the consumer adopt Keurig
binomP <- function(form){
  binomreg = glm(form, family = binomial(link='logit'))
  bprob = predict(binomreg, type = "response") 
  return(bprob)
}
hh_panel = hh_panel[, .(coffee_trip = sum(coffee_trip, na.rm=TRUE), k_first_week = k_first_week[1], 
                        brand_type_lag = brand_type_lag[1], ptype_lag=ptype_lag[1], inv_type=inv_type[1]),
                    by = c("household_code", "week_end", "quarter", "panel_year")]
hh_panel[, `:=`(adoption = ifelse(is.na(k_first_week), 0, as.integer(week_end>=k_first_week)),
                keurig_lag = as.integer(grepl("KEURIG", ptype_lag)),
                coffee_trip = as.integer(coffee_trip>=1), iota = 1)]
hh_panel[is.na(keurig_lag), keurig_lag:=0]
hh_panel[is.na(inv_type), inv_type:=0]
hh_panel = hh_panel[inv_type<=950, ]
demo_hh_list = hh_demo[, household_code]
panel_hh_list = hh_panel[, unique(household_code)]
selected_hh = intersect(demo_hh_list, panel_hh_list)
hh_panel = hh_panel[household_code%in%selected_hh, ]
hh_demo = hh_demo[household_code%in%selected_hh, ]
setkey(hh_panel, household_code, week_end)

# Individual specific logistic
# hh_panel[, pprob := binomP(coffee_trip~inv_type*keurig_lag), by = c("household_code")]
# Use Bayesm to allow predictions when the customer doesn't adopt the keurig machine. 
znames = c("inc40", "inc50", "inc60", "inc70",
           "hhsize2", "hhsize3", "hhsize5", "twofamily", "threefamily",
           "fulltime", "presence_of_children",
           "african_american", "hispanic", "total_spent", "total_spent_hhi")
pnames = c("adoption", "inv_type")
Names = list(id = "household_code", xnames = pnames, yname = "coffee_trip", znames=znames)
MCMC = list(R=5000, thin=2, sample_ratio=0.05)
Par = list(cl=cl)
# Data
# out = BinaryLogitSampler(hh_panel, ZD = hh_demo, Names, MCMC, Par)
# Predict choice probabilities for each household
k = 0
for (h in selected_hh){
  k = k+1
  DMat = t(Draws$bindv[3001:5000,k,])
  hh_panel[.(h), `:=`(pprob = rowMeans(exp(cbind(iota, adoption, inv_type)%*%DMat)/(1+exp(cbind(iota, adoption, inv_type)%*%DMat))),
                      pprob1 = rowMeans(exp(cbind(iota, 0, inv_type)%*%DMat)/(1+exp(cbind(iota, 0, inv_type)%*%DMat))),
                      pprob2 = rowMeans(exp(cbind(iota, 1, inv_type)%*%DMat)/(1+exp(cbind(iota, 1, inv_type)%*%DMat))))]
}

# Take the last part of markov chain to estimate consumer preferences
indx = seq(1, 2000, 1)
load('Data/Bayes-MCMC/MDCEV-MCMC-OX-NoAdjust.RData')
bindv[, ,28:30] = exp(bindv[, ,28:30])/(1+exp(bindv[, ,28:30]))
pref = colMeans(bindv[indx, ,], 2)
rownames(pref) = hh_code_list[, household_code]
pref = pref[order(row.names(pref)), ]
rm(bindv)
gc()

# Remove household panels with extremely high inventory
setkey(hh_panel, household_code, panel_year)
hh_panel = hh_panel[hh[, .(household_code, panel_year, dma_code)], nomatch=0L]
# hh_panel = hh_panel[(is.na(k_first_week) | week_end<=k_first_week), ]
hh_panel = hh_panel[inv_type<=950&week_end>="2007-01-01"&dma_code%in%big_markets, ]
# Drop households who didn't adopt in top markets
# hh_panel[, ftemp := sum(is.na(k_first_week)|k_first_week==week_end), by ="household_code"]
# hh_panel = hh_panel[ftemp>=1, ]
# hh_panel[, `:=`(ftemp=NULL, dma_code=NULL)]

#Obtain the list of correct households
hh_code_1 = as.integer(row.names(pref))
hh_code_2 = hh_panel[, unique(household_code)]
hh_codes = intersect(hh_code_1, hh_code_2)

# Filter data sets
hh_panel = hh_panel[household_code %in% hh_codes, ]
setkey(hh_panel, household_code, week_end)
hh_trip_prob = hh_trip_prob[panel_year>=2008, ]
hh_trip_prob = hh_trip_prob[household_code %in% hh_codes, ]
setkey(hh_trip_prob, household_code, dma_code, quarter, retailer_code)
retailer_panel = retailer_panel[week_end>="2008-01-01", ]
setkey(retailer_panel, dma_code, quarter, retailer_code, week_end)
gc()
save(hh_trip_prob, hh_demo, retailer_panel, hh_panel, pref, bhatd, hh_codes, 
     file = "Data/Machine-Adoption/MU-Diff-Asist-NoAdjust.RData")

# Compute the adoption value consumer by consumer
xvars = c(paste0("a", 2:16), "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
          "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")
xnames = c("0NOTHING", brands, "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
           "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")

hhValFun<-function(i, n=1000){
  hh_prob_temp = hh_trip_prob[.(i), ]
  setkey(hh_prob_temp, dma_code, quarter, retailer_code)
  hh_retailers_temp = retailer_panel[hh_prob_temp, nomatch=0L]
  if (nrow(hh_retailers_temp)==0) return(data.table(NULL))
  setkey(hh_retailers_temp, week_end)
  hh_lags = hh_panel[.(i), ]
  setkey(hh_lags, week_end)
  hh_retailers_temp = hh_retailers_temp[hh_lags[,.(week_end, brand_type_lag, pprob, pprob1, pprob2)], nomatch=0L]
  if (nrow(hh_retailers_temp)==0) return(data.table(NULL))
  hh_retailers_temp[, `:=`(brand_lag = as.integer(grepl(brand_type, brand_type_lag))), by = "nobs"]
  hh_retailers_temp[, `:=`(brand_lag_keurig = brand_lag*grepl("KEURIG", brand_lag))]
  hh_retailers_temp[is.na(brand_lag), brand_lag:=0]
  hh_retailers_temp[is.na(brand_lag_keurig), brand_lag_keurig:=0]
  xmat = as.matrix(hh_retailers_temp[, xvars, with=FALSE])
  kmat = as.matrix(hh_retailers_temp[, cbind(1-keurig, keurig)])
  
  # Compute the quality of product
  hh_retailers_temp[, zb := xmat %*% pref[as.character(i),1:27]]
  # Compute the alpha of the product
  hh_retailers_temp[, alpha := kmat %*% pref[as.character(i), 28:29]]
  # Compute the rho
  hh_retailers_temp[, rho := pref[as.character(i),30]]
  
  # Number of retailer and week pair 
  setkey(hh_retailers_temp, household_code, retailer_code, week_end)
  hh_agg = hh_retailers_temp[, .(av = list(mc_vfun(zb, alpha, rho, price, keurig, n)), 
                                 tprob=tprob[1], pprob=pprob[1], pprob1=pprob1[1], pprob2=pprob2[1]), 
                             by = .(household_code, dma_code, retailer_code, week_end)]
  hh_agg[, `:=`(av=sapply(av, "[[", 1), gv=sapply(av, "[[", 2))]
  hh_agg = hh_agg[, .(av=sum(av*tprob)/sum(tprob), gv=sum(gv*tprob)/sum(tprob), 
                      pprob=pprob[1], pprob1=pprob1[1], pprob2=pprob2[1]), 
                  by = c("household_code", "week_end")]
  #hh_agg[, `:=`(av = av*pprob2, gv = gv*pprob1)]
  #hh_agg[, `:=`(mu_diff = av-gv)]
  return(hh_agg)
}
invisible(clusterEvalQ(cl, load('Data/Machine-Adoption/MU-Diff-Asist-NoAdjust.RData')))
clusterExport(cl, c('pref', 'xvars', 'hhValFun'))
cval_list = parLapply(cl, hh_codes, hhValFun, n=300)
cval_list = rbindlist(cval_list)
save(cval_list, file = paste(output_dir, "/HH-Choice-Value-NoAdjust.RData", sep=""))
cval_list[, `:=`(mu_diff = (av-gv)*pprob1)]
bds = cval_list[, quantile(mu_diff, c(0.001, 0.999))]
cval_list = cval_list[mu_diff<=bds[2] & mu_diff>=bds[1],]
mean_gain = cval_list[, .(delta = mean(mu_diff), dvar = var(mu_diff)/.N, sdv = sd(mu_diff)), by = "week_end"]
setkey(mean_gain, week_end)
save(cval_list, mean_gain, file = paste(output_dir, "/HH-Util-Diff-NoAdjust.RData", sep=""))

load(paste(output_dir, "/HH-Util-Diff-NoAdjust.RData", sep=""))
load(paste(output_dir, "/HW-Full-Panel.RData", sep=""))
# load(paste(output_dir, "/HH-HW-Panel.RData", sep=""))
setkey(cval_list, household_code, week_end)
setkey(hw_panel, household_code, week_end)
cval_list[, mu_diff:=(av-gv)*pprob1]
cval_list[, mu_diff_lag := c(NA, mu_diff[1:(length(mu_diff)-1)]), by = "household_code"]
cval_list[, mu_diff_lag_2 := c(NA, mu_diff_lag[1:(length(mu_diff_lag)-1)]), by = "household_code"]

# Merge consumption value back to the value function
hw_market_panel = hw_panel[cval_list, nomatch=0L]

# Load market specific information.
load("Data/HMS-Summary/DMA-Panel.RData")
setkey(hw_market_panel, dma_code, week_end)
setkey(dma_panel, dma_code, week_end)
hw_market_panel = hw_market_panel[dma_panel[, .(dma_code, week_end, ashare, nbrand, thanksgiving,
                                                christmas, bchristmas, achristmas, mother, father)], nomatch=0L]
hw_market_panel = hw_market_panel[mu_diff>=0, ]
setkey(hw_market_panel, household_code, week_end)
save(hw_market_panel, file = paste(output_dir, "/HW-MU-Panel-NoAdjust.RData", sep=""))

# Export Relevant Data sets to csv format, and use them for julia estimation
hw_market_panel[, `:=`(ntrip =.GRP), by = c("household_code", "week_end")]
setkey(hw_market_panel, ntrip)
write.csv(hw_market_panel[,.(household_code, hware, ntrip, t, price, price_avg, price_avgn,
                             mu_diff, purchased, thanksgiving, christmas, 
                             bchristmas, achristmas, mother, father, ashare, nbrand)],
          file = "Data/Machine-Adoption/HW-MU-Panel-NoAdjust.csv", row.names = FALSE)

# Estimate the parameter governing the evolution process of mu_diff
# Both time and individual plays a very small role in determining the next period mu_diff.
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

hw_markov_check = hw_market_panel[nobs>=52, .(p_lag1 = pval_lag1(log(mu_diff+1)~log(mu_diff_lag+1)+log(mu_diff_lag_2+1)),
                                              p_lag2 = pval_lag2(log(mu_diff+1)~log(mu_diff_lag+1)+log(mu_diff_lag_2+1)),
                                              gamma1 = gamma1fun(log(mu_diff+1)~log(mu_diff_lag+1)),
                                              gamma2 = gamma2fun(log(mu_diff+1)~log(mu_diff_lag+1)),
                                              sig = sigfun(mu_diff~mu_diff_lag)), 
                                  by = "household_code"]
regout = hw_market_panel[, lm(log(mu_diff+1)~log(mu_diff_lag+1))]
summary(regout)

#fereg0 = plm(mu_diff~mu_diff_lag, data = hw_market_panel, index = c("household_code", "week_end"), model = "within")
fereg0 = plm(log(mu_diff+1)~log(mu_diff_lag+1), data = hw_market_panel, index = c("household_code", "week_end"), model = "within")
#fereg1 = plm(mu_diff~mu_diff_lag+mu_diff_lag_2, data = hw_market_panel, index = c("household_code", "week_end"), model = "within")
#fereg2 = plm(mu_diff ~ mu_diff_lag, data = hw_market_panel, index = c("household_code", "week_end"), model = "random")
summary(fereg0)
mean(fixef(fereg0))

fereg1 = plm(log(price)~log(price_avg), data = hw_market_panel, index = c("household_code", "week_end"), model = "within")
summary(fereg1)
mean(fixef(fereg1))

