#####################################################################################################
#
# Expected Revenue
# Xiliang Lin
# Jan, 2017
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
library(plm)
library(data.table)
setNumericRounding(0)
library(nloptr)

# Set Working Folder Path Here
setwd("~/Keurig")
meta_dir  = "Data/Meta-Data"
HMS_input_dir = "Data/HMS-Transactions"
mlogit_dir = "Data/MLogit-Data"
input_dir = "Data/Machine-Adoption"
output_dir = "Data/Counterfactual"

# Conditional on Adoption
cond = FALSE

# Set seed
RNGkind("L'Ecuyer-CMRG")
set.seed(12345)

# Source function
source('Scripts/Two-Step-Demand/Machine-Adoption/UMax-functions.R')
Rcpp::sourceCpp('Scripts/Two-Step-Demand/Machine-Adoption/CoffeeValue.cpp')

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
invisible(clusterEvalQ(cl, Rcpp::sourceCpp('Scripts/Two-Step-Demand/Machine-Adoption/CoffeeValue.cpp')))

#---------------------------------------------------------------------------------------------------#
# Load Datasets
load("Data/Machine-Adoption/MU-Diff-Asist-NoAdjust.RData")
if (cond){
  hh_adpt_list = hh_panel[adoption==1, unique(household_code)]
  hh_demo = hh_demo[household_code%in%hh_adpt_list, ]
  hh_panel = hh_panel[household_code%in%hh_adpt_list, ]
}

# Brand start and end dates
anames = c(paste0("a", 2:16), "keurig")
k_brand_dates = retailer_panel[keurig==1, .(first_week = min(week_end), last_week = max(week_end)),
                               by = c("dma_code", "brand_descr", "brand_descr_orig", anames)] 

#---------------------------------------------------------------------------------------------------#
# Compute the adoption value consumer by consumer
brands = c("OTHER", "CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GREEN MOUNTAIN KEURIG", "MAXWELL HOUSE", 
           "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")
k_specific = c("CARIBOU", "DONUT HOUSE", "FOLGERS", "GREEN MOUNTAIN", "NEWMAN'S OWN ORGANICS", 
               "STARBUCKS", "TULLY'S")
bspec_ind = which(grepl("KEURIG", brands))
bgen_ind = c(1, 3, 4, 6, 7, 11)
bindx = sort(c(bgen_ind, bspec_ind))

xvars = c(paste0("a", 2:16), "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
          "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")
xnames = c("0NOTHING", brands, "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
           "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")
clusterExport(cl, c('brands', 'anames', 'k_specific', 'bspec_ind', 'bgen_ind', 'bindx', 'xvars', 'pref'))

NoQualityImprove<-function(i, n=1000){
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
  i_pref = pref[as.character(i), ]
  i_pref[16] = ifelse(i_pref[16]<=0, i_pref[16], 0)
  # Compute the quality of product
  hh_retailers_temp[, zb := xmat %*% i_pref[1:27]]
  # Compute the alpha of the product
  hh_retailers_temp[, alpha := kmat %*% i_pref[28:29]]
  # Compute the rho
  hh_retailers_temp[, rho := i_pref[30]]
  
  # Number of retailer and week pair 
  setkey(hh_retailers_temp, household_code, retailer_code, week_end)
  hh_agg = hh_retailers_temp[, .(av = list(mc_vfun(zb, alpha, rho, price, keurig, n)), 
                                 tprob=tprob[1], pprob=pprob[1], pprob1=pprob1[1], pprob2=pprob2[1]), 
                             by = .(household_code, dma_code, retailer_code, week_end)]
  hh_agg[, `:=`(av=sapply(av, "[[", 1), gv=sapply(av, "[[", 2))]
  hh_agg = hh_agg[, .(av=sum(av*tprob)/sum(tprob), gv=sum(gv*tprob)/sum(tprob), 
                      pprob=pprob[1], pprob1=pprob1[1], pprob2=pprob2[1]), 
                  by = c("household_code", "week_end")]
  return(hh_agg)
}


NoBestMatch<-function(i, n=1000){
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
  
  # Preference modification
  i_pref = pref[as.character(i), ]
  bmat = as.matrix(hh_retailers_temp[, anames, with=FALSE])
  hh_retailers_temp[, `:=`(bintercept = bmat%*%i_pref[1:16])]
  hh_retailers_temp[keurig==1, `:=`(bmax_ind = (bintercept == max(bintercept)),
                                    bmean_ind = mean(bintercept)), 
                    by = c("week_end")]
  hh_retailers_temp[keurig==0, bmax_ind := FALSE]
  hh_retailers_temp[, badjust := ifelse(bmax_ind == TRUE, bmean_ind-bintercept, 0)]
  
  xmat = as.matrix(hh_retailers_temp[, xvars, with=FALSE])
  kmat = as.matrix(hh_retailers_temp[, cbind(1-keurig, keurig)])
  xmat = cbind(xmat, hh_retailers_temp$badjust)
  
  # Compute the quality of product
  hh_retailers_temp[, zb := xmat %*% c(i_pref[1:27], 1)]
  # Compute the alpha of the product
  hh_retailers_temp[, alpha := kmat %*% i_pref[28:29]]
  # Compute the rho
  hh_retailers_temp[, rho := i_pref[30]]
  
  # Number of retailer and week pair 
  setkey(hh_retailers_temp, household_code, retailer_code, week_end)
  hh_agg = hh_retailers_temp[, .(av = list(mc_vfun(zb, alpha, rho, price, keurig, n)), 
                                 tprob=tprob[1], pprob=pprob[1], pprob1=pprob1[1], pprob2=pprob2[1]), 
                             by = .(household_code, dma_code, retailer_code, week_end)]
  hh_agg[, `:=`(av=sapply(av, "[[", 1), gv=sapply(av, "[[", 2))]
  hh_agg = hh_agg[, .(av=sum(av*tprob)/sum(tprob), gv=sum(gv*tprob)/sum(tprob), 
                      pprob=pprob[1], pprob1=pprob1[1], pprob2=pprob2[1]), 
                  by = c("household_code", "week_end")]
  return(hh_agg)
}

OnlyBestMatch<-function(i, n=1000){
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
  
  # Find the best match and eliminate the rest
  i_pref = pref[as.character(i), ]
  bmat = as.matrix(hh_retailers_temp[, anames, with=FALSE])
  hh_retailers_temp[, `:=`(bintercept = bmat%*%i_pref[1:16])]
  hh_retailers_temp[keurig==1, `:=`(bmax_ind = (bintercept == max(bintercept))), 
                    by = c("week_end")]
  hh_retailers_temp[keurig==0, bmax_ind := TRUE]
  hh_retailers_temp = hh_retailers_temp[bmax_ind==TRUE, ]

  xmat = as.matrix(hh_retailers_temp[, xvars, with=FALSE])
  kmat = as.matrix(hh_retailers_temp[, cbind(1-keurig, keurig)])
  
  # Compute the quality of product
  hh_retailers_temp[, zb := xmat %*% i_pref[1:27]]
  # Compute the alpha of the product
  hh_retailers_temp[, alpha := kmat %*% i_pref[28:29]]
  # Compute the rho
  hh_retailers_temp[, rho := i_pref[30]]
  
  # Number of retailer and week pair 
  setkey(hh_retailers_temp, household_code, retailer_code, week_end)
  hh_agg = hh_retailers_temp[, .(av = list(mc_vfun(zb, alpha, rho, price, keurig, n)), 
                                 tprob=tprob[1], pprob=pprob[1], pprob1=pprob1[1], pprob2=pprob2[1]), 
                             by = .(household_code, dma_code, retailer_code, week_end)]
  hh_agg[, `:=`(av=sapply(av, "[[", 1), gv=sapply(av, "[[", 2))]
  hh_agg = hh_agg[, .(av=sum(av*tprob)/sum(tprob), gv=sum(gv*tprob)/sum(tprob), 
                      pprob=pprob[1], pprob1=pprob1[1], pprob2=pprob2[1]), 
                  by = c("household_code", "week_end")]
  return(hh_agg)
}

# Run the codes
invisible(clusterEvalQ(cl, load('Data/Machine-Adoption/MU-Diff-Asist-NoAdjust.RData')))
clusterExport(cl, c('NoQualityImprove', 'NoBestMatch', 'OnlyBestMatch'))
cval_NoQ = parLapply(cl, hh_codes, NoQualityImprove, n=1000)
save(cval_NoQ, file = paste0(output_dir, "/Delta-NoQualityImprovement-NoAdjust-1000-Week.RData"))

cval_NoB = parLapply(cl, hh_codes, NoBestMatch, n=1000)
save(cval_NoB, file = paste0(output_dir, "/Delta-NoBestMatch-NoAdjust-1000-Week.RData"))

cval_OnB = parLapply(cl, hh_codes, OnlyBestMatch, n=1000)
save(cval_OnB, file = paste0(output_dir, "/Delta-OnlyBestMatch-NoAdjust-1000-Week.RData"))

load(paste0(output_dir, "/Delta-NoBestMatch-NoAdjust-100-Week.RData"))
load(paste0(output_dir, "/Delta-OnlyBestMatch-NoAdjust-100-Week.RData"))
load(paste0(output_dir, "/HH-Choice-Value-NoAdjust.RData"))
cval_NoB = rbindlist(cval_NoB)
cval_OnB = rbindlist(cval_OnB)

setkey(cval_list, household_code, week_end)
setkey(cval_NoB, household_code, week_end)
setkey(cval_OnB, household_code, week_end)

cval_list[, `:=`(mu_diff_orig = (av-gv)*pprob1)]
cval_NoB[, `:=`(mu_diff_nob = (av-gv)*pprob1)]
cval_OnB[, `:=`(mu_diff_onb = (av-gv)*pprob1)]
cval_list[, `:=`(pprob1 = NULL, pprob2=NULL, pprob=NULL)]

cval_list = cval_list[cval_NoB[, .(household_code, week_end, mu_diff_nob)], nomatch=0L]
cval_list = cval_list[cval_OnB[, .(household_code, week_end, mu_diff_onb)], nomatch=0L]
setkey(cval_list, household_code, week_end)

cval_list[, mu_diff_orig_lag := c(NA, mu_diff_orig[1:(length(mu_diff_orig)-1)]), by = "household_code"]
cval_list[, mu_diff_nob_lag := c(NA, mu_diff_nob[1:(length(mu_diff_nob)-1)]), by = "household_code"]
cval_list[, mu_diff_onb_lag := c(NA, mu_diff_onb[1:(length(mu_diff_onb)-1)]), by = "household_code"]

load(paste0(input_dir, "/HW-Full-Panel.RData"))

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
setkey(hw_market_panel, household_code, week_end)

# Export Relevant Data sets to csv format, and use them for julia estimation
hw_market_panel[, `:=`(ntrip =.GRP), by = c("household_code", "week_end")]
setkey(hw_market_panel, ntrip)
write.csv(hw_market_panel[,.(household_code, hware, ntrip, t, price, price_avg, price_avgn,
                             mu_diff_orig, mu_diff_nob, mu_diff_onb, thanksgiving, christmas, 
                             bchristmas, achristmas, mother, father, ashare, nbrand)],
          file = "Data/Counterfactual/HW-MU-Decomp-NoAdjust.csv", row.names = FALSE)

feregorig = plm(log(mu_diff_orig+1)~log(mu_diff_orig_lag+1), data = hw_market_panel, index = c("household_code", "week_end"), model = "within")
feregnob = plm(log(mu_diff_nob+1)~log(mu_diff_nob_lag+1), data = hw_market_panel, index = c("household_code", "week_end"), model = "within")
feregonb = plm(log(mu_diff_onb+1)~log(mu_diff_onb_lag+1), data = hw_market_panel, index = c("household_code", "week_end"), model = "within")
summary(feregorig)
mean(fixef(feregorig))
summary(feregnob)
mean(fixef(feregnob))
summary(feregonb)
mean(fixef(feregonb))

# Load the probabilities
probv_orig = fread(paste0(output_dir, "/noadjust_decomp_type_1.csv"))
probv_nob = fread(paste0(output_dir, "/noadjust_decomp_type_2.csv"))
probv_onb = fread(paste0(output_dir, "/noadjust_decomp_type_3.csv"))
setnames(probv_orig, paste0("V", 1:3), c("household_code", "t", "prob"))
setnames(probv_nob, paste0("V", 1:3), c("household_code", "t", "prob"))
setnames(probv_onb, paste0("V", 1:3), c("household_code", "t", "prob"))
probv_orig[, Scenario:="Estimated Adoption Rate"]
probv_nob[, Scenario:="No Best Match Brand"]
probv_onb[, Scenario:="Only Best Match Brand"]
probv = rbindlist(list(probv_orig, probv_nob, probv_onb))
probv = probv[!is.na(prob), ]
probv = probv[, prob := 1-prob]
setkeyv(probv, c("Scenario", "household_code", "t"))
probv = probv[, prob := cumprod(prob), by = c("Scenario","household_code")]
# probv = probv[, .(prob = 1-mean(prob)), by = c("Scenario", "t")]
setkeyv(probv, c("Scenario", "t"))
probv = probv[, .(rate = mean(prob)), by = c("Scenario", "t")]
probv[, rate := 1-rate]
week_dt = hw_market_panel[, .(week_end = week_end[1]), by = "t"]
setkey(week_dt, t)
setkey(probv, t)
probv = probv[week_dt, nomatch=0L]
setkeyv(probv, c("Scenario", "t"))
ggplot(probv, aes(x = week_end, y = rate, colour = Scenario, linetype = Scenario)) + 
  geom_line() + theme_minimal()+theme(legend.justification=c(0,1), legend.position=c(0,1)) + 
  labs(x = "Time (Week)", y = "Adoption Rate")

load(paste0(output_dir, "/Delta-NoBestMatch-NoAdjust-100-Week.RData"))
load(paste0(output_dir, "/Delta-OnlyBestMatch-NoAdjust-100-Week.RData"))
load(paste0(output_dir, "/HH-Choice-Value-NoAdjust.RData"))

cval_list[, `:=`(mu_diff = (av-gv)*pprob1)]
mean_gain = cval_list[, .(delta25 = quantile(mu_diff, 0.25), delta50 = median(mu_diff), 
                          delta75 = quantile(mu_diff, 0.75)), by = "week_end"]
mean_gain[, Type:= "Original Option Value"]
setkey(mean_gain, week_end)

cval_NoB = rbindlist(cval_NoB)
cval_NoB[, `:=`(mu_diff = (av-gv)*pprob1)]
mean_gain_NoB = cval_NoB[, .(delta25 = quantile(mu_diff, 0.25), delta50 = median(mu_diff), 
                             delta75 = quantile(mu_diff, 0.75)), by = "week_end"]
mean_gain_NoB[, Type := "No Best Match"]
setkey(mean_gain_NoB, week_end)

cval_OnB = rbindlist(cval_OnB)
cval_OnB[, `:=`(mu_diff = (av-gv)*pprob1)]
mean_gain_OnB = cval_OnB[, .(delta25 = quantile(mu_diff, 0.25), delta50 = median(mu_diff), 
                             delta75 = quantile(mu_diff, 0.75)), by = "week_end"]
mean_gain_OnB[, Type := "Only Best Match"]
setkey(mean_gain_OnB, week_end)

mean_gain = rbindlist(list(mean_gain, mean_gain_NoB, mean_gain_OnB))
mean_gain[, Type := factor(Type, levels = c("Original Option Value", "No Best Match", "Only Best Match"))]

# Plot these
ggplot(mean_gain, aes(x = week_end, y = delta50, colour = Type, linetype = Type)) + 
  geom_line() + theme_minimal()+theme(legend.justification=c(0,1), legend.position=c(0,1)) + 
  labs(x = "Time (Week)", y = "Median Option Value")
rowMax <- function(data) apply(data, 1, max, na.rm = TRUE)
prefm = rowMax(pref[, bspec_ind])
for (i in bspec_ind){
  cat("Brand", brands[i])
  print(table(pref[,i]>=prefm))
  cat("\n")
}
