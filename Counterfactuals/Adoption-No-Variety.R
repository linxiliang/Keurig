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

# Set seed
RNGkind("L'Ecuyer-CMRG")
set.seed(12345)

# Source function
source('Scripts/Counterfactuals/machine-functions.R')

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
invisible(clusterEvalQ(cl, source('Scripts/Counterfactuals/machine-functions.R')))

#---------------------------------------------------------------------------------------------------#
# Load Datasets
load("Data/Machine-Adoption/MU-Diff-Asist.RData")

# No variety seeking
pref[, c(27,28)] = 1
pref[, 25] = 0
#---------------------------------------------------------------------------------------------------#
# Brand Descriptions 
brands = c("CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GREEN MOUNTAIN KEURIG", "MAXWELL HOUSE", 
           "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")
xnames = c("0OTHER", brands, "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
           "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")

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
  uall1_cum = rep(0, nw)
  uall2_cum = rep(0, nw)
  uall3_cum = rep(0, nw)
  ugrd_cum = rep(0, nw)
  
  # HH week and retailer information 
  hh_agg = hh_retailers_temp[, .(household_code=household_code[1],retailer_code=retailer_code[1],
                                 week_end=week_end[1],tprob=tprob[1],pprob=pprob[1]), by = c("idx")]
  setkey(hh_agg, idx)
  
  # Monte Carlo Integration
  # Vectorization is the key
  starttime <- proc.time()
  for (j in 1:30){
    if ("fil0_1"%in%names(hh_retailers_temp)){
      hh_retailers_temp[, `:=`(fil0_1 = NULL, fil0_2=NULL, fil0_3=NULL, fil1=NULL, eps=NULL)]
    }
    
    # Simulate eps
    hh_retailers_temp[, eps:=-log(-log(runif(nobs)))]
    # Simulate E
    E = exp(rnorm(1, mean=pref[as.character(i), 29], sd= pref[as.character(i), 30]))
    
    # Compute the utility in the scenario
    hh_retailers_temp[, `:=`(UE = 1/price * exp(zb + eps))]
    hh_retailers_temp[licensed==0&thirdp==0, `:=`(fil0_1 = as.integer(UE>=max(UE))), by = .(idx)]
    n1 =  nrow(hh_retailers_temp[licensed==0&thirdp==0,])
    if (n1==0){
      uall1_cum = 0 #no ground, get no value
    } else{
      dt_v0_1 = hh_retailers_temp[fil0_1>=0.9, .(ubar = log(sum(exp(zb + eps)*(E/price)))), by = .(idx)]
      uall1_cum[dt_v0_1$idx] = uall1_cum[dt_v0_1$idx] + dt_v0_1$ubar
    }
    hh_retailers_temp[licensed==0, `:=`(fil0_2 = as.integer(UE>=max(UE))), by = .(idx)]
    n2 =  nrow(hh_retailers_temp[licensed==0,])
    if (n2==0){
      uall2_cum = 0 #no ground, get no value
    } else{
      dt_v0_2 = hh_retailers_temp[fil0_2>=0.9, .(ubar = log(sum(exp(zb + eps)*(E/price)))), by = .(idx)]
      uall2_cum[dt_v0_2$idx] = uall2_cum[dt_v0_2$idx] + dt_v0_2$ubar
    }
    hh_retailers_temp[thirdp==0, `:=`(fil0_3 = as.integer(UE>=max(UE))), by = .(idx)]
    n3 =  nrow(hh_retailers_temp[thirdp==0,])
    if (n3==0){
      uall3_cum = 0 #no ground, get no value
    } else{
      dt_v0_3= hh_retailers_temp[fil0_3>=0.9, .(ubar = log(sum(exp(zb + eps)*(E/price)))), by = .(idx)]
      uall3_cum[dt_v0_3$idx] = uall3_cum[dt_v0_3$idx] + dt_v0_3$ubar
    }
    
    hh_retailers_temp[, `:=`(fil1 = as.integer(UE>=max(UE))), by = .(idx, keurig)]
    if (nrow(hh_retailers_temp[fil1>=0.9&keurig<=0.1,])==0){
      ugrd_cum = 0 #no ground, get no value
    } else{
      dt_v1 = hh_retailers_temp[fil1>=0.9&keurig<=0.1,
                                .(ubar = log(sum(exp(zb + eps)*(E/price)))), by = .(idx)]
      ugrd_cum[dt_v1$idx] = ugrd_cum[dt_v1$idx] + dt_v1$ubar
      ugrd_cum[-dt_v1$idx] = ugrd_cum[-dt_v1$idx] + 0 #no ground, get no value
    }
    #cat("Processed", j, "after", proc.time()-starttime, "seconds.\n\n")
  }
  hh_agg[, `:=`(uall1=uall1_cum/30, uall2=uall2_cum/30, uall3=uall3_cum/30, ugrd=ugrd_cum/30, idx=NULL)]
  hh_agg[, `:=`(mu_diff1=uall1-ugrd, mu_diff2=uall2-ugrd, mu_diff3=uall3-ugrd)]
  hh_agg = hh_agg[, .(mu_diff1 = sum(mu_diff1 * tprob)/sum(tprob),
                      mu_diff2 = sum(mu_diff2 * tprob)/sum(tprob),
                      mu_diff3 = sum(mu_diff3 * tprob)/sum(tprob),
                      uall1 = sum(uall1 * tprob)/sum(tprob),
                      uall2 = sum(uall2 * tprob)/sum(tprob),
                      uall3 = sum(uall3 * tprob)/sum(tprob),
                      ugrd = sum(ugrd * tprob)/sum(tprob)), 
                      by = c("household_code", "week_end", "pprob")]
  hh_agg = hh_agg[, .(mu_diff1 = mu_diff1 * pprob, uall1 = uall1*pprob, 
                      mu_diff2 = mu_diff2 * pprob, uall2 = uall2*pprob, 
                      mu_diff3 = mu_diff3 * pprob, uall3 = uall3*pprob, 
                      ugrd=ugrd*pprob), 
                      by = c("household_code", "week_end")]
  return(hh_agg)
}
invisible(clusterEvalQ(cl, load('Data/Machine-Adoption/MU-Diff-Asist.RData')))
clusterExport(cl, c('pref', 'xvars', 'hhValFun'))
cval_list = parLapply(cl, hh_codes, hhValFun)
cval_list = rbindlist(cval_list)
save(cval_list, file = paste(output_dir, "/HH-NoVariety-Diff-Type.RData", sep=""))

load(paste(output_dir, "/HH-NoVariety-Diff-Type.RData", sep=""))
load(paste(output_dir, "/HW-Full-Panel.RData", sep=""))
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
hw_market_panel = hw_market_panel[mu_diff1>=0, ]
setkey(hw_market_panel, household_code, week_end)
# Export Relevant Data sets to csv format, and use them for julia estimation
hw_market_panel[, `:=`(ntrip =.GRP), by = c("household_code", "week_end")]
setkey(hw_market_panel, ntrip)
write.csv(hw_market_panel[,.(household_code, hware, ntrip, t, price, price_avg, price_avgn,
                             mu_diff1, mu_diff2, mu_diff3, ashare, thanksgiving, christmas, 
                             bchristmas, achristmas, mother, father, nbrand)],
          file = paste(output_dir,"/NoVariety-MU-Panel.csv",sep=""), row.names = FALSE)

# Estimate the parameter governing the evolution process of mu_diff
# Both time and individual plays a very small role in determining the next period mu_diff.
for (i in 1:3){
  setnames(hw_market_panel, paste0("mu_diff", i), "mu_diff")
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
                                                sig = sigfun(mu_diff~mu_diff_lag)), 
                                    by = "household_code"]
  regout = hw_market_panel[, lm(mu_diff~mu_diff_lag)]
  print(summary(regout))
  setnames(hw_market_panel, "mu_diff", paste0("mu_diff", i))
}

