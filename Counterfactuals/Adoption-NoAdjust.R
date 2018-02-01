#####################################################################################################
#
# Adoption Rate 
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
Rcpp::sourceCpp('Scripts/Counterfactuals/CoffeeValue.cpp')
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
invisible(clusterEvalQ(cl, Rcpp::sourceCpp('Scripts/Counterfactuals/CoffeeValue.cpp')))

#---------------------------------------------------------------------------------------------------#
# Load Datasets
load("Data/Machine-Adoption/MU-Diff-Asist-NoAdjust.RData")

#---------------------------------------------------------------------------------------------------#
# Brand Descriptions 
brands = c("0OTHER", "CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GREEN MOUNTAIN KEURIG", "MAXWELL HOUSE", 
           "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")
xnames = c("0OTHER", brands, "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
           "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")

# Compute the adoption value consumer by consumer
xvars_all = c(paste0("a", 2:16), "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
          "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")
xvars_own = c(paste0("a", 2:16), "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
          "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")
xvars_licensed = c(paste0("a", 2:16), "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
          "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")

hhValFun<-function(i, n=300){
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
  xmat_all = as.matrix(hh_retailers_temp[, xvars_all, with=FALSE])
  xmat_own = as.matrix(hh_retailers_temp[, xvars_own, with=FALSE])
  xmat_licensed = as.matrix(hh_retailers_temp[, xvars_licensed, with=FALSE])
  kmat = as.matrix(hh_retailers_temp[, cbind(1-keurig, keurig)])
  
  # Compute the quality of product
  hh_retailers_temp[, zb_all := xmat_all %*% pref[as.character(i),1:27]]
  hh_retailers_temp[, zb_own := xmat_own %*% pref[as.character(i),1:27]]
  hh_retailers_temp[, zb_licensed := xmat_licensed %*% pref[as.character(i),1:27]]
  # Compute the alpha of the product
  hh_retailers_temp[, alpha := kmat %*% pref[as.character(i), 28:29]]
  # Compute the rho
  hh_retailers_temp[, rho := pref[as.character(i), 30]]
  
  # Number of retailer and week pair 
  setkey(hh_retailers_temp, household_code, retailer_code, week_end)
  hh_agg = hh_retailers_temp[, .(av = list(mc_vfun(zb_all,zb_own,zb_licensed,alpha, rho, price, keurig, 
                                                   owned, licensed, thirdp, n)), 
                                 tprob=tprob[1], pprob=pprob[1], pprob1=pprob1[1], pprob2=pprob2[1]), 
                             by = .(household_code, dma_code, retailer_code, week_end)]
  hh_agg[, `:=`(av1=sapply(av, "[[", 1), av2=sapply(av, "[[", 2), 
                av3=sapply(av, "[[", 3), gv=sapply(av, "[[", 4))]
  hh_agg = hh_agg[, .(av1=sum(av1*tprob)/sum(tprob), 
                      av2=sum(av2*tprob)/sum(tprob), 
                      av3=sum(av3*tprob)/sum(tprob), 
                      gv=sum(gv*tprob)/sum(tprob), pprob=pprob[1], 
                      pprob1=pprob1[1], pprob2=pprob2[1]), 
                  by = c("household_code", "week_end")]
  return(hh_agg)
}
invisible(clusterEvalQ(cl, load("Data/Machine-Adoption/MU-Diff-Asist-NoAdjust.RData")))
clusterExport(cl, c('pref', 'xvars_all', 'xvars_own', 'xvars_licensed', 'hhValFun'))
cval_list = parLapply(cl, hh_codes, hhValFun, n=1000)
cval_list = rbindlist(cval_list)
save(cval_list, file = paste(output_dir, "/HH-Util-Diff-Type-NoAdjust.RData", sep=""))

load(paste(output_dir, "/HH-Util-Diff-Type-NoAdjust.RData", sep=""))
load(paste(output_dir, "/HW-Full-Panel.RData", sep=""))
cval_list[, `:=`(mu_diff1 = (av1-gv)*pprob1, mu_diff2 = (av2-gv)*pprob1, mu_diff3 = (av3-gv)*pprob1)]
bds = cval_list[, quantile(mu_diff1, c(0.001, 0.999))]
cval_list = cval_list[mu_diff1<=bds[2] & mu_diff1>=bds[1],]
cval_list[, mu_diff1_lag := c(NA, mu_diff1[1:(length(mu_diff1)-1)]), by = "household_code"]
cval_list[, mu_diff2_lag := c(NA, mu_diff2[1:(length(mu_diff2)-1)]), by = "household_code"]
cval_list[, mu_diff3_lag := c(NA, mu_diff3[1:(length(mu_diff3)-1)]), by = "household_code"]

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
                             mu_diff1, mu_diff2, mu_diff3, thanksgiving, christmas, 
                             bchristmas, achristmas, mother, father, ashare, nbrand)],
          file = paste(output_dir,"/Type-MU-Panel-NoAdjust.csv",sep=""), row.names = FALSE)

# Estimate the parameter governing the evolution process of mu_diff
# Both time and individual plays a very small role in determining the next period mu_diff.
fereg1 = plm(log(mu_diff1+1)~log(mu_diff1_lag+1), data = hw_market_panel, index = c("household_code", "week_end"), model = "within")
fereg2 = plm(log(mu_diff2+1)~log(mu_diff2_lag+1), data = hw_market_panel, index = c("household_code", "week_end"), model = "within")
fereg3 = plm(log(mu_diff3+1)~log(mu_diff3_lag+1), data = hw_market_panel, index = c("household_code", "week_end"), model = "within")
summary(fereg1)
mean(fixef(fereg1))
summary(fereg2)
mean(fixef(fereg2))
summary(fereg3)
mean(fixef(fereg3))

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
  hw_markov_check[, print(c(median(gamma1, na.rm=T), median(gamma2, na.rm=T), median(sig, na.rm=T)))]
  regout = hw_market_panel[, lm(mu_diff~mu_diff_lag)]
  print(summary(regout))
  setnames(hw_market_panel, "mu_diff", paste0("mu_diff", i))
}
