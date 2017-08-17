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

hhRevFun<-function(i, n=300){
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
  hh_retailers_temp[, c("rev1", "rev2", "rev3", "rev4") := mc_rfun(zb_all,zb_own,zb_licensed,alpha, rho[1], price,
                                                                   keurig, owned, licensed, thirdp, n),
                    by = .(household_code, dma_code, retailer_code, week_end)]
  hh_retailers_temp[keurig==0, brand_descr:="GROUND"]
  hh_agg = hh_retailers_temp[, .(rev1 = sum(rev1), rev2 = sum(rev2), rev3 = sum(rev3)), 
                             by = c("household_code", "brand_descr", "dma_code", "retailer_code", 
                                    "week_end", "tprob", "pprob", "pprob1", "pprob2")]
  hh_agg = hh_retailers_temp[, .(rev1 = sum(rev1 * tprob)/sum(tprob),
                                 rev2 = sum(rev2 * tprob)/sum(tprob),
                                 rev3 = sum(rev3 * tprob)/sum(tprob)),
                      by = c("household_code", "brand_descr", "week_end", "pprob", "pprob1", "pprob2")]
  return(hh_agg)
}
invisible(clusterEvalQ(cl, load("Data/Machine-Adoption/MU-Diff-Asist-NoAdjust.RData")))
clusterExport(cl, c('pref', 'xvars_all', 'xvars_own', 'xvars_licensed', 'hhRevFun'))
hh_br_rev = parLapply(cl, hh_codes, hhRevFun, 100)
hh_br_rev = rbindlist(hh_br_rev)
save(hh_br_rev, file = paste(output_dir, "/HH-Rev-Panel-NoAdjust.RData", sep=""))

hh_agg = hh_agg[, .(rev1 = rev1 * pprob, rev2 = rev2 * pprob, rev3 = rev3 * pprob),
                by = c("household_code", "brand_descr", "week_end")]

