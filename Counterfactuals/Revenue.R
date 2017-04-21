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

hhRevFun<-function(i){
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
  hh_retailers_temp[, idx:=.GRP, by = .(household_code, dma_code, retailer_code, week_end)]
  setkey(hh_retailers_temp, idx)
  hh_retailers_temp[, `:=`(rev1=0, rev2=0, rev3=0)]

  # Monte Carlo Integration
  # Vectorization is the key
  starttime <- proc.time()
  for (j in 1:1000){
    if ("fil0_1"%in%names(hh_retailers_temp)){
      hh_retailers_temp[, `:=`(fil0_1 = NULL, fil0_2=NULL, fil0_3=NULL, fil1=NULL, eps=NULL)]
    }
    
    # Simulate eps
    hh_retailers_temp[, eps:=-log(-log(runif(nobs)))]
    # Simulate E
    E = exp(rnorm(1, mean=pref[as.character(i), 29], sd= pref[as.character(i), 30]))
    
    # Compute the utility in the scenario
    hh_retailers_temp[, `:=`(UE = 1/price * exp(zb + eps)*(E/price+1)^(alpha-1),
                             U0 = 1/price * exp(zb + eps))]
    hh_retailers_temp[, spent1:=0]
    hh_retailers_temp[licensed==0&thirdp==0, `:=`(fil0_1 = as.integer(U0>=max(UE))), by = .(idx)]
    n1 =  nrow(hh_retailers_temp[licensed==0&thirdp==0,])
    if (n1>=1){
      hh_retailers_temp[fil0_1>=0.9, `:=`(spent1 = eu2rev(min(UE-0.00001), max(U0+0.00001), 
                                                          alpha, zb, price, eps, E)), by = .(idx)]
    }
    hh_retailers_temp[, spent2:=0]
    hh_retailers_temp[licensed==0, `:=`(fil0_2 = as.integer(UE>=max(UE))), by = .(idx)]
    n2 =  nrow(hh_retailers_temp[licensed==0,])
    if (n2>=1){
      hh_retailers_temp[fil0_2>=0.9, `:=`(spent2 = eu2rev(min(UE-0.00001), max(U0+0.00001), 
                                                          alpha, zb, price, eps, E)), by = .(idx)]
    }
    hh_retailers_temp[, spent3:=0]
    hh_retailers_temp[thirdp==0, `:=`(fil0_3 = as.integer(UE>=max(UE))), by = .(idx)]
    n3 =  nrow(hh_retailers_temp[thirdp==0,])
    if (n3>=1){
      hh_retailers_temp[fil0_3>=0.9, `:=`(spent3 = eu2rev(min(UE-0.00001), max(U0+0.00001), 
                                                          alpha, zb, price, eps, E)), by = .(idx)]
    }
    hh_retailers_temp[,`:=`(rev1 = rev1 + spent1, rev2 = rev2 + spent2, rev3 = rev3 + spent3)]
  }
  hh_retailers_temp[, `:=`(rev1=rev1/1000, rev2=rev2/1000, rev3=rev3/1000)]
  hh_retailers_temp[keurig==0, brand_descr:="GROUND"]
  hh_agg = hh_retailers_temp[, .(rev1 = sum(rev1), rev2 = sum(rev2), rev3 = sum(rev3)), 
                             by = c("household_code", "brand_descr", "dma_code", "retailer_code", 
                                    "week_end", "tprob", "pprob")]
  hh_agg = hh_retailers_temp[, .(rev1 = sum(rev1 * tprob)/sum(tprob),
                                 rev2 = sum(rev2 * tprob)/sum(tprob),
                                 rev3 = sum(rev3 * tprob)/sum(tprob)),
                      by = c("household_code", "brand_descr", "week_end", "pprob")]
  hh_agg = hh_agg[, .(rev1 = rev1 * pprob, rev2 = rev2 * pprob, rev3 = rev3 * pprob),
                      by = c("household_code", "brand_descr", "week_end")]
  return(hh_agg)
}
invisible(clusterEvalQ(cl, load('Data/Machine-Adoption/MU-Diff-Asist.RData')))
clusterExport(cl, c('pref', 'xvars', 'hhRevFun'))
hh_br_rev = parLapply(cl, hh_codes, hhRevFun)
hh_br_rev = rbindlist(hh_br_rev)
save(hh_br_rev, file = paste(output_dir, "/HH-Rev-Panel.RData", sep=""))
