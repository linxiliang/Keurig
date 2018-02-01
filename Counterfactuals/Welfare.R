################################################################################
#
# Welfare Analysis
# Xiliang Lin
# Jan 2017
#
################################################################################
# Settings
rm(list = ls())
gc()

# Packages
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(parallel)
setNumericRounding(0)

# Set Working Folder Path Here
setwd("~/Keurig")
meta_dir = "Data/Meta-Data/"
input_dir = "Data/Bayes-MCMC/"
data_input_dir = "Data/MLogit-Data/"
graph_dir = "Tabfigs/Counterfactuals/"
code_dir = "Scripts/Counterfactuals/"
# Set seed
RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
# Source function
source('Scripts/Counterfactuals/machine-functions.R')
Rcpp::sourceCpp('Scripts/Counterfactuals/CoffeeValue.cpp')
#-----------------------------------------------------------------------------#
# Initialize Parallel Execution Environment
cores = detectCores(logical=TRUE)
cl = makeCluster(cores)

#-----------------------------------------------------------------------------#
# Put relevant packages and functions on cluster
invisible(clusterEvalQ(cl, library(data.table)))
invisible(clusterEvalQ(cl, setNumericRounding(0)))
invisible(clusterEvalQ(cl, setwd('~/Keurig')))
invisible(clusterEvalQ(cl, RNGkind("L'Ecuyer-CMRG")))
invisible(clusterEvalQ(cl, source('Scripts/Counterfactuals/machine-functions.R')))
invisible(clusterEvalQ(cl, Rcpp::sourceCpp('Scripts/Counterfactuals/CoffeeValue.cpp')))

# Set seed for each workers
s = .Random.seed
for (i in 1:length(cl)) {
  s = nextRNGStream(s)
  clusterExport(cl[i], c('s'))
  # send s to worker i as .Random.seed
}
invisible(clusterEvalQ(cl, .Random.seed <- s))
#---------------------------------------------------------------------------------------------------#
# Load data
load(paste(data_input_dir, "MDC-Cond-Purchase-Flavors.RData", sep=""))
hh_market_prod[keurig==1&(!grepl("KEURIG", brand_descr_orig)), brd := paste(brand_descr_orig, "KEURIG")]
hh_market_prod[keurig==0|grepl("KEURIG", brand_descr_orig), brd := brand_descr_orig]
hh_market_prod[brand_descr_orig=="KEURIG", brd:="KEURIG KEURIG"]
owned_brands = c("KEURIG KEURIG", "GREEN MOUNTAIN KEURIG")
licensed_brands = c("CARIBOU KEURIG", "NEWMAN'S OWN ORGANICS KEURIG", "EIGHT O'CLOCK KEURIG")
thirdp_brands = setdiff(unique(hh_market_prod[keurig==1, brd]), 
                        c(owned_brands, licensed_brands))
hh_market_prod[, `:=`(owned=as.integer(brd%in%owned_brands),
                      licensed=as.integer(brd%in%licensed_brands),
                      thirdp=as.integer(brd%in%thirdp_brands))]
setkeyv(hh_market_prod, c("household_code", "t", "brand", "roast"))

# Do the welfare analysis only for households on the platform
hh_market_prod = hh_market_prod[kholding==1, ]

# Drop cases where only Keurig is available -- not sure how to deal with it.
# Drop cases if spending is below $1 -- likely to be very special cases
hh_market_prod[, `:=`(any_ground = sum(keurig==0), total_trip_paid = sum(total_price_paid)), 
               by = c("hh", "t")]
hh_market_prod = hh_market_prod[any_ground>=5&total_trip_paid>=1, ]
hh_market_prod[, `:=`(any_ground=NULL, total_trip_paid=NULL)]

# Take the last part of markov chain to estimate consumer preferences
indx = seq(501, 1000, 1)
load('Data/Bayes-MCMC/MDCEV-MCMC-OX.RData')
bindv[, ,29:31] = exp(bindv[, ,29:31])/(1+exp(bindv[, ,29:31]))
pref = colMeans(bindv[indx, ,], 2)
rownames(pref) = hh_code_list[, household_code]
pref = pref[order(row.names(pref)), ]
rm(bindv)
gc()
hh_market_prod = hh_market_prod[household_code%in%hh_code_list[, household_code], ]

# Update the number of products
hh_market_prod[, nbrand:=log(nbrand)]
setnames(hh_market_prod, "nbrand", "nbrand_all")
hh_market_prod[thirdp==1, `:=`(nbrand_own = length(unique(brand_descr_orig)) + 0.0), 
               by = c("hh", "t")]
hh_market_prod[licensed==1, `:=`(nbrand_licensed = length(unique(brand_descr_orig)) + 0.0), 
               by = c("hh", "t")]
hh_market_prod[, `:=`(nbrand_own = mean(nbrand_own, na.rm=T), nbrand_licensed = mean(nbrand_licensed, na.rm=T)),
               by = c("hh", "t")]
hh_market_prod[is.na(nbrand_own) | is.infinite(nbrand_own), nbrand_own:=0]
hh_market_prod[is.na(nbrand_licensed) | is.infinite(nbrand_licensed), nbrand_licensed:=0]
hh_market_prod[keurig==1 | grepl(" KEURIG", brand_descr), 
               `:=`(nbrand_own = log(round(exp(nbrand_all))-nbrand_own-nbrand_licensed),
                    nbrand_licensed = log(round(exp(nbrand_all))-nbrand_own))]
hh_market_prod[(keurig==0 & !grepl(" KEURIG", brand_descr)), `:=`(nbrand_own = nbrand_all, nbrand_licensed = nbrand_all)]
hh_market_prod[is.na(nbrand_own) | is.infinite(nbrand_own), nbrand_own:=0]
hh_market_prod[is.na(nbrand_licensed) | is.infinite(nbrand_licensed), nbrand_licensed:=0]

# Generate the corresponding zb and alpha
# Compute the adoption value consumer by consumer
xvars_all = c(paste0("a", 2:16), "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
              "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag", "nbrand_all")
xvars_own = c(paste0("a", 2:16), "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
              "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag", "nbrand_own")
xvars_licensed = c(paste0("a", 2:16), "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
                   "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag", "nbrand_licensed")
setkeyv(hh_market_prod, c("household_code", "hh", "t"))
hh_list = unique(hh_market_prod[, household_code])
for (h in hh_list){
  xmat_all = as.matrix(hh_market_prod[.(h), xvars_all, with=FALSE])
  xmat_own = as.matrix(hh_market_prod[.(h), xvars_own, with=FALSE])
  xmat_licensed = as.matrix(hh_market_prod[.(h), xvars_licensed, with=FALSE])
  kmat = as.matrix(hh_market_prod[.(h), cbind(1-keurig, keurig)])
  
  # Compute the quality of product
  hh_market_prod[.(h), zb_all := xmat_all %*% pref[as.character(h),1:28]]
  hh_market_prod[.(h), zb_own := xmat_own %*% pref[as.character(h),1:28]]
  hh_market_prod[.(h), zb_licensed := xmat_licensed %*% pref[as.character(h),1:28]]
  # Compute the alpha of the product
  hh_market_prod[.(h), alpha := kmat %*% pref[as.character(h), 29:30]]
  # Compute the rho
  hh_market_prod[.(h), rho := pref[as.character(h), 31]]
}

# Obtain the list of households
hh_full = copy(hh_market_prod)
hh_n_list = hh_full[, unique(household_code)]
ncl = length(cl)
nhsize = ceiling(length(hh_n_list)/ncl)

# Put each chunk of data to the data.table
setkey(hh_full, household_code, t)
for (i in 1:ncl){
  i_start = (i-1)*nhsize+1
  i_end = i*nhsize
  if (i==ncl){
    i_end = length(hh_n_list)
  }
  hh_list = hh_n_list[i_start:i_end]
  hh_market_prod = hh_full[household_code%in%hh_list, ]
  setkey(hh_market_prod, household_code, t)
  clusterExport(cl[i], c('hh_list', 'hh_market_prod'))
}

clusterEvalQ(cl, (nr=nrow(hh_market_prod)))

hhcv = function(n){
  hh_agg = hh_market_prod[, .(cv = list(mc_cvfun(zb_all,zb_own,zb_licensed,alpha, rho, price, keurig, 
                                                owned, licensed, thirdp, n))), 
                          by = .(household_code, t)]
  hh_agg[, `:=`(cv1=sapply(cv, "[[", 1), cv2=sapply(cv, "[[", 2), 
                cv3=sapply(cv, "[[", 3), gv=sapply(cv, "[[", 4))]
  return(hh_agg)
}
clusterExport(cl, c('hhcv'))
hh_agg = clusterEvalQ(cl, hhcv(1000))
hh_agg = rbindlist(hh_agg)
save(hh_agg, file = "~/Keurig/Data/Counterfactual/Welfare.RData")

#------------------------------------------------------------------------------#
# Needs revision!
# Plot the welfare analysis
load("~/Keurig/Data/Counterfactual/Welfare.RData")
hh_agg = hh_agg[, .(cv_owned = sum(cv1-cv2), cv_licensed=sum(cv1-cv3), cv_val = sum(cv1-gv)), by = c("household_code")]
hh_spent = hh_full[kholding==1, .(total_price_paid = sum(total_price_paid)), by = "household_code"]
setkey(hh_agg, household_code)
setkey(hh_spent, household_code)
hh_agg=hh_spent[hh_agg, nomatch=0L]
hh_agg[, `:=`(cv_per_owned = 100*cv_owned/(total_price_paid),
              cv_per_licensed = 100*cv_licensed/(total_price_paid),
              cv_per_val = 100*cv_val/(total_price_paid))]

# Plots
hh_agg_1 = hh_agg[,.(household_code, cv_per_val)]
setnames(hh_agg_1, "cv_per_val", "cv_percent")
hh_agg_1[, Type:="Ground Only"]
hh_agg_2 = hh_agg[,.(household_code, cv_per_owned)]
setnames(hh_agg_2, "cv_per_owned", "cv_percent")
hh_agg_2[, Type:="GMCR Owned"]
hh_agg_3 = hh_agg[,.(household_code, cv_per_licensed)]
setnames(hh_agg_3, "cv_per_licensed", "cv_percent")
hh_agg_3[, Type:="GMCR Owned + Licensed"]
hh_agg = rbindlist(list(hh_agg_1, hh_agg_2, hh_agg_3))
hh_agg[, Type := factor(Type, levels = c("Ground Only", "GMCR Owned", "GMCR Owned + Licensed"))]
setkey(hh_agg, Type)
welgraph = ggplot(hh_agg[cv_percent<=302.8918 & cv_percent>=-147.0321, ], aes(x=cv_percent))+
  geom_histogram(bins=20, aes(y = ..density..), fill = "skyblue", col = "grey75")+
  theme_minimal()+labs(list(x="CV Percent"))+facet_grid(.~Type)
ggsave(paste(graph_dir, "figs/welfare_hist.pdf", sep=""), welgraph, width=10, height=4)
hh_agg[, quantile(cv_percent, c(0.01, 0.025, 0.05, 0.25,0.50, 0.75, 0.95, 0.975, 0.99)), by = "Type"]
hh_agg[, mean(cv_percent<0), by = "Type"]

