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

# Set seed for each workers
s = .Random.seed
for (i in 1:length(cl)) {
  s = nextRNGStream(s)
  clusterExport(cl[i], c('s'))
  # send s to worker i as .Random.seed
}
invisible(clusterEvalQ(cl, .Random.seed <- s))
#---------------------------------------------------------------------------------------------------#
# Function
# Solve Lambda
BC<-function(lambda, alpha, zb, p, eps, E) return(sum(p*(exp((log(lambda)+log(p)-zb-eps)/(alpha-1))-1))-E)

# Obtain the mariginal utility at each product
EXPEND<-function(lambda, alpha, zb, p, eps, E) return(p*(exp((log(lambda)+log(p)-zb-eps)/(alpha-1))-1))

# Solve the utility maximization problem
eu <- function(umin, umax, alpha, zb, p, eps, E){
  if (length(p)==1){
    return((1/alpha * exp(zb + eps) * ((E/p + 1)^alpha-1)))
  } else{
    n0 = length(p)
    ind0 = seq(1, n0)
    dfx = 10
    while(dfx>0){
      kr = uniroot(BC, c(umin, umax), alpha = alpha[ind0],
                   zb = zb[ind0], p = p[ind0], eps = eps[ind0], E = E, tol=1e-16)
      Spent = EXPEND(kr$root, alpha, zb, p, eps, E)
      ind0 = which(Spent>0)
      n1 = length(ind0)
      if (n1==1){
        dfx=0
      } else{
        dfx=n0-n1
        n0=n1
      }
    }
    if (n1==1){
      return(1/alpha[ind0] * exp(zb[ind0] + eps[ind0]) * ((E/p[ind0] + 1)^alpha[ind0]-1))
    } else{
      return(sum(1/alpha[ind0] * exp(zb[ind0] + eps[ind0]) *
                   ((Spent[ind0]/p[ind0] + 1)^alpha[ind0]-1)))
    }
  }
}

# For given availability and preference parameters
vfun = function(E, alpha, zb, p, eps, u){
  UE = 1/p * exp(zb + eps)*(E/p+1)^(alpha-1)
  U0 = 1/p * exp(zb + eps)
  sind = which(U0>max(UE))
  umin = min(UE[sind])-0.00000001
  umax = max(U0[sind])+0.00000001
  return(eu(umin, umax, alpha[sind], zb[sind], p[sind], eps[sind], E) - u)
}

CVfun<-function(alpha, zb, p, eps, u){
  umax = u
  udiff = vfun(umax, alpha = alpha, zb=zb, p = p, eps = eps, u = u)
  while(udiff < 0){
    umax = 10*umax
    udiff = vfun(umax, alpha = alpha, zb=zb, p = p, eps = eps, u = u)
  }
  CV = uniroot(vfun, c(0.001, umax), alpha = alpha, 
               zb=zb, p = p, eps = eps, u = u, tol=1e-16)
  return(CV$root)
}
clusterExport(cl, c('BC', 'EXPEND', 'eu', 'vfun', 'CVfun'))

#------------------------------------------------------------------------------#
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
indx = seq(1001, 2500, 1)
load('Data/Bayes-MCMC/MDCEV-MCMC-All-30000.RData')
bindv[, ,27:28] = exp(bindv[, ,27:28])/(1+exp(bindv[, ,27:28]))
pref = colMeans(bindv[indx, ,], 2)
rownames(pref) = hh_code_list[, household_code]
hh_market_prod = hh_market_prod[household_code%in%hh_code_list[, household_code], ]

# Generate the corresponding zb and alpha
xvars = c(paste0("a", 2:15), "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
          "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")
hh_list = unique(hh_market_prod[, household_code])
for (h in hh_list){
  xmat = as.matrix(hh_market_prod[.(h), xvars, with=FALSE])
  kmat = as.matrix(hh_market_prod[.(h), cbind(1-keurig, keurig)])
  # Compute the quality of product
  hh_market_prod[.(h), zb := xmat %*% pref[as.character(h), 1:26]]
  # Compute the alpha of the product
  hh_market_prod[.(h), alpha := kmat %*% pref[as.character(h), 27:28]]
}

# Obtain the list of households
hh_full = copy(hh_market_prod)
hh_n_list = hh_full[, unique(household_code)]
ncl = length(cl)
nhsize = ceiling(length(hh_n_list)/ncl)

# Put each chunk of data to the data.table
for (i in 1:ncl){
  i_start = (i-1)*nhsize+1
  i_end = i*nhsize
  if (i==ncl){
    i_end = length(hh_n_list)
  }
  hh_list = hh_n_list[i_start:i_end]
  hh_market_prod = hh_full[household_code%in%hh_list, ]
  clusterExport(cl[i], c('hh_list', 'hh_market_prod'))
}

clusterEvalQ(cl, (nr=nrow(hh_market_prod)))

hhcv = function(){
  hh_market_prod[, `:=`(eps=-log(-log(runif(nr))))]
  hh_market_prod[, `:=`(EC=sum(total_price_paid)), by = c("household_code", "t")]
  hh_market_prod[,  uall := vfun(EC[1], alpha, zb, price, eps, 0), by = c("household_code", "t")]
  temp1 = hh_market_prod[licensed==0&thirdp==0, .(cv_all = CVfun(alpha, zb, price, eps, uall[1])), 
                         by = c("household_code", "t")]
  temp1[, ktype := "GMCR Only"]
  temp2 = hh_market_prod[licensed==0, .(cv_all = CVfun(alpha, zb, price, eps, uall[1])), 
                         by = c("household_code", "t")]
  temp2[, ktype := "GMCR+Third Party"]
  temp3 = hh_market_prod[thirdp==0, .(cv_all = CVfun(alpha, zb, price, eps, uall[1])), 
                         by = c("household_code", "t")]
  temp3[, ktype := "GMCR+Licensed"]
  return(rbindlist(list(temp1, temp2, temp3)))
}
clusterExport(cl, c('hhcv'))

mc_draws = 1000
hh_agg = hh_full[keurig==0, .(cval = 0), by = c("household_code", "t")]
nht = nrow(hh_agg)
hh_agg = rbindlist(list(hh_agg, hh_agg, hh_agg))
for (i in 1:mc_draws){
  # Generate the type 1 EV draw
  hh_temp = clusterEvalQ(cl, hhcv())
  hh_temp = rbindlist(hh_temp)
  if (i==1) hh_agg[, `:=`(household_code=hh_temp$"household_code", t=hh_temp$"t", Type=hh_temp$ktype)]
  hh_agg[, cval := cval + hh_temp$cv_all]
  cat("Draw", i, "Finished out of", mc_draws, ".\n\n")
}
hh_agg[, cval := cval/mc_draws]
hh_agg = hh_agg[cval<=2000, ]
# Stop Cluster
stopCluster(cl)
save(hh_agg, file = "~/Keurig/Data/Counterfactual/Welfare.RData")

#------------------------------------------------------------------------------#
# Plot the welfare analysis
load("~/Keurig/Data/Counterfactual/Welfare.RData")
hh_agg = hh_agg[, .(cval = sum(cval)), by = c("household_code", "Type")]
hh_spent = hh_full[, .(total_price_paid = sum(total_price_paid)), by = "household_code"]
setkey(hh_agg, household_code)
setkey(hh_spent, household_code)
hh_agg=hh_spent[hh_agg, nomatch=0L]
hh_agg[, cv_percent := 100*cval/(total_price_paid)]

# Plots
setkey(hh_agg, Type)
welgraph = ggplot(hh_agg[cv_percent<=5000, ], aes(x=cv_percent))+
  geom_histogram(bins=70, aes(y = ..density..), fill = "skyblue")+
  theme_bw()+labs(list(x="CV Percent"))+facet_grid(.~Type)
ggsave(paste(graph_dir, "/figs/welfare_hist.pdf", sep=""), welgraph, width=10, height=4)
