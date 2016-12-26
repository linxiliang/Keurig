#####################################################################################################
#
# Bayesian Estimation of Multiple Discrete-continuous Extreme Value (MDCEV)
# Xiliang Lin
# October, 2016
#
#####################################################################################################
# Settings
rm(list = ls())               # Clear workspace

# Markets
market_code = c(501, 602, 803, 504, 539, 506, 524, 613, 623, 753) # Big Markets

# Load Required packages
require(Rmpi)
require(snow)
require(data.table)
setNumericRounding(0)
require(numDeriv)
require(bayesm)
require(MASS)
require(pracma)

# Directories
setwd("~/Keurig")
code_dir = "Scripts/Two-Step-Demand/Coffee-Demand/"
input_dir = "Data/MLogit-Data/"
output_dir = "Data/Bayes-MCMC/"
fig_dir = "Tabfigs/Bayes-MCMC/figs/"
tab_dir = "Tabfigs/Bayes-MCMC/tabs/"

#---------------------------------------------------------------------------------------------------------# 
# Initialize Parallel Execution Environment
np <- ceil(4*(mpi.universe.size() - 1)/5)
cl <- makeMPIcluster(np)
# Print the hostname for each cluster member
sayhello <- function() {
  info <- Sys.info()[c("nodename", "machine")]
  paste("Hello from", info[1], "with CPU type", info[2])
}

names <- clusterCall(cl, sayhello)
print(unlist(names))
invisible(clusterEvalQ(cl, library(data.table)))
invisible(clusterEvalQ(cl, setNumericRounding(0)))
invisible(clusterEvalQ(cl, library(bayesm)))
invisible(clusterEvalQ(cl, library(MASS)))
invisible(clusterEvalQ(cl, library(numDeriv)))
invisible(clusterEvalQ(cl, library(pracma)))
#---------------------------------------------------------------------------------------------------------#
#Source the function file
source(paste(code_dir, 'mdc-functions.R', sep=""))
invisible(clusterEvalQ(cl, source('~/Keurig/Scripts/Two-Step-Demand/Coffee-Demand/mdc-functions.R')))

# ubar: Implicit ubar function for utility level
# ll_linear: log-likelihood function for logit model with 
# llfrac: log-likelihood function for simple logit model for given data set
# llg: Analytical gradient function (currently takes long to execute)
# llgn: Numerical gradient function
# Hess: Negative Hessian of the fractional likelihood evaluated at the optimal point
# pif: Posterior fractions of new versus old draws
# rwmhd: function for the Metropolis RW draws for given households

#---------------------------------------------------------------------------------------------------------#
# Data Preparation
# Load estimation data
load(paste(input_dir,"MDC-Cond-Purchase-Flavors.RData",sep=""))

if (market_code != "all"){
  if (market_code == "remaining"){
    hh_in_market = hh_demo[!(dma_code %in% big_markets), unique(household_code)]
  } else{
    hh_in_market = hh_demo[dma_code%in%market_code, unique(household_code)]
  }
  hh_market_prod = hh_market_prod[household_code%in%hh_in_market, ]
}

# Reassign hh and t
setkey(hh_market_prod, household_code, t, brand_descr, keurig, roast, brand_descr_orig)
hh_index_list = hh_market_prod[, unique(hh)]
hh_market_prod[, hh := .GRP, by = "hh"]
setkey(hh_market_prod, hh, t)
hh_market_prod[, t := .GRP, by = "t"]

# Reset Key
setkey(hh_market_prod, hh, t, brand, roast)

# Create the Design Matrix
bnames = paste0("a", c(2:15))
for (i in 2:15){
  npresence = sum(hh_market_prod[, paste0("a", i), with=FALSE])
  if (npresence<=500) bnames = setdiff(bnames, paste0("a", i))
}
BMat = as.matrix(hh_market_prod[, bnames, with=FALSE])
xnames = c(bnames, "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
           "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")
XMat = as.matrix(hh_market_prod[, xnames, with=FALSE])
#Create a K matrix to allow different satiation rate for Keurig and Ground 
KMat = hh_market_prod[, cbind(1-keurig, keurig)]

# Mark the start and end positions of households
hh_market_prod[, row_i:=1:.N]
hh_market_prod[, `:=`(start = min(row_i), end = max(row_i)), by = "household_code"]
hh_market_prod[, `:=`(nt_i = length(unique(t))), by  = "household_code"]

# Obtain the M --- Number of items chosen in the trip
hh_market_prod[, `:=`(M = sum(purchased)), by = c("household_code", "t")]

# Create Expenditure based on size and aggregate price -- not actual price (Robustness check needed)
hh_market_prod[, expenditure := price * size]
# Or
# hh_market_prod[, expenditure := total_price_paid - coupon_value] # correlation is high.
# Create log price and log expenditure part
hh_market_prod[, `:=`(lprice=log(price), lexpend=log(expenditure/price+1))]
#---------------------------------------------------------------------------------------------------------#
# Parameter setting
# Number of Parameters to be estimated
nk = ncol(KMat)
nx = ncol(XMat)
np = nk+nx # if sigma is estimated, put + 1
nh = length(hh_market_prod[, unique(hh)])
nt = length(hh_market_prod[, unique(t)])

# save auxiliary dataset 
save(xnames, hh_market_prod, nt, nk, nx, np, nh, file = "Asist.RData")
#---------------------------------------------------------------------------------------------------------#
# Model Tuning
# Estimate an homogeneous logit model to use it to tune RW draws
# b0 = rnorm(np)
# opt0 = optim(b0, ll_homo, gr = NULL, method = c("BFGS"), control = list(reltol=1e-12))
# hess = hessian(ll_homo, b0, h = 1e-4)

# Evaluate Hessian at Fractional likelihood
#hess_list = parLapply(cl, 1:nh, ihessfun)
#---------------------------------------------------------------------------------------------------------#
invisible(clusterEvalQ(cl, load('Asist.RData')))
invisible(clusterEvalQ(cl, load('aux.RData')))

# Bayesian Estimation 
# MCMC Settings
burnin = 0
thin   = 3
draws  = 8000
totdraws = draws*thin + burnin

## Auxiliary prior settings, and computations
nu0 = max(c(4, 0.01*nh, np))
V0 = nu0 * diag(np);
sig0 = rwishart(nu0, solve(V0))$IW;
bhat0 = rep(0, np)
beta0 = matrix(rnorm(nh*np, mean=0, sd=3), nrow=nh)
Z = rep(1, nh);
A = 0;
Dbar = t(rep(0,np));
s2 = 2.93^2/np;

# Distribute the functions and relevant data to the workers.
clusterExport(cl, c('s2'))

#Initialize storage of MCMC draws
bhatd = matrix(rep(0, draws*np), nrow=draws)
sigd = array(0, dim=c(np, np, draws))
bindv = array(0, dim=c(draws, nh, np))

## Gibbs Sampling
start_time = proc.time()[3]
for (d in 1:totdraws){
  # For given betas - preference matrix, draw posterior mean and covariance matrix
  # Compute the posterior mean and variance of betas
  
  Dhat = solve(t(Z) %*% Z) %*% t(Z) %*% beta0
  Dtild = solve(t(Z) %*% Z + A) %*% (t(Z) %*% Z %*% Dhat + A %*% Dbar)     
  S    = t(beta0 - Z %*% Dtild) %*% (beta0 - Z %*% Dtild) + t(Dhat - Dbar) %*% A %*% (Dhat - Dbar)
  
  # Draw from the posterior
  sig  = rwishart(nu0+nh, solve(V0+S))$IW
  bhat = mvrnorm(1, as.vector(Dhat), kronecker(sig, solve(t(Z)%*%Z)))
  print(bhat)
  
  # Store the posterior draws of mean betas and sigmas
  if ((d > burnin) & (d %% thin == 0)){
    indx = ceiling((d - burnin)/thin)
    bhatd[indx,] = bhat; 
    sigd[, ,indx] = sig; 
    bindv[indx,,] = beta0;
  }
  
  # RW MH to draw betas for each household
  # Non-parallel version 
  # for (i in 1:nh) {b0 = rwmhd_simple(i); beta0[i, ]=b0} 
  
  # Parallel version
  clusterExport(cl, c('sig', 'bhat', 'beta0'))
  beta_list = parLapply(cl, 1:nh, rwmhd)
  beta0 = matrix(unlist(beta_list), ncol=np, byrow=TRUE)
  
  cat("Finished drawing", d, "out of", totdraws, ", and total time elapsed:", 
      proc.time()[3] - start_time, "\n\n")
}

stopCluster(cl)
mpi.exit()

# Save output to a dataset
save(hh_index_list, bhatd, sigd, bindv, bnames, 
     file = paste(output_dir, "MDCEV-MCMC-All", ".RData", sep = ""))


