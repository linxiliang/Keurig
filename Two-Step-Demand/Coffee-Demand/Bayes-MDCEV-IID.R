#####################################################################################################
#
# Bayesian Estimation of Multiple Discrete-continuous Extreme Value (MDCEV)
# Assuming Independence of Parameters to Stablize Estimation
# Xiliang Lin
# December, 2016
#
#####################################################################################################
# Settings
rm(list = ls())               # Clear workspace

# Rscript Setting
options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)
print(args)
# trailingOnly=TRUE means that only your arguments are returned, check:
# print(commandsArgs(trailingOnly=FALSE))

market_code = as.integer(args[1])  # Give the market code for which the model is to be estimated,
                                   # all for everything, and remaining for small markets.
if (is.na(market_code)) market_code = args[1]
big_markets = c(501, 602, 803, 504, 539, 506, 524, 613, 623, 753) # Big Markets

# Load Required packages
require(data.table)
setNumericRounding(0)
require(numDeriv)
require(bayesm)
require(MASS)
require(parallel)
require(pracma)
require(extraDistr)
require(matrixStats)

# Directories
setwd("~/Keurig")
code_dir = "Scripts/Two-Step-Demand/Coffee-Demand/"
input_dir = "Data/MLogit-Data/"
output_dir = "Data/Bayes-MCMC/"
fig_dir = "Tabfigs/Bayes-MCMC/figs/"
tab_dir = "Tabfigs/Bayes-MCMC/tabs/"

#---------------------------------------------------------------------------------------------------------# 
# Initialize Parallel Execution Environment
cores = min(detectCores(logical=TRUE), as.integer(args[length(args)]))
cl = makeCluster(cores)
invisible(clusterEvalQ(cl, library(data.table)))
invisible(clusterEvalQ(cl, setNumericRounding(0)))
invisible(clusterEvalQ(cl, library(bayesm)))
invisible(clusterEvalQ(cl, library(MASS)))
invisible(clusterEvalQ(cl, library(numDeriv)))
invisible(clusterEvalQ(cl, library(pracma)))

#---------------------------------------------------------------------------------------------------------#
#Source the function file
source(paste(code_dir, 'mdc-functions.R', sep=""))

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
    hh_in_market = hh_demo[dma_code==market_code, unique(household_code)]
  }
  hh_market_prod = hh_market_prod[household_code%in%hh_in_market, ]
}

# Reassign hh and t
setkey(hh_market_prod, household_code, t, brand_descr, keurig, roast, brand_descr_orig)
hh_index_list = hh_market_prod[, unique(hh)]
hh_market_prod[, hh := .GRP, by = "hh"]
setkey(hh_market_prod, hh, t)
hh_market_prod[, t := .GRP, by = "t"]

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

# Reset Key
setkey(hh_market_prod, hh, t, brand, roast)

#---------------------------------------------------------------------------------------------------------#
# Parameter setting
# Number of Parameters to be estimated
nk = ncol(KMat)
nx = ncol(XMat)
np = nk+nx # if sigma is estimated, put + 1
nh = length(hh_market_prod[, unique(hh)])
nt = length(hh_market_prod[, unique(t)])

#---------------------------------------------------------------------------------------------------------#
# Bayesian Estimation 
# MCMC Settings
burnin = 0
thin   = 3
draws  = 8000
totdraws = draws*thin + burnin

## Auxiliary prior settings, and computations
# Set priors
bhat0 = rep(0, np)
k0 = 0.1
nu0 = max(c(4, 0.01*nh, np))
sig0 = 100;
beta0 = matrix(rnorm(nh*np, mean=0, sd=10), nrow=nh)
s2 = 2.93^2/np;

# Posterior degrees
kn = k0+nh
nun = nu0+nh
igammag = function(x) rinvchisq(1, nun, x)

# Distribute the functions and relevant data to the workers.
clusterExport(cl,c('i_ll', 'll', 'rwmhd','hh_market_prod', 'xnames', 's2',
                   'nt', 'nk', 'nx', 'np', 'nh', 'XMat', 'KMat'))


#Initialize storage of MCMC draws
bhatd = matrix(rep(0, draws*np), nrow=draws)
sigd = matrix(rep(0, draws*np), nrow=draws)
bindv = array(0, dim=c(draws, nh, np))

## Gibbs Sampling
start_time = proc.time()[3]
for (d in 1:totdraws){
  # For given betas - preference matrix, draw posterior mean and covariance matrix
  # Compute the posterior mean and variance of betas
  sign = (nu0 * sig0 + (nh-1)*colVars(beta0) + k0*nh*(colMeans(beta0)-bhat0)^2/kn)/nun
  mun = k0/kn * bhat0 + nh/kn * colMeans(beta0)
  
  # Draw from the posterior
  sig = sapply(sign, igammag)
  bhat = mvrnorm(1, mun, diag(sig/kn))
  print(bhat)
  
  # Store the posterior draws of mean betas and sigmas
  if ((d > burnin) & (d %% thin == 0)){
    indx = ceiling((d - burnin)/thin)
    bhatd[indx,] = bhat; 
    sigd[indx,] = sig; 
    bindv[indx,,] = beta0;
  }
  
  # Make sig a matrix for computation
  sig = diag(sig)
  
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

# Save output to a dataset
save(hh_index_list, bhatd, sigd, bindv, bnames, 
     file = paste(output_dir, "MDCEV-IID-MCMC-", market_code, ".RData", sep = ""))


