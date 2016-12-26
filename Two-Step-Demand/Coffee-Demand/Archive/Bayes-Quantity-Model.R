#####################################################################################################
#
# Bayesian Estimation of Discrete Choice with Quantity Model
# Xiliang Lin
# April, 2016
#
#####################################################################################################
# Settings
rm(list = ls())               # Clear workspace

# Load Required packages
require(data.table)
setNumericRounding(0)
require(numDeriv)
require(bayesm)
require(MASS)
require(parallel)

# Directories
setwd("~/Keurig")
code_dir = "Scripts/Two-Step-Demand/Coffee-Demand/"
input_dir = "Data/MLogit-Data/"
output_dir = "Data/Bayes-MCMC/"
fig_dir = "Tabfigs/Bayes-MCMC/figs/"
tab_dir = "Tabfigs/Bayes-MCMC/tabs/"

#---------------------------------------------------------------------------------------------------------# 
# Initialize Parallel Execution Environment
cores = min(detectCores(logical=FALSE), 48)
cl = makeCluster(cores)
invisible(clusterEvalQ(cl,library(data.table)))
invisible(clusterEvalQ(cl, setNumericRounding(0)))
invisible(clusterEvalQ(cl,library(bayesm)))
invisible(clusterEvalQ(cl,library(MASS)))
invisible(clusterEvalQ(cl,library(numDeriv)))

#---------------------------------------------------------------------------------------------------------#
#Source the function file
source(paste(code_dir, 'functions.R', sep=""))

# ubar: Implicit ubar function for utility level
# ll_linear: log-likelihood function for logit model with 
# llfrac: log-likelihood function for simple logit model for given data set
# llg: Analytical gradient function (currently takes long to execute)
# llgn: Numerical gradient function
# Hess: Negative Hessian of the fractional likelihood evaluated at the optimal point
# pif: Posterior fractions of new versus old draws
# rwmhd: function for the Metropolis RW draws for given households

#---------------------------------------------------------------------------------------------------------# 
# Load estimation data
load(paste(input_dir,"NewYork.RData",sep=""))

# Get rid different sizes of the chosen brand and only leave the one that is cgiseb
hh_market_prod[, maxp := as.integer(purchased==max(purchased)), by = c("household_code", "t", "brand_descr")]
hh_market_prod = hh_market_prod[as.integer(maxp)==1, ]

# Convert size to log for estimation.
hh_market_prod[, size1_amount:=log(size1_amount)]

# Change total spent to the max price of available product or total_spent, whichever is greater
hh_market_prod[, price_max:=max(price), by = "trip_code_uc"]
hh_market_prod[, total_spent:= pmax(price, total_spent)]
hh_max_spent = hh_market_prod[, max(price), by = "hh"]$V1
hh_max_spent = hh_max_spent + hh_max_spent*0.2

# Create the Design Matrix
bnames = paste0("a", c(1:18))
BMat = as.matrix(hh_market_prod[, bnames, with=FALSE])
xnames = c(bnames, "keurig", "brand_lag")
XMat = as.matrix(hh_market_prod[, xnames, with=FALSE])

# Mark the start and end positions of households
hh_market_prod[, row_i:=1:.N]
hh_market_prod[, `:=`(start = min(row_i), end = max(row_i)), by = "household_code"]

# Number of Parameters to be estimated
np = length(xnames) + 1
nh = length(hh_market_prod[, unique(hh)])
nt = length(hh_market_prod[, unique(t)])

# Bayesian Estimation 

# MCMC Settings
burnin = 1000
thin   = 20
draws  = 2000
totdraws = draws*thin + burnin

## Auxiliary prior settings, and computations
nu0 = max(c(4, 0.01*nh, np))
V0 = nu0 * diag(np);
sig0 = rwishart(nu0, solve(V0))$IW;
sig0[np, np] = 1000
bhat0 = c(rep(0, np-2), 1, mean(hh_max_spent));
beta0 = cbind(matrix(rep(0, nh*(np-2)), nrow=nh), 1, hh_max_spent);
Z = rep(1, nh);
A = 0;
Dbar = t(rep(0,np));
s2 = 2.93^2/np;

# Distribute the functions and relevant data to the workers.
clusterExport(cl,c('ll_linear', 'hh_market_prod', 'XMat', 'np', 'nt', 
                   'nh', 'pif_linear', 'rwmhd_linear', 's2'))

#Initialize storage of MCMC draws
bhatd = matrix(rep(0, draws*np), nrow=draws)
sigd = array(0, dim=c(np, np, draws))

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
  
  # Store the posterior draws of mean betas and sigmas
  if ((d > burnin) & (d %% thin == 0)){
    indx = ceiling((d - burnin)/thin)
    bhatd[indx,] = bhat; 
    sigd[, ,indx] = sig; 
  }
  
  # RW MH to draw betas for each household
  # for (i in 1:nh) beta0[i,] = rwmhd_linear(i) # non-parallel version
  
  # Parallel version
  clusterExport(cl, c('sig', 'bhat', 'beta0'))
  beta_list = parLapply(cl, 1:nh, rwmhd_linear)
  beta0 = matrix(unlist(beta_list), ncol=np, byrow=TRUE)
  
  cat("Finished drawing", d, "out of", totdraws, ", and total time elapsed:",  proc.time()[3] - start_time, "\n\n")
}

stopCluster(cl)

# Save output to a dataset
save(bhatd, sigd, file = paste(output_dir, "Normal-MCMC.RData", sep = ""))
load(paste(output_dir, "Normal-MCMC.RData", sep = ""))


