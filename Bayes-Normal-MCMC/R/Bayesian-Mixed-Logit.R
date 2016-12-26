##########################################################################################################
# This script implements the bayesian estimation of mixed logit model
# Xiliang Lin
# Dec, 2015
##########################################################################################################

# Settings
rm(list = ls())               # Clear workspace
run_on_Linux        = TRUE    # Determines the path settings
sample_run          = FALSE    # Determine whether to run the codes on full data or a subsample

#--------------------------------------------------------------------------------------------------------#
# Load Required packages
require(data.table)
setNumericRounding(0)
require(numDeriv)
require(bayesm)
require(MASS)
require(parallel)

#---------------------------------------------------------------------------------------------------------# 
# Initialize Parallel Execution Environment
cores = detectCores(logical=FALSE)
cl = makeCluster(cores)
invisible(clusterEvalQ(cl,library(data.table)))
invisible(clusterEvalQ(cl, setNumericRounding(0)))
invisible(clusterEvalQ(cl,library(bayesm)))
invisible(clusterEvalQ(cl,library(MASS)))
invisible(clusterEvalQ(cl,library(numDeriv)))

#---------------------------------------------------------------------------------------------------------#
# Set working directory and corresponding dir 
if (run_on_Linux){
  setwd("~/Keurig")
} else{
  setwd("D:/Cygwin64/home/xlin0/Keurig")
}

code_dir = "Scripts/Bayes-Normal-MCMC/R/"
input_dir = "Data/MLogit-Data/"
output_dir = "Data/Bayes-MCMC/"
fig_dir = "Tabfigs/Bayes-MCMC/figs/"
tab_dir = "Tabfigs/Bayes-MCMC/tabs/"

#---------------------------------------------------------------------------------------------------------#
# Load estimation data
cbrands = c(1:20)
if (sample_run){
  hh_panel = fread(paste(input_dir,"keurig_julia_samp.csv",sep=""))
} else{
  load(paste(input_dir,"keurig_panel.RData",sep=""))
}
# Specify the X variables
xnames = c(paste0("a", cbrands), "price", "size1_amount", "brand_lag", "brand_cum")
hh_panel[, brand_cum:=log(brand_cum+1)]
np = length(xnames) - 1
nh = length(hh_panel[, unique(hh)])
nt = length(hh_panel[, unique(t)])

#---------------------------------------------------------------------------------------------------------#
#Source the function file
source(paste(code_dir, 'functions.R', sep=""))

# ll: log-likelihood function for simple logit model
# llfrac: log-likelihood function for simple logit model for given data set
# llg: Analytical gradient function (currently takes long to execute)
# llgn: Numerical gradient function
# Hess: Negative Hessian of the fractional likelihood evaluated at the optimal point
# pif: Posterior fractions of new versus old draws
# rwmhd: function for the Metropolis RW draws for given households

#---------------------------------------------------------------------------------------------------------#

# Simple Logit estimation
b0 = rep(0, np) # Starting point
opt2 = optim(b0, ll, llgn, method = c("BFGS")) # BFGS works better than L-BFGS-B
h = solve(hessian(ll, opt2$par)) # Get the hessian
# Use the optimal betas as the starting point for the fractional likelihood.

#---------------------------------------------------------------------------------------------------------#

# Bayesian Estimation 

# MCMC Settings
burnin = 1000
thin   = 20
draws  = 5000
totdraws = draws*thin + burnin

## Auxiliary prior settings, and computations
nu0 = max(c(4, 0.01*nh, np))
V0 = nu0 * diag(np);
sig0 = rwishart(nu0, solve(V0))$IW;
bhat0 = rep(0, np);
beta0 = matrix(rep(0, nh*np), nrow=nh);
Z = rep(1, nh);
A = 0;
Dbar = t(rep(0,np));
s2 = 2.93^2/np;

# Distribute the functions and relevant data to the workers.
clusterExport(cl,c('ll', 'llfrac', 'llg', 'llgn', 'hh_panel', 'np', 'nt', 
                   'nh', 'opt2', 'Hess', 'pif', 'rwmhd', 's2'))

# Obtain the hessian for each consumer's fractional likelihood
if (file.exists(paste(output_dir, "Fractional-Hess.RData", sep=""))){
  load(paste(output_dir, "Fractional-Hess.RData", sep=""))
} else{
  Hess_list = parLapply(cl, 1:nh, Hess)
  save(Hess_list, file = paste(output_dir, "Fractional-Hess.RData", sep=""))
}
clusterExport(cl, c('Hess_list'))

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
  # for (i in 1:nh) beta0[i,] = rwmhd(i) # non-parallel version

  # Parallel version
  clusterExport(cl, c('sig', 'bhat', 'beta0'))
  beta_list = parLapply(cl, 1:nh, rwmhd)
  beta0 = matrix(unlist(beta_list), ncol=np, byrow=TRUE)
  
  cat("Finished drawing", d, "out of", totdraws, ", and total time elapsed:",  proc.time()[3] - start_time, "\n\n")
}

stopCluster(cl)

# Save output to a dataset
save(bhatd, sigd, file = paste(output_dir, "Normal-MCMC.RData", sep = ""))
