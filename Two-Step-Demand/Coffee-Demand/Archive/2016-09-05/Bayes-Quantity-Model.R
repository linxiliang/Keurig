#####################################################################################################
#
# Bayesian Estimation of Discrete Choice with Quantity Model
# Xiliang Lin
# April, 2016
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

# Directories
setwd("~/Keurig")
code_dir = "Scripts/Two-Step-Demand/Coffee-Demand/"
input_dir = "Data/MLogit-Data/"
output_dir = "Data/Bayes-MCMC/"
fig_dir = "Tabfigs/Bayes-MCMC/figs/"
tab_dir = "Tabfigs/Bayes-MCMC/tabs/"

#---------------------------------------------------------------------------------------------------------# 
# Initialize Parallel Execution Environment
cores = min(detectCores(logical=FALSE), as.integer(args[2]))
cl = makeCluster(cores)
invisible(clusterEvalQ(cl, library(data.table)))
invisible(clusterEvalQ(cl, setNumericRounding(0)))
invisible(clusterEvalQ(cl, library(bayesm)))
invisible(clusterEvalQ(cl, library(MASS)))
invisible(clusterEvalQ(cl, library(numDeriv)))
invisible(clusterEvalQ(cl, library(pracma)))

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
load(paste(input_dir,"Coffee-Panel-Cond-Purchase.RData",sep=""))

if (market_code != "all"){
  if (market_code == "remaining"){
    hh_in_market = hh_demo[!(dma_code %in% big_markets), unique(household_code)]
  } else{
    hh_in_market = hh_demo[dma_code==market_code, unique(household_code)]
  }
  hh_market_prod = hh_market_prod[household_code%in%hh_in_market, ]
}

# Reassign hh
hh_index_list = hh_market_prod[, unique(hh)]
hh_market_prod[, hh := .GRP, by = "hh"]
setkey(hh_market_prod,hh)
hh_market_prod[, t := .GRP, by = "t"]

# Get rid different sizes of the chosen brand and only leave the one that is chosen
hh_market_prod[, maxp := as.integer(purchased==max(purchased)), by = c("household_code", "t", "brand_descr")]
hh_market_prod = hh_market_prod[as.integer(maxp)==1, ]

# Convert size to log for estimation.
hh_market_prod[, size1_amount:=log(size1_amount)]

# Change total spent to the max price of available product or total_spent, whichever is greater
hh_market_prod[, price_max:=max(price), by = "trip_code_uc"]
hh_market_prod[, min_budget:=max(price*purchased)+1, by = "hh"]
hh_max_spent = hh_market_prod[, min_budget[1], by = "hh"]$V1
hh_max_spent = hh_max_spent + hh_max_spent*0.2
hh_modified = hh_market_prod[price<min_budget, ]
hh_modified[, price_mod := log(min_budget - price)]

# Create the Design Matrix
#bnames = paste0("a", c(2:18))
bnames = paste0("a", c(2:16))
for (i in 2:16){
  npresence = sum(hh_market_prod[, paste0("a", i), with=FALSE])
  if (npresence<=500) bnames = setdiff(bnames, paste0("a", i))
}
BMat = as.matrix(hh_market_prod[, bnames, with=FALSE])
xnames = c(bnames, "keurig", "brand_lag_ground", "brand_lag_keurig")
XMat = as.matrix(hh_market_prod[, xnames, with=FALSE])
XMat_mod = as.matrix(hh_modified[, c(xnames, "price_mod"), with=FALSE])

# Mark the start and end positions of households
hh_market_prod[, row_i:=1:.N]
hh_market_prod[, `:=`(start = min(row_i), end = max(row_i)), by = "household_code"]
hh_market_prod[, `:=`(nt_i = length(unique(t))), by  = "household_code"]

hh_modified[, row_i:=1:.N]
hh_modified[, `:=`(start = min(row_i), end = max(row_i)), by = "household_code"]
hh_modified[, `:=`(nt_i = length(unique(t))), by  = "household_code"]


# Number of Parameters to be estimated
np = length(xnames)+2
nh = length(hh_market_prod[, unique(hh)])
nt = length(hh_market_prod[, unique(t)])

# Estimate an homogeneous logit model to use it to tune RW draws
b0 = rep(0, np-1)
opt0 = optim(b0, ll_homo, gr = NULL, method = c("BFGS"))
hess = hessian(ll_homo, opt0$par)

# Evaluate Hessian at Fractional likelihood
clusterExport(cl,c('opt0', 'hess', 'xnames', 'ihessfun', 'hh_modified', 'nt'))
hess_list = parLapply(cl, 1:nh, ihessfun)

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
bhat0 = c(rep(0, np-1), 50);
beta0 = cbind(matrix(rnorm(nh*(np-1), mean=0, sd=3), nrow=nh), hh_max_spent);
Z = rep(1, nh);
A = 0;
Dbar = t(rep(0,np));
s2 = 2.93^2/np;

# Distribute the functions and relevant data to the workers.
clusterExport(cl,c('ll_linear', 'hh_market_prod', 'XMat', 'np', 'nt', 
                   'nh', 'pif_linear', 'rwmhd_linear', 's2', 'hess_list'))

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
  # for (i in 1:nh) b0 = rwmhd_linear(i); beta0[i, ]=b0[1:np]; T0[i,] = b0[np+1] # non-parallel version
  
  # Parallel version
  clusterExport(cl, c('sig', 'bhat', 'beta0'))
  beta_list = parLapply(cl, 1:nh, rwmhd_linear)
  beta0 = matrix(unlist(beta_list), ncol=np, byrow=TRUE)

  cat("Finished drawing", d, "out of", totdraws, ", and total time elapsed:",  proc.time()[3] - start_time, "\n\n")
}

stopCluster(cl)

# Save output to a dataset
save(hh_index_list, bhatd, sigd, bindv, bnames, 
     file = paste(output_dir, "Normal-MCMC-", market_code, ".RData", sep = ""))

# T is not diffused enough and step sizes are not adequate.


