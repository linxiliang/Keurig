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
# market_code = c(501, 506, 504, 602, 803, 511, 539, 623, 618, 505, 
#                613, 819, 524, 534, 533, 753, 510, 508, 514, 512, 
#                517, 807, 751, 862, 535, 521, 548, 609, 566, 641) # Big Markets
market_code = "all"
sratio = 0.05 # Sampling ratio if needed.

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

# Set seed
RNGkind("L'Ecuyer-CMRG")
set.seed(12345)

#---------------------------------------------------------------------------------------------------------# 
# Initialize Parallel Execution Environment
primary <- 'bushgcn01'
machineAddresses <- list(
  list(host=primary, user='xlin0',
       ncore=20),
  list(host='bushgcn02',user='xlin0',
       ncore=20),
  list(host='bushgcn03',user='xlin0',
       ncore=20),
  list(host='bushgcn04',user='xlin0',
       ncore=20),
  list(host='bushgcn05',user='xlin0',
       ncore=20),
  list(host='bushgcn06',user='xlin0',
       ncore=20)
  )
spec <- lapply(machineAddresses,
               function(machine) {
                 rep(list(list(host=machine$host,
                               user=machine$user)),
                     machine$ncore)
               })
spec <- unlist(spec,recursive=FALSE)
cl <- makeCluster(type='PSOCK', master=primary, spec=spec)

invisible(clusterEvalQ(cl, library(data.table)))
invisible(clusterEvalQ(cl, setNumericRounding(0)))
invisible(clusterEvalQ(cl, library(bayesm)))
invisible(clusterEvalQ(cl, library(MASS)))
invisible(clusterEvalQ(cl, library(numDeriv)))
invisible(clusterEvalQ(cl, library(pracma)))
invisible(clusterEvalQ(cl, RNGkind("L'Ecuyer-CMRG")))

# Set seed for each workers
s = .Random.seed
for (i in 1:length(cl)) {
  s = nextRNGStream(s)
  clusterExport(cl[i], c('s'))
  # send s to worker i as .Random.seed
}
invisible(clusterEvalQ(cl, .Random.seed <- s))

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
    hh_in_market = hh_demo[dma_code%in%market_code, unique(household_code)]
  }
  hh_market_prod = hh_market_prod[household_code%in%hh_in_market, ]
}

source(paste(code_dir, 'Preprocessing.R', sep=""))

# Reassign hh and t
setkey(hh_market_prod, household_code, t, brand_descr, keurig, roast, brand_descr_orig)
hh_market_prod[, hh := .GRP, by = "hh"]
setkey(hh_market_prod, hh, t)
hh_market_prod[, t := .GRP, by = "t"]

# Reset Key
setkey(hh_market_prod, hh, t, brand, roast)
hh_code_list = unique(hh_market_prod[, .(hh, household_code)])

# Count number of choice occasions
hh_market_prod[, `:=`(nt_i = length(unique(t))), by  = "household_code"]

# Obtain the M --- Number of items chosen in the trip
hh_market_prod[, `:=`(M = sum(purchased)), by = c("household_code", "t")]

# Create Expenditure based on size and aggregate price -- not actual price (Robustness check needed)
hh_market_prod[, expenditure := price * size]
# Or
# hh_market_prod[, expenditure := total_price_paid - coupon_value] # correlation is high.
# Create log price and log expenditure part
hh_market_prod[, `:=`(lprice=log(price), lexpend=log(expenditure/price+1))]
hh_full = copy(hh_market_prod)

# Obtain the list of households
hh_n_list = hh_full[, unique(hh)]
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
  hh_market_prod = hh_full[hh%in%hh_list, ]
  clusterExport(cl[i], c('hh_list', 'hh_market_prod'))
}

#---------------------------------------------------------------------------------------------------------#
# Parameter setting
# Number of Parameters to be estimated
nk = ncol(KMat)
nx = ncol(XMat)
np = nk+nx # if sigma is estimated, put + 1
nh = length(hh_full[, unique(hh)])
nt = length(hh_full[, unique(t)])
clusterExport(cl, c('xnames', 'll', 'ihessfun', 'input_dir', "nk", "nx", "np", "nh"))
invisible(clusterEvalQ(cl, setwd("~/Keurig")))
invisible(clusterEvalQ(cl, load(paste(input_dir, "/HH-Aux-Market.RData", sep=""))))

#---------------------------------------------------------------------------------------------------------#
# Model Tuning
# Estimate an homogeneous logit model to use it to tune RW draws
b0 = rnorm(np)
opt0 = optim(b0, ll_homo, gr = NULL, method = c("BFGS"), control = list(reltol=1e-16))
hess = hessian(ll_homo, b0, h = 1e-4)
save(opt0, hess, file = paste(input_dir, "/Homo-Hessian.RData", sep=""))

# Evaluate Hessian at Fractional likelihood
invisible(clusterEvalQ(cl, load(paste(input_dir, "/Homo-Hessian.RData", sep=""))))
hess_list = clusterEvalQ(cl, lapply(hh_list, ihessfun))
hess_list = unlist(hess_list, recursive=F)
save(opt0, hess, hess_list, file = paste(input_dir, "/Hessian.RData", sep=""))
# save(opt0, hess, hess_list, file = paste(input_dir, "/Posterior-Variance.RData", sep=""))
gc()
#---------------------------------------------------------------------------------------------------------#
# Bayesian Estimation 
# MCMC Settings
burnin = 0
thin   = 6
draws  = 10000
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
invisible(clusterEvalQ(cl, load(paste(input_dir, "/Hessian.RData", sep=""))))
# invisible(clusterEvalQ(cl, load(paste(input_dir, "/Posterior-Variance.RData", sep=""))))
clusterExport(cl,c('i_ll', 'rwmhd', 's2'))

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
  beta_list = clusterEvalQ(cl, lapply(hh_list, rwmhd))
  beta_list = unlist(beta_list, recursive=F)
  beta0 = matrix(unlist(beta_list), ncol=np, byrow=TRUE)
  
  cat("Finished drawing", d, "out of", totdraws, ", and total time elapsed:", 
      proc.time()[3] - start_time, "\n\n")
}

# Save output to a dataset
inx = seq(1, 10000, 4)
bindv = bindv[inx,,]
save(hh_code_list, bhatd, sigd, bindv, bnames, 
     file = paste(output_dir, "MDCEV-MCMC-All-60000.RData", sep = ""))
stopCluster(cl)
