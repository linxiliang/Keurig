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
sratio = 0.01 # Sampling ratio if needed.

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
set.seed(1234567)

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
primary <- 'bushgcn10'
machineAddresses <- list(
  list(host=primary, user='xlin0',
       ncore=12),
  list(host='bushgcn11',user='xlin0',
       ncore=12),
  list(host='bushgcn12',user='xlin0',
       ncore=12),
  list(host='bushgcn33',user='xlin0',
       ncore=12),
  list(host='bushgcn34',user='xlin0',
       ncore=12),
  list(host='bushgcn35',user='xlin0',
       ncore=12),
  list(host='bushgcn36',user='xlin0',
       ncore=12),
  list(host='bushgcn37',user='xlin0',
       ncore=12),
  list(host='bushgcn38',user='xlin0',
       ncore=12),
  list(host='bushgcn39',user='xlin0',
       ncore=12)
  )
# 
primary <- 'bushgcn27'
machineAddresses <- list(
  list(host=primary, user='xlin0',
       ncore=32),
  list(host='bushgcn26',user='xlin0',
       ncore=32),
  list(host='bushgcn29',user='xlin0',
       ncore=32)
)

primary <- 'bushgcn10'
machineAddresses <- list(
  list(host=primary, user='xlin0',
       ncore=24),
  list(host='bushgcn11',user='xlin0',
       ncore=24),
  list(host='bushgcn12',user='xlin0',
       ncore=24),
  list(host='bushgcn13',user='xlin0',
       ncore=24)
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
source(paste(code_dir, 'mdc-functions-OX.R', sep=""))

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
hh_market_prod = hh_market_prod[brand_descr!="0NOTHING", ]
hh_market_prod[grep(" KEURIG", brand_descr), keurig:=1]
# Make nprod and nbrand to logs
hh_market_prod[, `:=`(nbrand = log(nbrand), nprod = log(nprod))]
hh_market_prod[,`:=`(nbrandg=ifelse(keurig==0, nbrand, 0), nbrandk=ifelse(keurig==1, nbrand, 0))]
gc()

# Names settings
nb = hh_market_prod[, max(brand)]
bnames = paste0("a", c(2:nb))
xnames = c(bnames, "keurig", "flavored", "lightR", "medDR", "darkR", "assorted", "kona",
          "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")
znames = c("overall_rate", "inc40", "inc50", "inc60", "inc70",
           "hhsize2", "hhsize3", "hhsize5", "twofamily", "threefamily",
           "fulltime", "presence_of_children",
           "african_american", "hispanic", "total_spent", "total_spent_hhi")
hh_demo[, `:=`(total_spent = log(total_spent), total_spent_hhi=total_spent_hhi/100,
               overall_rate = log(overall_rate))]

if (market_code != "all"){
  if (market_code == "remaining"){
    hh_in_market = hh_demo[!(dma_code %in% big_markets), unique(household_code)]
  } else{
    hh_in_market = hh_demo[dma_code%in%market_code, unique(household_code)]
  }
  hh_market_prod = hh_market_prod[household_code%in%hh_in_market, ]
}

source(paste(code_dir, 'Preprocessing-O1.R', sep=""))

# Reassign hh and t if needed
setkey(hh_market_prod, household_code, t, brand_descr, keurig, roast, brand_descr_orig)
hh_market_prod[, hh:= .GRP, by = "hh"]
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
  if (i==ncl){
    hh_list = sort(hh_n_list)
  } else{
    hh_list = hh_n_list[1:nhsize]
  }
  hh_n_list = setdiff(hh_n_list, hh_list)
  hh_market_prod = hh_full[hh%in%hh_list, ]
  hh_demo_chunk = hh_demo[hh%in%hh_list, ]
  clusterExport(cl[i], c('hh_list', 'hh_market_prod', 'hh_demo_chunk'))
}

#---------------------------------------------------------------------------------------------------------#
# Parameter setting
# Number of Parameters to be estimated
nz = length(znames) + 1
nk = ncol(KMat)
nx = ncol(XMat)
np = nk+nx+1# if sigma is estimated, put + 1
nh = length(hh_full[, unique(hh)])
nt = length(hh_full[, unique(t)])
clusterExport(cl, c('xnames', 'znames', 'detfun', 'll', 'i_par', 'ihessfun', 'input_dir', "nk", "nx", "np", "nh"))
invisible(clusterEvalQ(cl, setwd("~/Keurig")))
invisible(clusterEvalQ(cl, load(paste(input_dir, "/HH-Aux-Market.RData", sep=""))))

#---------------------------------------------------------------------------------------------------------#
# Model Tuning
# Estimate an homogeneous logit model to use it to tune RW draws
b0 = rnorm(np)
opt0 = optim(b0, ll_homo, gr = NULL, method = c("BFGS"), control = list(reltol=1e-16))
hess = hessian(ll_homo, opt0$par, h = 1e-4)
save(opt0, hess, file = paste(input_dir, "/Homo-Hessian-OX.RData", sep=""))

# Evaluate Hessian at Fractional likelihood
invisible(clusterEvalQ(cl, load(paste(input_dir, "/Homo-Hessian-OX.RData", sep=""))))
par_list = clusterEvalQ(cl, lapply(hh_list, i_par))
par_list = unlist(par_list, recursive=F)
clusterExport('par_list')
hess_list = clusterEvalQ(cl, lapply(hh_list, ihessfun))
hess_list = unlist(hess_list, recursive=F)
save(opt0, hess, par_list, hess_list, file = paste(input_dir, "/Hessian-OX.RData", sep=""))
# save(opt0, hess, hess_list, file = paste(input_dir, "/Posterior-Variance.RData", sep=""))
# save(opt0, hess, hess_list, file = paste(input_dir, "/Hess-OX-NAR.RData", sep=""))
gc()
#---------------------------------------------------------------------------------------------------------#
# Bayesian Estimation 
# MCMC Settings
burnin = 0
thin   = 3
draws  = 8000
tunein = 0
totdraws = draws*thin + burnin

## Auxiliary prior settings, and computations
nu0 = max(c(4, 0.01*nh, np))
V0 = nu0 * diag(np);
sig0 = rwishart(nu0, solve(V0))$IW;
Z = cbind(rep(1, nh), as.matrix(hh_demo[, znames, with=F]))
A = nu0 * diag(diag(var(Z)));
Dbar = matrix(rep(0, nz*np), nrow=nz)
s2 = 2.93^2/np;

# Distribute the functions and relevant data to the workers.
invisible(clusterEvalQ(cl, load(paste(input_dir, "/Hess-OX-NAR.RData", sep=""))))
invisible(clusterEvalQ(cl, load(paste(input_dir, "/Homo-OX-Opt0.RData", sep=""))))
invisible(clusterEvalQ(cl, load(paste(input_dir, "/par_temp.RData", sep=""))))
# invisible(clusterEvalQ(cl, load(paste(input_dir, "/Posterior-Variance.RData", sep=""))))
clusterExport(cl,c('i_ll', 'rwmhd', 'prefrun', 's2', 'tunein', 'initrun', 'bcorrect'))
invisible(clusterEvalQ(cl, (Z_i=cbind(rep(1, nrow(hh_demo_chunk)), as.matrix(hh_demo_chunk[, znames, with=F])))))
# invisible(clusterEvalQ(cl, (beta_dt=data.table(matrix(c(rnorm(length(hh_list)*(np-3), mean=0, sd=2),
#                                                         rnorm(length(hh_list), mean=2, sd=0.5),
#                                                         rnorm(length(hh_list), mean=2, sd=0.5),
#                                                         rnorm(length(hh_list), mean=-2, sd=0.02)),
#                                                       ncol=length(hh_list), byrow=T)))))
invisible(clusterEvalQ(cl, (beta_dt=data.table(opt0$par + matrix(c(rnorm(length(hh_list)*(np-3), mean=0, sd=1),
                                                        rnorm(length(hh_list), mean=0, sd=0.1),
                                                        rnorm(length(hh_list), mean=0, sd=0.1),
                                                        rnorm(length(hh_list), mean=0, sd=0.1)),
                                                      ncol=length(hh_list), byrow=T)))))
invisible(clusterEvalQ(cl, setnames(beta_dt, names(beta_dt), as.character(hh_list))))
invisible(clusterEvalQ(cl, (hh_list_samp = sort(sample(hh_list, ceiling(length(hh_list)*0.10))))))

# Tune the betas for each household
beta0 = clusterEvalQ(cl, initrun())
#beta0 = clusterEvalQ(cl, bcorrect())
beta0 = clusterEvalQ(cl, beta_dt)
bhnames = as.integer(unlist(lapply(beta0, names)))
beta0 = matrix(t(unlist(beta0)), ncol=np, byrow=T)
beta0 = beta0[order(bhnames), ]

# Intialize the tuning dataset
invisible(clusterEvalQ(cl, (ac_dt=data.table(hh=hh_list))))
invisible(clusterEvalQ(cl, ac_dt[, `:=`(nd=0,nac=0,lambda=1)]))
invisible(clusterEvalQ(cl, setkey(ac_dt, hh)))

#Initialize storage of MCMC draws
bhatd = matrix(rep(0, draws*np), nrow=draws)
deltad = matrix(rep(0, draws*(np*nz)), nrow=draws)
sigd = array(0, dim=c(np, np, draws))
bindv = array(0, dim=c(draws - ceiling((tunein - burnin)/thin), nh, np))
gc()

## Gibbs Sampling
start_time = proc.time()[3]
for (d in 1:totdraws){
  # For given betas - preference matrix, draw posterior mean and covariance matrix
  # Compute the posterior mean and variance of betas
  Z_s = Z[bhnames, ]
  n_s = length(bhnames)

  Dhat = solve(t(Z_s) %*% Z_s) %*% t(Z_s) %*% beta0
  Dtild = solve(t(Z_s) %*% Z_s + A) %*% (t(Z_s) %*% Z_s %*% Dhat + A %*% Dbar)     
  S    = t(beta0 - Z_s %*% Dtild) %*% (beta0 - Z_s %*% Dtild) + t(Dtild - Dbar) %*% A %*% (Dtild - Dbar)
  
  # Draw from the posterior
  sig  = rwishart(nu0+n_s, solve(V0+S))$IW
  delta = mvrnorm(1, as.vector(Dtild), kronecker(sig, solve(t(Z_s)%*%Z_s + A)))
  Delta = matrix(delta, nrow=nz)
  bhat_all = Z_s %*% Delta
  bhat = colMeans(bhat_all)
  print(bhat)
  
  # Store the posterior draws of mean betas and sigmas
  if ((d > burnin) & (d %% thin == 0)){
    indx = ceiling((d - burnin)/thin)
    bhatd[indx,] = bhat; 
    deltad[indx,] = delta;
    sigd[, ,indx] = sig;
    bnorder = order(bhnames)
    if (d > tunein) bindv[indx - ceiling((tunein - burnin)/thin), ,] = beta0[bnorder, ];
  }
  
  # RW MH to draw betas for each household
  # Non-parallel version 
  # for (i in 1:nh) {b0 = rwmhd_simple(i); beta0[i, ]=b0}
  
  # Parallel version
  clusterExport(cl, c('sig', 'Delta', 'd'))
  if (d %% 500==0 & d <= (tunein+1)){
    for (i in 1:30) beta_list = clusterEvalQ(cl, prefrun())
    invisible(clusterEvalQ(cl, (hh_list_samp = sort(sample(hh_list, ceiling(length(hh_list)*0.10))))))
  }
  beta_list = clusterEvalQ(cl, prefrun())
  bhnames = as.integer(unlist(lapply(beta_list, names)))
  beta0 = matrix(unlist(beta_list), ncol=np, byrow=T)
  
  cat("Finished drawing", d, "out of", totdraws, ", and total time elapsed:", 
      proc.time()[3] - start_time, "\n\n")
}

# Save output to a dataset
inx = seq(4001, 8000, 2)
bac_dt = invisible(clusterEvalQ(cl, ac_dt))
bac_dt = rbindlist(bac_dt)
setkey(bac_dt, hh)
# bindv = bindv[inx,,]
# save(hh_code_list, bhatd, deltad, sigd, bindv, bnames, 
#      file = paste(output_dir, "MDCEV-MCMC-All-30000.RData", sep = ""))
# stopCluster(cl)

#save(hh_code_list, bhatd, deltad, sigd, bindv, bnames, 
#      file = paste(output_dir, "MDCEV-MCMC-O1.RData", sep = ""))
save(hh_code_list, bhatd, deltad, sigd, bindv, bnames, bac_dt,
      file = paste(output_dir, "MDCEV-MCMC-OX-NoAdjust.RData", sep = ""))
save(hh_code_list, bhatd, deltad, sigd, bindv, bnames, bac_dt,
     file = paste("Keurig/Data/Bayes-MCMC/MDCEV-MCMC-OX.RData", sep = ""))
save(hh_code_list, bhatd, deltad, sigd, bindv, bnames, bac_dt, beta0, 
     file = paste(output_dir, "MDCEV-MCMC-OX-All.RData", sep = ""))

save(hh_code_list, bhatd, deltad, sigd, bindv, bnames, bac_dt, beta0, 
     file = paste(output_dir, "MDCEV-MCMC-OX2.RData", sep = ""))
save(hh_code_list, bhatd, deltad, sigd, bindv, bnames, bac_dt, beta0, 
     file = paste(output_dir, "MDCEV-MCMC-OX-NoAdjust.RData", sep = ""))
