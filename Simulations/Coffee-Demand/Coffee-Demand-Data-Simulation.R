#####################################################################################################
#
# Multiple Discrete-continuous Extreme Value 
# Simulate Data - Use the same environment and only simulate preferences
# Xiliang Lin
# March, 2017
#
#####################################################################################################
# Settings
rm(list = ls())               # Clear workspace

# Load Required packages
require(MASS)
require(data.table)
setNumericRounding(0)
require(bayesm)

# Directories
setwd("~/Keurig")
code_dir = "Scripts/Two-Step-Demand/Coffee-Demand/"
input_dir = "Data/MLogit-Data/"
output_dir = "Data/Simulation-Data/"

# Set seed
RNGkind("L'Ecuyer-CMRG")
set.seed(21322132)

#---------------------------------------------------------------------------------------------------------#
# Set Preferences
# Use the last draw of my MCMC chain - can actually use other values.
load(paste(input_dir,"Simulation-TrueParams.RData",sep=""))

# Spending - Assume lognormal distribution
spend_mu = 2
spend_sd = 0.48
#---------------------------------------------------------------------------------------------------------#
# Load environment data
load(paste(input_dir,"MDC-Cond-Purchase-Flavors.RData",sep=""))
onames = names(hh_market_prod)

# Reset Key
setkey(hh_market_prod, hh, t, brand, roast)

# Simulate preferences based on 
nh = hh_market_prod[, length(unique(hh))]
pref = mvrnorm(nh, Betas, Sigma)
pref[, 27:28] = exp(pref[, 27:28])/(1+exp(pref[, 27:28]))

# Simulate spending
nt = hh_market_prod[, length(unique(t))]
spending = exp(rnorm(nt, mean=spend_mu, sd=spend_sd))
spending = data.table(t = hh_market_prod[, unique(t)], E = spending)

# Merge Spending with Data 
setkey(hh_market_prod, t)
setkey(spending, t)
hh_market_prod = hh_market_prod[spending, nomatch=0L]
setkey(hh_market_prod, hh, t, brand, roast)

# Generate IID error for quality index
hh_market_prod[, eps:=-log(-log(runif(.N)))]

#---------------------------------------------------------------------------------------------------------#
# Functions for solving optimal choice
# Solve Lambda
BC<-function(lambda, alpha, zb, p, eps, E) return(sum(p*(exp((log(lambda)+log(p)-zb-eps)/(alpha-1))-1))-E)

# Obtain the mariginal utility at each product
EXPEND<-function(lambda, alpha, zb, p, eps, E) return(p*(exp((log(lambda)+log(p)-zb-eps)/(alpha-1))-1))

# Solve the optimization problem using iterative elimination
eu2rev <- function(umin, umax, alpha, zb, p, eps, E){
  if (length(p)==1){
    return(E)
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
    Spent[-(ind0)] = 0
    return(Spent)
  }
}
#---------------------------------------------------------------------------------------------------------#
# Compute the choices for each household for given budget 

# Compute the mean component of quality index of preferences and decay parameter
# Note - last chosen brand is taken as given -- it doesn't affect the way model is estimated
xvars = c(paste0("a", 2:15), "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
          "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")
for (h in 1:nh){
  xmat= as.matrix(hh_market_prod[.(h), xvars, with = F])
  kmat = as.matrix(hh_market_prod[.(h), cbind(1-keurig, keurig)])
  
  # Compute the quality of product
  hh_market_prod[.(h), zb := xmat %*% pref[h, 1:26]]
  
  # Compute the alpha of the product
  hh_market_prod[.(h), alpha := kmat %*% pref[h, 27:28]]
}

# Eliminate Choices 
hh_market_prod[, `:=`(UE = 1/price * exp(zb + eps)*(E/price+1)^(alpha-1),
                      U0 = 1/price * exp(zb + eps))]
hh_market_prod[, spent:=0]
hh_market_prod[, `:=`(p_choices = as.integer(U0>=max(UE))), by = c("hh", "t")]
hh_market_prod[p_choices==1, spent:=eu2rev(min(UE-0.00001), max(U0+0.00001), alpha, zb, price, eps, E[1]), 
               by = c("hh", "t")]

# Now map back to old variables 
hh_market_prod[, `:=`(purchased = as.integer(spent>0), size = spent/price)]
hh_market_prod[, `:=`(total_price_paid = sum(spent), coupon_value=0), by = c("hh", "t")]

# Keep only the original variables
hh_market_prod = hh_market_prod[, onames, with=F]
setkey(hh_market_prod, hh, t, brand, roast)
save(hh_market_prod, file = paste(output_dir,"Simulated-Purchases.RData",sep=""))
