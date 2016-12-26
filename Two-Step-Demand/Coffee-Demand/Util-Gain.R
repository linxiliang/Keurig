#####################################################################################################
#
# Estimate Utility Gain
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

# Directories
setwd("~/Keurig")
code_dir = "Scripts/Two-Step-Demand/Coffee-Demand/"
input_dir = "Data/MLogit-Data/"
output_dir = "Data/Bayes-MCMC/"
fig_dir = "Tabfigs/Bayes-MCMC/figs/"
tab_dir = "Tabfigs/Bayes-MCMC/tabs/"

#Set Working Folder Path Here
setwd("~/Keurig")
HMS_input_dir = "Data/HMS-Transactions"
HMS_trip_dir = "~/!Data/Nielsen/HMS-Raw-R/Meta-Data"
RMS_input_dir = "~/!Data/Nielsen/RMS-Raw-R/Coffee-Related/Movement"
meta_dir  = "Data/Meta-Data"
output_dir = "Data/MLogit-Data"
graph_dir = "Tabfigs/MLogit-Data"
code_dir  = "Scripts/MLogit-Data"


#---------------------------------------------------------------------------------------------------------#
#Source the function file
source(paste(code_dir, 'mdc-functions.R', sep=""))

# Obtain the means 
indv = colMeans(bindv[1200:2000,,],3)

alpha = rep(0.95, 10)
p = runif(10)+0.5
eps = -log(-log(runif(10))) 
E = 10

# Bounds 
e_lb = rep(0, length(p))
e_ub = rep(E, length(p))

# Test
#ex0 = runif(10)
ex0 = rep(0, 10)


# Solve using NLOPT_LD_MMA with gradient information supplied in separate function
res0 <- nloptr( x0=ex0, 
                eval_f=eval_f0, 
                eval_grad_f=eval_grad_f0,
                lb = e_lb, 
                ub = e_ub, 
                eval_g_ineq = eval_g0,
                eval_jac_g_ineq = eval_jac_g0,                
                opts = list("algorithm"="NLOPT_LD_MMA", xtol_rel = 1e-16, ftol_abs = 0))
print( res0 )
