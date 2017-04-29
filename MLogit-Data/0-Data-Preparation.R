#####################################################################################################
#
# Preparing Data for Mixed Logit Bayesian Estimation
# Xiliang Lin
# March, 2016
#
#####################################################################################################

# Settings
rm(list = ls())
run_on_Linux        = TRUE    # Determines the path settings

coffee_modules      = c(1463)
#Module Description
#1463 - Ground and Whole Bean Coffee - including most Keurig K-Cups
#1464 - Soluable Flavored Coffee - Latte, Cappuccino etc.
#1465 - Soluable Coffee - Essentially instant coffee.
#1466 - Liquid Coffee- e.g. Starbucks Frappuccino.
#1467 - Coffee Substitutes such as POSTUM and Pero, tends to be fairly small.

maker_modules       = 7755
# Module Description
#7755 - Coffee and Tea Maker Appliances - Single Cup Serving or Dripping Machines.

# When filter households
ground_only = TRUE # Only purchases in ground coffee category will be considered. 

# Burnin period
burnindays = 45 # The number of days for burnin for identification of hardware adoption. 
                # Inclusive of 0s, it is about 80th percentile.

# Platform which I would to impute data
platform = "KEURIG"
focal_module = 1463 # Focus on ground and whole bean coffee 

# Purchases threshold
npurch = 5 # The purchase threshold for households to be selected.
rms_purch_criteria = 3 # Number of purchases needed to put in RMS to be included

# DMA Rank Threshold
dma_rank_threshold = 30 # Only DMAs with top N coffee consumption households

#Load Necessary Packages
library(glmnet)
library(data.table)
setNumericRounding(0)
library(bit64)
library(parallel)
library(nleqslv)
library(Rcpp)
library(RcppArmadillo)
library(nnet)
library(MASS)

#Set Working Folder Path Here
setwd("~/Keurig")
HMS_input_dir = "Data/HMS-Transactions"
HMS_trip_dir = "~/Data/Nielsen/HMS-Raw-R/Meta-Data"
RMS_input_dir = "~/Data/Nielsen/RMS-Build-2016/RMS-Processed/Modules"
meta_dir  = "Data/Meta-Data"
output_dir = "Data/MLogit-Data"
graph_dir = "Tabfigs/MLogit-Data"
code_dir  = "Scripts/MLogit-Data"

#Source the function file
source(paste(code_dir, '/functions.R', sep=""))
Rcpp::sourceCpp(paste(code_dir, '/base_prices.cpp', sep=""))
#---------------------------------------------------------------------------------------------------#

# Initialize Parallel Execution Environment
cores = detectCores(logical=TRUE)
cl = makeCluster(cores)
invisible(clusterEvalQ(cl,library(data.table)))
invisible(clusterEvalQ(cl, setNumericRounding(0)))
invisible(clusterEvalQ(cl,library(nleqslv)))

#---------------------------------------------------------------------------------------------------#
# Run the appropriate scripts.

# Identify the first purchase of Keurig/Other Single Serving Coffee
# Flag consumers appropriately.
source(paste(code_dir, '/1-Identify-HH.R', sep=""), echo=TRUE)

# Impute prices and availability
source(paste(code_dir, '/2-Retailer-Price-Panel.R', sep=""), echo=TRUE)

# Construct the HH purchase occasssion panel
source(paste(code_dir, '/3-HH-Panel.R', sep=""), echo=TRUE)

# Household MDC Panel
source(paste(code_dir, '/4-HH-Market-Panel-MDC.R', sep=""), echo=TRUE)

# Household Hardware panel
source(paste(code_dir, '/5-Single-Model-HW.R', sep=""), echo=TRUE)

#---------------------------------------------------------------------------------------------------#
