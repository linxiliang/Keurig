#####################################################################################################
#
# Counterfactual Analysis for the Keurig Machine Adoption
# Xiliang Lin
# June, 2016
#
#####################################################################################################
# Settings
rm(list = ls())               # Clear workspace

# Packages
library(data.table)
setNumericRounding(0)

# Set Working Folder Path Here
setwd("~/Keurig")
input_dir = "Data/Bayes-MCMC/"
graph_dir = "Tabfigs/Counterfactuals/"

#----------------------------------------------------------------------------------------------------#

# Load counterfactual analysis data
pprob = fread(paste(input_dir, "pprob.csv", sep = ""))
load("Data/Machine-Adoption/HH-HW-Brand-Panel.RData")

# Data
hw_br_panel[, pr:=pprob[,V1]]
hw_br_panel[, pr_max := max(pr), by = "ntrip"]
hw_br_panel[pr<pr_max, .N, by = "brand_descr"]

# Obtain the probability change
bpr_agg = hw_br_panel[, .(pr = mean(pr)), by = "brand_descr"]
bpr_agg[, change := pr/pr[1]]