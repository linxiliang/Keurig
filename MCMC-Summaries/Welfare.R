################################################################################
#
# Welfare Analysis
# Xiliang Lin
# Jan 2017
#
################################################################################
# Settings
rm(list = ls())
gc()

# Packages
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
setNumericRounding(0)

# Set Working Folder Path Here
setwd("~/Keurig")
meta_dir = "Data/Meta-Data/"
input_dir = "Data/Bayes-MCMC/"
data_input_dir = "Data/MLogit-Data"
graph_dir = "Tabfigs/MCMC-Summaries"
code_dir = "Scripts/MCMC-Summaries"
#------------------------------------------------------------------------------#

# Function
# Compute the welfare
