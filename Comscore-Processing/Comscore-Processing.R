#####################################################################################################
#
# Comscore Data Processing - Estimate Consumer Awareness
# Xiliang Lin
# Nov, 2015
#
#####################################################################################################
# Settings
rm(list = ls())
run_on_Linux        = FALSE    # Determines the path settings


#Load Necessary Packages
library(data.table)
setNumericRounding(0)

#Set Working Folder Path Here
if (run_on_Linux) {
  setwd("~")
} else{
  setwd("D:/cygwin64/home/xlin0")
}
input_dir = "!Data/Comscore"
output_dir = "Keurig-Competition-Externality/Data/Comscore"
tabfig_dir = "Keurig-Competition-Externality/Tabfigs/Comscore-Processed"
