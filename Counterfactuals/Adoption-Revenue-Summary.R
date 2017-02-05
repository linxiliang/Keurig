#####################################################################################################
#
# Summarize Adoption and Welfare Loss
# Xiliang Lin
# Jan, 2017
#
#####################################################################################################
# Settings
rm(list = ls())

coffee_modules      = c(1463)
maker_modules       = 7755
big_markets = c(501, 506, 504, 602, 803, 511, 539, 623, 618, 505, 
                613, 819, 524, 534, 533, 753, 510, 508, 514, 512, 
                517, 807, 751, 862, 535, 521, 548, 609, 566, 641) 

# Load Necessary Packages
library(parallel)
library(data.table)
setNumericRounding(0)
library(nloptr)

# Set Working Folder Path Here
setwd("~/Keurig")
meta_dir  = "Data/Meta-Data"
HMS_input_dir = "Data/HMS-Transactions"
mlogit_dir = "Data/MLogit-Data"
input_dir = "Data/Machine-Adoption"
output_dir = "Data/Counterfactual"

# types of counterfactuals
ctype = c(1,2,3)
mtype = c(1,2,3)

# Source function
source('Scripts/Counterfactuals/machine-functions.R')

#---------------------------------------------------------------------------------------------------#
# Load and combine data
load(paste(output_dir, "/HW-Full-Panel.RData", sep=""))
setkey(hw_panel, household_code, t)
for (c in ctype){
  for (m in mtype){
    hnames = names(hw_panel)
    dtemp = fread(paste(output_dir, "/Prob_type_", c, "_mu_", m, ".csv",sep=""))
    knames = c("household_code", "t", paste0("c",c,"m",m))
    setnames(dtemp, paste0("V", 1:3), knames)
    setkeyv(dtemp, c("household_code", "t"))
    hw_panel = dtemp[hw_panel, nomatch=0L]
    setkey(hw_panel, household_code, t)
    coln = c(hnames, knames[3])
    setcolorder(hw_panel, coln)
  }
}

# Aggregate to get summary adoption by date
hh_sum = as.list(1:(c*m))
k = 0
maxt = max(hw_panel[, t])
for (c in ctype){
  for (m in mtype){
    k = k+1
    kvar = paste0("c",c,"m",m)
    setnames(hw_panel, kvar, "pvar")
    hw_panel[, prob:=as.numeric(NA)]
    for (it in 1:313){
      hw_panel[t<=it, prb_temp := prod(pvar), by = c("household_code")]
      hw_panel[t==it, prob := prb_temp]
    }
    hw_panel[, `:=`(ct = c, mt = m)]
    hh_sum[[k]] = hw_panel[, .(prob = mean(prob)), by = c("t", "ct", "mt")]
    setnames(hw_panel, "pvar", kvar)
  }
}
#---------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------#
# Given adoption probability 
for (c in ctype){
  if (c==1){
    filename = paste(output_dir, "/HH-Rev-Panel.RData", sep="")
  } else if (c==2) {
    filename = paste(output_dir, "/HH-Rev-Homo-Panel.RData", sep="")
  } else{
    filename = paste(output_dir, "/HH-Rev-No-Variety-Panel.RData", sep="")
  }
  load(filename)
  for (m in mtype){
    vname = paste()

  }
}

