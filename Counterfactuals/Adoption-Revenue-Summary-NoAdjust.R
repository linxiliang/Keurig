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
library(ggplot2)
library(grid)
library(gridExtra)

# Set Working Folder Path Here
setwd("~/Keurig")
meta_dir  = "Data/Meta-Data"
HMS_input_dir = "Data/HMS-Transactions"
mlogit_dir = "Data/MLogit-Data"
input_dir = "Data/Machine-Adoption"
output_dir = "Data/Counterfactual"
graph_dir = "Tabfigs/Counterfactuals"

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
  hnames = names(hw_panel)
  dtemp = fread(paste(output_dir, "/Prob_type_", c, "_mu.csv",sep=""))
  knames = c("household_code", "t", paste0("c",c))
  setnames(dtemp, paste0("V", 1:3), knames)
  setkeyv(dtemp, c("household_code", "t"))
  hw_panel = dtemp[hw_panel, nomatch=0L]
  setkey(hw_panel, household_code, t)
  coln = c(hnames, knames[3])
  setcolorder(hw_panel, coln)
}


# Aggregate to get summary adoption by date
hh_sum = as.list(1:c)
k = 0
maxt = max(hw_panel[, t])
for (c in ctype){
  k = k+1
  kvar = paste0("c",c)
  setnames(hw_panel, kvar, "pvar")
  hw_panel[, prob:=as.numeric(NA)]
  for (it in 1:maxt){
    hw_panel[t<=it, prb_temp := prod(1-pvar), by = c("household_code")]
    hw_panel[t==it, prob := prb_temp]
  }
  hw_panel[, `:=`(ct = as.integer(c))]
  hh_sum[[k]] = hw_panel[, .(prob = mean(prob)), by = c("t", "ct")]
  setnames(hw_panel, "prob", paste0("prob",c))
  setnames(hw_panel, "pvar", kvar)
}
hh_sum = rbindlist(hh_sum)
hh_sum[, prob := 1-prob]
hh_sum[, t:=as.Date("2007-12-28") + (t-1)*7]
setkey(hh_sum, ct, t)
#---------------------------------------------------------------------------------------------------#
# Plot adoption rate over time 
hh_sum[, Type := ifelse(ct==1, "GMCR+Licensed+Third Party", ifelse(ct==2, "GMCR Only", "GMCR+Licensed"))]
hh_sum[, Type := factor(Type, levels = c("GMCR+Licensed+Third Party", "GMCR+Licensed", "GMCR Only"))]
adopt_graph= ggplot(hh_sum, aes(x = t, y = prob, colour = Type, linetype = Type)) + 
  geom_line() + theme_minimal()+theme(legend.justification=c(0,1), legend.position=c(0,1)) + 
  labs(x = "Time (Week)", y = "Adoption Rate")
ggsave(paste(graph_dir, "/figs/adoption_rate.pdf", sep=""), adopt_graph, width=10, height=6)

#---------------------------------------------------------------------------------------------------#
# Given adoption probability 
hh_rev = as.list(ctype)
k = 0
hw_panel[, nh:=.N, by = c("week_end")]
filename = paste(output_dir, "/HH-Rev-Panel.RData", sep="")
load(filename)
hh_br_rev = hh_br_rev[rev1<=200 & rev2<=200 & rev3<=200, ]
hh_br_rev = hh_br_rev[grepl("KEURIG", brand_descr), ]
setkey(hh_br_rev, household_code, week_end)
setkey(hw_panel, household_code, week_end)
hw_panel_merged = hw_panel[hh_br_rev, nomatch=0L]
for (c in ctype){
  k = k+1
  fvar1 = paste0("rev", c)
  fvar2 = paste0("prob",c)
  setnames(hw_panel_merged, c(fvar1, fvar2), c("fv1", "fv2"))
  hw_panel_merged[, rev := fv1*(1-fv2)]
  hw_panel_merged[, `:=`(ct = as.integer(c))]
  hh_rev[[k]] = hw_panel_merged[, .(rev = sum(rev)/mean(nh)), by = c("brand_descr", "t", "ct")]
  setnames(hw_panel_merged, c("fv1", "fv2"), c(fvar1, fvar2))
}

hh_rev = rbindlist(hh_rev)
hh_rev_agg = hh_rev[, .(rev = sum(rev)), by=c("t", "ct")]
hh_rev_gmcr = hh_rev[brand_descr%in%c("GREEN MOUNTAIN KEURIG", "KEURIG"), 
                     .(rev = sum(rev)), by=c("t", "ct")]
setkey(hh_rev_agg, ct, t)
setkey(hh_rev_gmcr, ct, t)
#---------------------------------------------------------------------------------------------------#
# Plot revenue over time
hh_rev_agg[, Type := ifelse(ct==1, "GMCR+Licensed+Third Party", ifelse(ct==2, "GMCR Only", "GMCR+Licensed"))]
hh_rev_agg[, Type := factor(Type, levels = c("GMCR+Licensed+Third Party", "GMCR+Licensed", "GMCR Only"))]
rev_graph= ggplot(hh_rev_agg, aes(x = t, y = rev, colour = Type, linetype = Type)) + 
  geom_line() + theme_minimal()+theme(legend.justification=c(0,1), legend.position=c(0,1)) + 
  labs(x = "Time (Week)", y = "Expected Revenue")
ggsave(paste(graph_dir, "/figs/overall_rev.pdf", sep=""), rev_graph, width=10, height=6)

#---------------------------------------------------------------------------------------------------#
# Plot revenue over GMCR
hh_rev_gmcr[, Type := ifelse(ct==1, "GMCR+Licensed+Third Party", ifelse(ct==2, "GMCR Only", "GMCR+Licensed"))]
hh_rev_gmcr[, Type := factor(Type, levels = c("GMCR+Licensed+Third Party", "GMCR+Licensed", "GMCR Only"))]
gmcr_rev_graph= ggplot(hh_rev_gmcr, aes(x = t, y = rev, colour = Type, linetype = Type)) + 
  geom_line() + theme_minimal()+theme(legend.justification=c(0,1), legend.position=c(0,1)) + 
  labs(x = "Time (Week)", y = "Expected Revenue")

ggsave(paste(graph_dir, "/figs/gmcr_rev.pdf", sep=""), gmcr_rev_graph, width=10, height=4)
