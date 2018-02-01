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
    for (it in 1:maxt){
      hw_panel[t<=it, prb_temp := prod(pvar), by = c("household_code")]
      hw_panel[t==it, prob := prb_temp]
    }
    hw_panel[, `:=`(ct = as.integer(c), mt = as.integer(m))]
    hh_sum[[k]] = hw_panel[, .(prob = mean(prob)), by = c("t", "ct", "mt")]
    setnames(hw_panel, "prob", paste0("prob",c,"_",m))
    setnames(hw_panel, "pvar", kvar)
  }
}
hh_sum = rbindlist(hh_sum)
hh_sum[, prob := 1-prob]
setkey(hh_sum, ct, mt, t)
#---------------------------------------------------------------------------------------------------#
# Plot adoption rate over time 
hh_sum[, Type := ifelse(mt==1, "GMCR Only", ifelse(mt==2, "GMCR+Third Party", "GMCR+Licensed"))]
adpt1 = ggplot(hh_sum[.(1), ], aes(x=t, y=prob, group = mt, colour= Type))+
  theme(legend.position=c(0.3,0.8), plot.title = element_text(size=12))+
  geom_line(aes(linetype=Type))+ylim(0, 0.80)+labs(list(title="Original Households", x="Time", y="Adoption Rate"))
adpt2 = ggplot(hh_sum[.(2), ], aes(x=t, y=prob, group = mt, colour= Type))+
  theme(legend.position="none", plot.title = element_text(size=12), axis.text.y=element_blank(), 
        axis.ticks=element_blank(), axis.title.y=element_blank())+
  geom_line(aes(linetype=Type))+ylim(0, 0.80)+labs(list(title="Homogeneous but Variety Seeking", x="Time"))
adpt3 = ggplot(hh_sum[.(3), ], aes(x=t, y=prob, group = mt, colour= Type))+
  theme(legend.position="none", plot.title = element_text(size=12), axis.text.y=element_blank(), 
        axis.ticks=element_blank(), axis.title.y=element_blank())+
  geom_line(aes(linetype=Type))+ylim(0, 0.80)+labs(list(title="Heterogeneous but Not Variety Seeking", x="Time"))
multplot = marrangeGrob(list(adpt1, adpt2, adpt3), ncol=3, nrow=1, top="")
ggsave(paste(graph_dir, "/figs/adoption_rate.pdf", sep=""), multplot, width=10, height=4)

#---------------------------------------------------------------------------------------------------#
# Given adoption probability 
hh_rev = as.list(1:(c*m))
k = 0
hw_panel[, nh:=.N, by = c("week_end")]
for (c in ctype){
  if (c==1){
    filename = paste(output_dir, "/HH-Rev-Panel.RData", sep="")
  } else if (c==2) {
    filename = paste(output_dir, "/HH-Rev-Homo-Panel.RData", sep="")
  } else{
    filename = paste(output_dir, "/HH-Rev-No-Variety-Panel.RData", sep="")
  }
  load(filename)
  hh_br_rev = hh_br_rev[rev1<=100 & rev2<=100 & rev3<=100, ]
  hh_br_rev = hh_br_rev[grepl("KEURIG", brand_descr), ]
  setkey(hh_br_rev, household_code, week_end)
  setkey(hw_panel, household_code, week_end)
  hw_panel_merged = hw_panel[hh_br_rev, nomatch=0L]
  for (m in mtype){
    k = k+1
    fvar1 = paste0("rev", m)
    fvar2 = paste0("prob",c,"_",m)
    setnames(hw_panel_merged, c(fvar1, fvar2), c("fv1", "fv2"))
    hw_panel_merged[, rev := fv1*(1-fv2)]
    hw_panel_merged[, `:=`(ct = as.integer(c), mt = as.integer(m))]
    hh_rev[[k]] = hw_panel_merged[, .(rev = sum(rev)/mean(nh)), by = c("brand_descr", "t", "ct", "mt")]
    setnames(hw_panel_merged, c("fv1", "fv2"), c(fvar1, fvar2))
  }
}
hh_rev = rbindlist(hh_rev)
hh_rev_agg = hh_rev[, .(rev = sum(rev)), by=c("t", "ct", "mt")]
hh_rev_gmcr = hh_rev[brand_descr%in%c("GREEN MOUNTAIN KEURIG", "KEURIG"), 
                     .(rev = sum(rev)), by=c("t", "ct", "mt")]
setkey(hh_rev_agg, ct, mt, t)
setkey(hh_rev_gmcr, ct, mt, t)
#---------------------------------------------------------------------------------------------------#
# Plot revenue over time
hh_rev_agg[, Type := ifelse(mt==1, "GMCR Only", ifelse(mt==2, "GMCR+Third Party", "GMCR+Licensed"))]
orev1 = ggplot(hh_rev_agg[.(1), ], aes(x=t, y=rev, group = mt, colour= Type))+
  theme(legend.position=c(0.3,0.8), plot.title = element_text(size=12))+
  geom_line(aes(linetype=Type))+ylim(0, 0.50)+labs(list(title="Original Households", x="Time", y="Revenue"))
orev2 = ggplot(hh_rev_agg[.(2), ], aes(x=t, y=rev, group = mt, colour= Type))+
  theme(legend.position="none", plot.title = element_text(size=12), axis.text.y=element_blank(), 
        axis.ticks=element_blank(), axis.title.y=element_blank())+
  geom_line(aes(linetype=Type))+ylim(0, 0.50) +  labs(list(title="Homogeneous but Variety Seeking", x="Time"))
orev3 = ggplot(hh_rev_agg[.(3), ], aes(x=t, y=rev, group = mt, colour= Type))+
  theme(legend.position="none", plot.title = element_text(size=12), axis.text.y=element_blank(), 
        axis.ticks=element_blank(), axis.title.y=element_blank())+
  geom_line(aes(linetype=Type))+ylim(0, 0.50)+labs(list(title="Heterogeneous but Not Variety Seeking", x="Time"))
multplot = marrangeGrob(list(orev1, orev2, orev3), ncol=3, nrow=1, top="")
ggsave(paste(graph_dir, "/figs/overall_rev.pdf", sep=""), multplot, width=10, height=4)

#---------------------------------------------------------------------------------------------------#
# Plot revenue over GMCR
hh_rev_gmcr[, Type := ifelse(mt==1, "GMCR Only", ifelse(mt==2, "GMCR+Third Party", "GMCR+Licensed"))]
grev1 = ggplot(hh_rev_gmcr[.(1), ], aes(x=t, y=rev, group = mt, colour= Type))+
  theme(legend.position=c(0.3,0.8), plot.title = element_text(size=12))+
  geom_line(aes(linetype=Type))+ylim(0, 0.50)+labs(list(title="Original Households", x="Time", y="Revenue"))
grev2 = ggplot(hh_rev_gmcr[.(2), ], aes(x=t, y=rev, group = mt, colour= Type))+
  theme(legend.position="none", plot.title = element_text(size=12), axis.text.y=element_blank(), 
        axis.ticks=element_blank(), axis.title.y=element_blank())+
  geom_line(aes(linetype=Type))+ylim(0, 0.50) +  labs(list(title="Homogeneous but Variety Seeking", x="Time"))
grev3 = ggplot(hh_rev_gmcr[.(3), ], aes(x=t, y=rev, group = mt, colour= Type))+
  theme(legend.position="none", plot.title = element_text(size=12), axis.text.y=element_blank(), 
        axis.ticks=element_blank(), axis.title.y=element_blank())+
  geom_line(aes(linetype=Type))+ylim(0, 0.50)+labs(list(title="Heterogeneous but Not Variety Seeking", x="Time"))
multplot = marrangeGrob(list(grev1, grev2, grev3), ncol=3, nrow=1, top="")
ggsave(paste(graph_dir, "/figs/gmcr_rev.pdf", sep=""), multplot, width=10, height=4)

