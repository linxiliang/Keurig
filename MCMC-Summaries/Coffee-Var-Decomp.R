#####################################################################################################
#
# Variance Decomposition of Preferences of Brands
# Xiliang Lin
# Jan, 2017
# 
#####################################################################################################

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
graph_dir = "Tabfigs/MCMC-Summaries"
code_dir = "Scripts/MCMC-Summaries"
#----------------------------------------------------------------------------------------------------#
# Load Estimation Results
load(paste(input_dir, "/MDCEV-MCMC-All-90000.RData", sep=""))

brands = c("0OTHER", "CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GREEN MOUNTAIN KEURIG", "MAXWELL HOUSE", 
           "MILLSTONE", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG")
features = c("brand", "keurig", "flavored", "lightR", "medDR", "darkR", "assorted", "kona", "colombian", "sumatra", "wb")
xnames = c(brands, features[2:length(features)], "brand_lag_keurig", "brand_lag", 'ground_alpha', 'keurig_alpha')
k_ind = which(xnames=="keurig")-1

nb = length(brands)
nh = dim(bindv)[2]
ndraws = dim(bindv)[1]
r2_tab = matrix(rep(0, ndraws*4), ncol=4)

b_ind_list = NULL
for (br in brands){
  if (grepl("KEURIG", br)){
    b_ind = which(brands==br)-1
    b_ind_list = c(b_ind_list, b_ind)
  }
}

# Household level variance analysis
hh_var_tab = data.table(iter=rep(1:ndraws, nh*5))
setkey(hh_var_tab, iter)
hh_var_tab[, value:=as.numeric(NA)]

# Construct table 
for (d in 1:ndraws){
  indv_dt = bindv[d,,]
  gc()

  # Modify bindv so that Keurig brands have the values built in
  for (b_ind in b_ind_list) indv_dt[,b_ind] = indv_dt[,b_ind] + indv_dt[,k_ind]
  
  indv_dt = indv_dt[, 1:nb]
  indv_dt = data.table(value = as.vector(t(indv_dt)))
  indv_dt[, hh := sort(rep(1:nh, nb))]
  indv_dt[, brand := rep(1:nb, nh)]
  indv_dt[, keurig := as.integer(brand%in%c(b_ind_list, k_ind))]
  indv_dt[, hh_mean:= mean(value), by = "hh"]
  indv_dt[, br_mean:= mean(value), by = "brand"]
  indv_dt[, tp_mean:= mean(value), by = "keurig"]
  indv_dt[, all_mean:= mean(value-hh_mean), by = "brand"] # Only if panel is balanced.
  indv_dt[, resid_all_mean:= all_mean - mean(all_mean), by = "hh"] # Only if panel is balanced.
  
  # Obtain the R squares
  r2_tab[d, 1] = indv_dt[, 1 - sum((value-tp_mean)^2)/sum((value-mean(value))^2)]
  r2_tab[d, 2] = indv_dt[, 1 - sum((value-br_mean)^2)/sum((value-mean(value))^2)]
  r2_tab[d, 3] = indv_dt[, 1 - sum((value-hh_mean)^2)/sum((value-mean(value))^2)]
  r2_tab[d, 4] = indv_dt[, 1 - sum((resid_all_mean)^2)/sum((value-mean(value))^2)]
  
  # HH level variance analysis
  hh_v1 = indv_dt[keurig==0, sum((value - mean(value))^2), by = "hh"][,V1]
  hh_v2 = indv_dt[keurig==1&brand!=nb, sum((value - mean(value))^2), by = "hh"][,V1]
  hh_v3 = indv_dt[brand!=nb, sum((value - mean(value))^2), by = "hh"][,V1]
  indv_dt[brand!=nb, hh_mean_v := mean(value), by = "hh"]
  hh_v4 = indv_dt[keurig==0, sum((mean(value) - hh_mean_v)^2), by = "hh"][,V1]
  hh_v5 = indv_dt[keurig==1&brand!=nb, sum((mean(value) - hh_mean_v)^2), by = "hh"][,V1]
  
  hh_var_tab[.(d), value:=c(hh_v1, hh_v2, hh_v3, hh_v4, hh_v5)]
  
  cat("Draw", d, "Finished out of", ndraws, ".\n\n")
}
rm(bindv)
gc()
hh_var_tab[, `:=`(hh=rep(1:nh, ndraws*4), vtype=rep(sort(rep(1:5, nh)), ndraws))]

#----------------------------------------------------------------------------------------------------#

# Make the corresponding graphs


#----------------------------------------------------------------------------------------------------#