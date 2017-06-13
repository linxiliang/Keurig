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
load(paste(input_dir, "/MDCEV-MCMC-OX-All2.RData", sep=""))
bindv[,,29:31] = exp(bindv[,,29:31])/(1+exp(bindv[,,29:31]))

# Burnin
burnin = 401
d_ind = c((burnin+1):dim(bindv)[1])

brands=c("0OTHER", "CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", 
         "DUNKIN' DONUTS", "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GREEN MOUNTAIN KEURIG", 
         "MAXWELL HOUSE", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")
features=c("brand", "keurig", "flavored", "lightR", "medDR", "darkR", 
           "assorted", "kona", "colombian", "sumatra", "wb")
xnames = c(brands, features[2:length(features)], "brand_lag_keurig", "brand_lag", 'nbrand', 
           'ground_alpha', 'keurig_alpha', 'rho')
k_ind=which(xnames=="keurig")
ind_ga= which(xnames=="ground_alpha")
ind_ka = which(xnames=="keurig_alpha")
ind_rho = which(xnames=="rho")
for (b in brands){
  ind_b = which(xnames==b)
  if (grepl("KEURIG", b)){
    bindv[,,ind_b] = bindv[,,ind_b] + bindv[,,k_ind]
  } 
}

nb = length(brands)
nh = dim(bindv)[2]
ndraws = (dim(bindv)[1] - burnin)
r2_tab = matrix(rep(0, ndraws*4), ncol=4)

b_ind_list = NULL
for (br in brands){
  if (grepl("KEURIG", br)){
    b_ind = which(brands==br)
    b_ind_list = c(b_ind_list, b_ind)
  }
}

# Household level variance analysis
hh_var_tab = data.table(iter=rep(1:ndraws, nh*5))
setkey(hh_var_tab, iter)
hh_var_tab[, value:=as.numeric(NA)]

# Construct table 
for (d in 1:ndraws){
  indv_dt = bindv[(d+burnin),,]
  gc()

  # Modify bindv so that Keurig brands have the values built in
  # With levels
  # for (b_ind in 1:nb){
  #   if (b_ind %in% b_ind_list){
  #     indv_dt[,b_ind] = 0.1294638*(exp(indv_dt[,b_ind]) * (2^indv_dt[,ind_ka]-1))^indv_dt[,ind_rho]
  #   } else{
  #     indv_dt[,b_ind] = 0.1294638*(exp(indv_dt[,b_ind]) * (2^indv_dt[,ind_ga]-1))^indv_dt[,ind_rho]
  #   }
  # }
  # Relative
  for (b_ind in 2:nb) indv_dt[,b_ind] = indv_dt[,b_ind] - indv_dt[,1]
  indv_dt[,1] = 0
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
  hh_v1 = indv_dt[keurig==0, var(value), by = "hh"][,V1]
  hh_v2 = indv_dt[keurig==1, var(value), by = "hh"][,V1]
  hh_v3 = indv_dt[, var(value), by = "hh"][,V1]
  indv_dt[, hh_mean_v := mean(value), by = "hh"]
  hh_v4 = indv_dt[keurig==0, sum((mean(value) - hh_mean_v)^2), by = "hh"][,V1]
  hh_v5 = indv_dt[keurig==1, sum((mean(value) - hh_mean_v)^2), by = "hh"][,V1]
  
  hh_var_tab[.(d), value:=c(hh_v1, hh_v2, hh_v3, hh_v4, hh_v5)]
  
  cat("Draw", d, "Finished out of", ndraws, ".\n\n")
}
rm(bindv)
gc()
hh_var_tab[, `:=`(hh=rep(1:nh, ndraws*5), vtype=rep(sort(rep(1:5, nh)), ndraws))]
save(hh_var_tab, r2_tab, file = "~/Desktop/HH-Var-Decomposition.RData")
#----------------------------------------------------------------------------------------------------#
# Make the corresponding graphs
# R-squared distribution
r2_tab = data.table(r2_tab)
setnames(r2_tab, c("V1", "V2", "V3", "V4"), c("Keurig", "Brand", "Household", "BrandHousehold"))
g1 = ggplot(r2_tab, aes(x=Keurig))+theme_bw()+labs(list(title="K-Cup", x="R-Squared", y="Density"))+
  geom_histogram(bins = 20, aes(y = ..density..), fill = "skyblue", colour="black")+
  geom_vline(xintercept = quantile(r2_tab[, Keurig], c(0.025, 0.50, 0.975)))
g2 = ggplot(r2_tab, aes(x=Brand))+theme_bw()+labs(list(title="Brand", x="R-Squared", y="Density"))+
  geom_histogram(bins = 20, aes(y = ..density..), fill = "skyblue", colour="black")+
  geom_vline(xintercept = quantile(r2_tab[, Brand], c(0.025, 0.50, 0.975)))
multplot = marrangeGrob(list(g1, g2), ncol=2, nrow=1, top="")
ggsave(paste(graph_dir, "/figs/R2-Draws.pdf", sep=""), multplot, width=7, height=3.5)


# Make the corresponding graphs
hh_var_tab[vtype==4, value:=value/7]
hh_var_tab[vtype==5, value:=value/6]
setkey(hh_var_tab, iter, hh, vtype)
hh_var_tab = hh_var_tab[vtype %in% c(1, 2), ]
hh_var_tab = hh_var_tab[, .(value=mean(value)), by = c("hh", "vtype")]
g1 = ggplot(hh_var_tab[vtype==1, ], aes(x=value))+theme_bw()+labs(list(title="Ground", x="Variance", y="Density"))+
  geom_histogram(bins = 40, aes(y = ..density..), fill = "skyblue", colour="black")+ xlim(0,15) +
  geom_vline(xintercept = quantile(hh_var_tab[vtype==1, value], c(0.025, 0.50, 0.975)))
g2 = ggplot(hh_var_tab[vtype==2, ], aes(x=value))+theme_bw()+labs(list(title="K-Cup", x="Variance", y="Density"))+
  geom_histogram(bins = 40, aes(y = ..density..), fill = "skyblue", colour="black")+ xlim(0,15) +
  geom_vline(xintercept = quantile(hh_var_tab[vtype==2, value], c(0.025, 0.50, 0.975)))
multplot = marrangeGrob(list(g1, g2), ncol=2, nrow=1, top="")
ggsave(paste(graph_dir, "/figs/Var-Analysis.pdf", sep=""), multplot, width=7, height=3.5)
#----------------------------------------------------------------------------------------------------#