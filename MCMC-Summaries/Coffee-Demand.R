#####################################################################################################
#
# MCMC Summaries For the First Stage Estimation
# Xiliang Lin
# Jan, 2017
# First Version: March 2016
# 
#####################################################################################################

# Settings
rm(list = ls())
gc()

# Packages
library(data.table)
library(ggplot2)
library(ggmcmc)
library(grid)
library(gridExtra)
setNumericRounding(0)

# Set Working Folder Path Here
setwd("~/Keurig")
meta_dir = "Data/Meta-Data/"
input_dir = "Data/Bayes-MCMC/"
graph_dir = "Tabfigs/MCMC-Summaries"
load(paste(meta_dir, "HH.RData", sep=""))
#----------------------------------------------------------------------------------------------------#
# Load Estimation Results
load(paste(input_dir, "/MDCEV-MCMC-OX-All.RData", sep=""))

# Burnin
burnin = 6400
# d_ind = c((burnin+1):dim(bhatd)[1])
d_ind = c((burnin+1):7000)

big_markets = c(501, 506, 504, 602, 803, 511, 539, 623, 618, 505, 
                613, 819, 524, 534, 533, 753, 510, 508, 514, 512, 
                517, 807, 751, 862, 535, 521, 548, 609, 566, 641)
nm = length(big_markets)
big_markets_name = rep("a", nm)
i = 0
for (mcode in big_markets){
  i = i+1
  big_markets_name[i] = hh[dma_code==mcode, dma_descr][1]
}

brands = c("0OTHER", "CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GREEN MOUNTAIN KEURIG", "MAXWELL HOUSE", 
           "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")
features = c("brand", "keurig", "flavored", "lightR", "medDR", "darkR", "assorted", "kona", "colombian", "sumatra", "wb")
xnames = c(brands, features[2:length(features)], "brand_lag_keurig", "brand_lag", 'nbrand', 'ground_alpha', 'keurig_alpha', 'rho')

nb = length(brands)
np = length(xnames) 
nf = length(features)

# Initialize Storage Table
features_low = paste0(features, "_low")
features_high = paste0(features, "_high")
p_table = matrix(rep(0, nb*nf*3), nrow=nb)
rownames(p_table) = brands
colnames(p_table) = c(features, features_low, features_high)
ind_k = which(xnames=="keurig")
ind_ga= which(xnames=="ground_alpha")
ind_ka = which(xnames=="keurig_alpha")
ind_rho = which(xnames=="rho")
a_ground = exp(bhatd[d_ind,ind_ga])/(1+exp(bhatd[d_ind,ind_ga]))
a_keurig = exp(bhatd[d_ind,ind_ka])/(1+exp(bhatd[d_ind,ind_ka]))
rhov = exp(bhatd[d_ind,ind_rho])/(1+exp(bhatd[d_ind,ind_rho]))
# 0.1294638 is the average purchase probability.

for (b in brands){
  ind_b = which(xnames==b)
  if (grepl("KEURIG", b)){
    bhatd[,ind_b] = bhatd[,ind_b] + bhatd[, ind_k]
  } 
}

# Summarize preferences
for (b in brands){
  for (f in features){
    fl = paste0(f, "_low")
    fh = paste0(f, "_high")
    ind_b = which(xnames==b)
    if (f=="brand"){
      xval = bhatd[d_ind,ind_b]
      if (grepl("KEURIG", b)){
        yval = (exp(xval) * ((1+1)^a_keurig-1))^rhov
        p_table[b, "keurig"] = median(yval) * 0.1294638
        p_table[b, "keurig_low"] = quantile(yval, 0.025) * 0.1294638
        p_table[b, "keurig_high"] = quantile(yval, 0.975) * 0.1294638
      } else{
        yval = (exp(xval) * ((1+1)^a_ground-1))^rhov
        p_table[b, f] = median(bhatd[d_ind,ind_b]) * 0.1294638
        p_table[b, fl] = quantile(bhatd[d_ind,ind_b], 0.025) * 0.1294638
        p_table[b, fh] = quantile(bhatd[d_ind,ind_b], 0.975) * 0.1294638
      }
    } else{
      ind_f = which(xnames==f)
      xval = bhatd[d_ind, ind_b]+bhatd[d_ind,ind_f]
      if (f!="keurig"){
        if (grepl("KEURIG", b)){
          yval = (exp(xval) * ((1+1)^a_keurig-1))^rhov
        } else{
          yval = (exp(xval) * ((1+1)^a_ground-1))^rhov
        }
        p_table[b, f] = median(yval) * 0.1294638
        p_table[b, fl] = quantile(yval, 0.025) * 0.1294638
        p_table[b, fh] = quantile(yval, 0.975) * 0.1294638
      }
    }
  }
}
write.csv(p_table, file = paste(graph_dir, "/tabs/mean_post_prefer.csv", sep=""))

#----------------------------------------------------------------------------------------------------#
# Plot the posterior densities and credible intervals of lags
klag_ind = which(xnames=="brand_lag_keurig")
glag_ind = which(xnames=="brand_lag")

# Create x grid (two grid need to have the same size)
kgrid = seq(-4, 3, 0.01)
ggrid = seq(-2, 5, 0.01)
gsize = length(ggrid)
if (gsize!=length(kgrid)) stop("Keurig Adjustment and Ground Coffee Grid size is not the same")
# draws = dim(bhatd)[1] - burnin
draws = 7000 - burnin

# Initialize dataset to store percentiles
kd_mat = matrix(rep(0, gsize*draws), nrow=draws)
gd_mat = matrix(rep(0, gsize*draws), nrow=draws)
for (d in 1:draws){
  kd_mat[d, ] = dnorm(kgrid, mean = bhatd[(d+burnin), klag_ind], 
                      sd = chol(sigd[,,(d+burnin)])[klag_ind, klag_ind])
  gd_mat[d, ] = dnorm(ggrid, mean = bhatd[(d+burnin), glag_ind], 
                      sd = chol(sigd[,,(d+burnin)])[glag_ind, glag_ind])
}

# Obtain the percentiles in grid 
kd_pct = matrix(rep(0, gsize*3), nrow=gsize)
gd_pct = matrix(rep(0, gsize*3), nrow=gsize)
for (g in 1:gsize){
  kd_pct[g, ] = quantile(kd_mat[,g], c(0.025, 0.50, 0.975))
  gd_pct[g, ] = quantile(gd_mat[,g], c(0.025, 0.50, 0.975))
}
rm(kd_mat, gd_mat)
gc()

# Create the dataset for graphs
dat0 = data.table(grid_points=kgrid, den0=kd_pct[,1], den1=kd_pct[,2], 
                  den2=kd_pct[,3], Parameter = "KCup Adjustment")
dat1 = data.table(grid_points=ggrid, den0=gd_pct[,1], den1=gd_pct[,2], 
                  den2=gd_pct[,3], Parameter = "Ground")
dat = rbindlist(list(dat0, dat1))
pdf(file=paste(graph_dir, "/figs/StateDependenceDensity.pdf", sep=""), width=8, height=5)
ggplot(dat, aes(x=grid_points, y=den1, group=Parameter, colour=Parameter))+geom_line()+
  geom_ribbon(data=dat,aes(ymin=den0,ymax=den2, fill=Parameter), alpha=0.3)+theme_bw()+
  xlab("Parameter Value") + ylab("Density")+theme(legend.position=c(0.88,0.9))+
  scale_y_continuous(breaks=seq(0, 0.50, 0.05))+scale_x_continuous(breaks=seq(-4, 5, 1))
dev.off()

pdf(file=paste(graph_dir, "/figs/StateDependenceScatter.pdf", sep=""), width=8, height=5)
qplot(bhatd[d_ind, klag_ind], bhatd[d_ind, glag_ind], xlab = "KCup Adjustment", 
      ylab = "Ground Coffee State Dependence") + theme_bw()
dev.off()

#----------------------------------------------------------------------------------------------------#
# Plot the posterior densities and credible intervals of Satiation Parameters
ksa_ind = which(xnames=="keurig_alpha")
gsa_ind = which(xnames=="ground_alpha")

# Create x grid (two grid need to have the same size)
xgrid = seq(0, 1, 0.002)
kgrid = xgrid*(0.99999-0.8)+0.8
ggrid = xgrid*(0.99999-0.9)+0.9
gsize = length(ggrid)
if (gsize!=length(kgrid)) stop("KCup and Ground Coffee Grid size is not the same")

# Initialize dataset to store percentiles
kd_mat = matrix(rep(0, gsize*draws), nrow=draws)
gd_mat = matrix(rep(0, gsize*draws), nrow=draws)
for (d in 1:draws){
  kd_mat[d, ] = dnorm(log(kgrid/(1-kgrid)), mean = bhatd[(d+burnin), ksa_ind], 
                      sd = chol(sigd[,,(d+burnin)])[ksa_ind, ksa_ind])
  gd_mat[d, ] = dnorm(log(ggrid/(1-ggrid)), mean = bhatd[(d+burnin), gsa_ind], 
                      sd = chol(sigd[,,(d+burnin)])[gsa_ind, gsa_ind])
  kd_mat[d, ] = kd_mat[d, ] * 1/(kgrid*(1-kgrid))
  gd_mat[d, ] = gd_mat[d, ] * 1/(ggrid*(1-ggrid))
}

# Obtain the percentiles in grid 
kd_pct = matrix(rep(0, gsize*3), nrow=gsize)
gd_pct = matrix(rep(0, gsize*3), nrow=gsize)
for (g in 1:gsize){
  kd_pct[g, ] = quantile(kd_mat[,g], c(0.025, 0.50, 0.975))
  gd_pct[g, ] = quantile(gd_mat[,g], c(0.025, 0.50, 0.975))
}
rm(kd_mat, gd_mat)
gc()

# Create the dataset for graphs
dat0 = data.table(grid_points=kgrid, den0=kd_pct[,1], den1=kd_pct[,2], 
                  den2=kd_pct[,3], Parameter = "KCup")
dat1 = data.table(grid_points=ggrid, den0=gd_pct[,1], den1=gd_pct[,2], 
                  den2=gd_pct[,3], Parameter = "Ground")
dat = rbindlist(list(dat0, dat1))
pdf(file=paste(graph_dir, "/figs/SatiationDensity.pdf", sep=""), width=8, height=5)
ggplot(dat, aes(x=grid_points, y=den1, group=Parameter, colour=Parameter))+geom_line()+
  geom_ribbon(data=dat,aes(ymin=den0,ymax=den2, fill=Parameter), alpha=0.3)+theme_bw()+
  xlab("Parameter Value") + ylab("Density")+theme(legend.position=c(0.1,0.9))+
  scale_y_continuous(breaks=seq(0, 80, 10))+scale_x_continuous(breaks=seq(0.8, 1, 0.05))
dev.off()

pdf(file=paste(graph_dir, "/figs/SatiationScatter.pdf", sep=""), width=8, height=5)
qplot(exp(bhatd[d_ind, ksa_ind])/(1+exp(bhatd[d_ind, ksa_ind])), 
      exp(bhatd[d_ind, gsa_ind])/(1+exp(bhatd[d_ind, gsa_ind])), xlab = "KCup", 
      ylab = "Ground Coffee State Dependence") + theme_bw()
dev.off()

#----------------------------------------------------------------------------------------------------#
# Plot the posterior densities and credible intervals of lags
kind = which(xnames=="keurig")

# Create x grid (two grid need to have the same size)
kgrid = seq(-4, 10, 0.02)
ksize = length(kgrid)

# Initialize dataset to store percentiles
kd_mat = matrix(rep(0, ksize*draws), nrow=draws)
for (d in 1:draws){
  kd_mat[d, ] = dnorm(kgrid, mean = bhatd[(d+burnin), kind], 
                      sd = chol(sigd[,,(d+burnin)])[kind, kind])
}

# Obtain the percentiles in grid 
k_pct = matrix(rep(0, ksize*3), nrow=ksize)
for (k in 1:ksize){
  k_pct[k, ] = quantile(kd_mat[,k], c(0.025, 0.50, 0.975))
}
rm(kd_mat)
gc()

# Create the dataset for graphs
dat = data.table(grid_points=kgrid, den0=k_pct[,1], den1=k_pct[,2], 
                  den2=k_pct[,3], Parameter = "Keurig")
pdf(file=paste(graph_dir, "/figs/KeurigP.pdf", sep=""), width=8, height=5)
ggplot(dat, aes(x=grid_points, y=den1, group=Parameter, colour=Parameter))+geom_line()+
  geom_ribbon(data=dat,aes(ymin=den0,ymax=den2, fill=Parameter), alpha=0.3)+theme_bw()+
  xlab("Parameter Value") + ylab("Density")+theme(legend.position=c(0.88,0.9))+geom_vline(xintercept = 0)+
  scale_y_continuous(breaks=seq(0, 0.60, 0.10))+scale_x_continuous(breaks=seq(-4, 10, 2))
dev.off()

#----------------------------------------------------------------------------------------------------#
load("Data/Machine-Adoption/HH-Util-Diff.RData")
pdf(file=paste(graph_dir, "/figs/mean-util-gain.pdf", sep=""), width=7, height=5)
mean_gain[, plot(week_end, delta, type = "o", pch = 21, lwd = 0.4, bg = "skyblue1",
                 xlab = "Week", ylab = "Mean Utility Gain")]
axis(side = 2, at = seq(from = 0, to = 1.2, by = 0.1))
dev.off()
#----------------------------------------------------------------------------------------------------#
# Plot MCMC Draws to show stability
xlabels=c("BRAND: CARIBOU KEURIG", "BRAND: CHOCK FULL O NUTS", "BRAND: PRIVATE LABEL", "BRAND: DONUT HOUSE KEURIG", 
          "BRAND: DUNKIN' DONUTS", "BRAND: EIGHT O'CLOCK", "BRAND: FOLGERS", "BRAND: FOLGERS KEURIG",
          "BRAND: GREEN MOUNTAIN KEURIG", "BRAND: MAXWELL HOUSE", "BRAND: MILLSTONE", 
          "BRAND: NEWMAN'S OWN ORGANICS KEURIG", "BRAND: STARBUCKS", "BRAND: STARBUCKS KEURIG", 
          "KEURIG", "FLAVORED COFFEE", "LIGHT ROAST", "MEDIUM DARK ROAST", "DARK ROAST",
          "ASSORTED FLAVORS", "KONA COFFEE", "COLOMBIAN COFFEE", "SUMATRA COFFEE", "WHOLEBEAN COFFEE", 
          "STATE DEPENDENCE: KCUP ADJUSTMENT", "STATE DEPENDENCE", 
          "TRANSFORMED ALPHA: GROUND", "TRANSFORMED ALPHA: KCUP")

nc = 2
nr = 4
ngraph = nc*nr
i = 0
k = 0
gph = as.list(NULL)
for (xl in xlabels){
  i = i+1
  xind = which(xlabels==xl)
  dat = data.table(bhatd[, xind])
  dat[, Draws := 1:.N]
  #y_low  = min(bhatd[, xind])-0.5
  #y_high = max(bhatd[, xind])+0.5
  j = ifelse(i%%ngraph==0, ngraph, i%%ngraph)
  gph[[j]] = ggplot(dat, aes(x=Draws, y=V1))+geom_line(color="skyblue", size=0.2)+theme_bw()+ 
    theme(axis.title=element_text(size=8))+ylab(xl)
  if (i%%ngraph==0 | i==length(xlabels)){
    k = k+1
    multplot = marrangeGrob(gph, ncol=nc, nrow=nr, top="")
    ggsave(paste(graph_dir, "/figs/TracePlot", k, ".pdf", sep=""), multplot, width=7, height=9)
    gph = as.list(NULL)
  }
}



