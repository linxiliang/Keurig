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
load(paste(input_dir, "/MDCEV-MCMC-All-90000.RData", sep=""))

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
           "MILLSTONE", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG")
features = c("brand", "keurig", "flavored", "lightR", "medDR", "darkR", "assorted", "kona", "colombian", "sumatra", "wb")
xnames = c(brands, features[2:length(features)], "brand_lag_keurig", "brand_lag", 'ground_alpha', 'keurig_alpha')

nb = length(brands)
np = length(xnames) - 1
nf = length(features)

# Initialize Storage Table
features_low = paste0(features, "_low")
features_high = paste0(features, "_high")
p_table = matrix(rep(0, nb*nf*3), nrow=nb)
rownames(p_table) = brands
colnames(p_table) = c(features, features_low, features_high)

# Summarize preferences
for (b in brands){
  for (f in features){
    fl = paste0(f, "_low")
    fh = paste0(f, "_high")
    if (b=="0OTHER"){
      if (f!="brand"){
        ind_f = which(xnames==f) - 1
        p_table[b, f] = median(bhatd[,ind_f])
        p_table[b, fl] = quantile(bhatd[,ind_f], 0.025)
        p_table[b, fh] = quantile(bhatd[,ind_f], 0.975)
      }
    } else{
      ind_k = which(xnames=="keurig")-1
      ind_b = which(xnames==b)-1
      if (f!="brand"){
        ind_f = which(xnames==f)-1
        if (f=="keurig" | !(grepl("KEURIG", b))){
          p_table[b, f] = median(bhatd[,ind_b]+bhatd[,ind_f])
          p_table[b, fl] = quantile(bhatd[,ind_b]+bhatd[,ind_f], 0.025)
          p_table[b, fh] = quantile(bhatd[,ind_b]+bhatd[,ind_f], 0.975)
        } else{
          ind_f = which(xnames==f) - 1
          p_table[b, f] = median(bhatd[,ind_b]+bhatd[,ind_k]+bhatd[,ind_f])
          p_table[b, fl] = quantile(bhatd[,ind_b]+bhatd[,ind_k]+bhatd[,ind_f], 0.025)
          p_table[b, fh] = quantile(bhatd[,ind_b]+bhatd[,ind_k]+bhatd[,ind_f], 0.975)
        }
      } else{
        ind_b = which(xnames==b)-1
        p_table[b, f] = median(bhatd[,ind_b])
        p_table[b, fl] = quantile(bhatd[,ind_b], 0.025)
        p_table[b, fh] = quantile(bhatd[,ind_b], 0.975)
      }
    }
  }
}
write.csv(p_table, file = paste(graph_dir, "/tabs/mean_post_prefer.csv", sep=""))

#----------------------------------------------------------------------------------------------------#
# Plot the posterior densities and credible intervals of lags
klag_ind = which(xnames=="brand_lag_keurig")-1
glag_ind = which(xnames=="brand_lag")-1

# Create x grid (two grid need to have the same size)
kgrid = seq(-4, 3, 0.01)
ggrid = seq(-2, 5, 0.01)
gsize = length(ggrid)
if (gsize!=length(kgrid)) stop("Keurig Adjustment and Ground Coffee Grid size is not the same")
draws = dim(bhatd)[1]

# Initialize dataset to store percentiles
kd_mat = matrix(rep(0, gsize*draws), nrow=draws)
gd_mat = matrix(rep(0, gsize*draws), nrow=draws)
for (d in 1:draws){
  kd_mat[d, ] = dnorm(kgrid, mean = bhatd[d, klag_ind], sd = sqrt(sigd[klag_ind, klag_ind, d]))
  gd_mat[d, ] = dnorm(ggrid, mean = bhatd[d, glag_ind], sd = sqrt(sigd[glag_ind, glag_ind, d]))
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
dat0 = data.table(grid_points=kgrid, den0=kd_pct[,1], den1=kd_pct[,2], den2=kd_pct[,3], Parameter = "KCup Adjustment")
dat1 = data.table(grid_points=ggrid, den0=gd_pct[,1], den1=gd_pct[,2], den2=gd_pct[,3], Parameter = "Ground")
dat = rbindlist(list(dat0, dat1))
pdf(file=paste(graph_dir, "/figs/StateDependenceDensity.pdf", sep=""), width=8, height=5)
ggplot(dat, aes(x=grid_points, y=den1, group=Parameter, colour=Parameter))+geom_line()+
  geom_ribbon(data=dat,aes(ymin=den0,ymax=den2, fill=Parameter), alpha=0.3)+theme_bw()+
  xlab("Parameter Value") + ylab("Density")+theme(legend.position=c(0.88,0.9))+
  scale_y_continuous(breaks=seq(0, 0.40, 0.05))+scale_x_continuous(breaks=seq(-4, 5, 1))
dev.off()

pdf(file=paste(graph_dir, "/figs/StateDependenceScatter.pdf", sep=""), width=8, height=5)
qplot(bhatd[, klag_ind], bhatd[, glag_ind], xlab = "KCup Adjustment", 
      ylab = "Ground Coffee State Dependence") + theme_bw()
dev.off()

#----------------------------------------------------------------------------------------------------#
# Plot the posterior densities and credible intervals of Satiation Parameters
ksa_ind = which(xnames=="keurig_alpha")-1
gsa_ind = which(xnames=="ground_alpha")-1

# Create x grid (two grid need to have the same size)
xgrid = seq(0, 1, 0.002)
kgrid = xgrid*(0.99999-0.7)+0.7
ggrid = xgrid*(0.99999-0.9)+0.9
gsize = length(ggrid)
if (gsize!=length(kgrid)) stop("KCup and Ground Coffee Grid size is not the same")
draws = dim(bhatd)[1]

# Initialize dataset to store percentiles
kd_mat = matrix(rep(0, gsize*draws), nrow=draws)
gd_mat = matrix(rep(0, gsize*draws), nrow=draws)
for (d in 1:draws){
  kd_mat[d, ] = dnorm(log(kgrid/(1-kgrid)), mean = bhatd[d, ksa_ind], sd = sqrt(sigd[ksa_ind, ksa_ind, d]))
  gd_mat[d, ] = dnorm(log(ggrid/(1-ggrid)), mean = bhatd[d, gsa_ind], sd = sqrt(sigd[gsa_ind, gsa_ind, d]))
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
dat0 = data.table(grid_points=kgrid, den0=kd_pct[,1], den1=kd_pct[,2], den2=kd_pct[,3], Parameter = "KCup")
dat1 = data.table(grid_points=ggrid, den0=gd_pct[,1], den1=gd_pct[,2], den2=gd_pct[,3], Parameter = "Ground")
dat = rbindlist(list(dat0, dat1))
pdf(file=paste(graph_dir, "/figs/SatiationDensity.pdf", sep=""), width=8, height=5)
ggplot(dat, aes(x=grid_points, y=den1, group=Parameter, colour=Parameter))+geom_line()+
  geom_ribbon(data=dat,aes(ymin=den0,ymax=den2, fill=Parameter), alpha=0.3)+theme_bw()+
  xlab("Parameter Value") + ylab("Density")+theme(legend.position=c(0.1,0.9))+
  scale_y_continuous(breaks=seq(0, 50, 5))+scale_x_continuous(breaks=seq(0.7, 1, 0.05))
dev.off()

pdf(file=paste(graph_dir, "/figs/SatiationScatter.pdf", sep=""), width=8, height=5)
qplot(exp(bhatd[, ksa_ind])/(1+exp(bhatd[, ksa_ind])), 
      exp(bhatd[, gsa_ind])/(1+exp(bhatd[, gsa_ind])), xlab = "KCup", 
      ylab = "Ground Coffee State Dependence") + theme_bw()
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
  y_low  = min(bhatd[, xind])-0.5
  y_high = max(bhatd[, xind])+0.5
  j = ifelse(i%%ngraph==0, ngraph, i%%ngraph)
  gph[[j]] = ggplot(dat, aes(x=Draws, y=V1))+geom_line(color="skyblue", size=0.2)+theme_bw()+ 
    ylim(y_low, y_high)+theme(axis.title=element_text(size=8))+ylab(xl)
  if (i%%ngraph==0 | i==length(xlabels)){
    k = k+1
    multplot = marrangeGrob(gph, ncol=nc, nrow=nr, top="")
    ggsave(paste(graph_dir, "/figs/TracePlot", k, ".pdf", sep=""), multplot, width=7, height=9)
    gph = as.list(NULL)
  }
}



