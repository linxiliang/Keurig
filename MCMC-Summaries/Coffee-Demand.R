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

# Plot the MCMC Draws by Brands and Flavors


# Storage estimates by market
x_var = list(n=rep(1, nm), mu=matrix(1:(np*nm), nrow=nm), sig=array(rep(1,np*np*nm), dim=c(np, np, nm)))
p_table = matrix(rep(0, 3*np*nm), nrow=length(big_markets))


# Create posterior 95% coverage region of different brands
p_table = data.table(market = big_markets_name, dma_code = big_markets, p_table)

# Summarize 
# Obtain the posterior mean by Market
i = 0
for (mcode in big_markets){
  i = i+1 
  hh_temp = hh[dma_code==mcode, household_code]
  hh_selected = sort(hh_code_list[household_code %in% hh_temp, hh])
  
  # Store Relevant Variables
  btemp = bindv[,hh_selected,]
  x_var[["mu"]][i, ] =  colMeans(apply(btemp, c(1, 3), sum)/length(hh_selected))
  cx = dim(btemp)
  sigx = matrix(rep(0, cx[3]*cx[3]), nrow=cx[3])
  for (j in 1:cx[1]){
    sigx = sigx + crossprod(btemp[j,,])/length(hh_selected)
  }
  x_var[["sig"]][,,i] = sigx/cx[1]
  x_var[["n"]][i] = length(hh_selected)
}

# Simulate and draw the bivariate distribution
# Construct draws
xgrid = seq(-6, 6, 0.001)
x_var[["n"]] = (x_var[["n"]])/sum(x_var[["n"]])
dtab1 = matrix(rep(0, 9*length(xgrid)), ncol=9)
dtab2 = matrix(rep(0, 9*length(xgrid)), ncol=9)
for (i in 1:9){
  print(i)
  dtab1[,i] = dnorm(xgrid, mean = x_var[["mu"]][i, 1], sd = sqrt(x_var[["sig"]][1,1,i]))
  dtab2[,i] = dnorm(xgrid, mean = x_var[["mu"]][i, 2], sd = sqrt(x_var[["sig"]][2,2,i]))
}
yden1 = dtab1%*%x_var[["n"]]
yden2 = dtab2%*%x_var[["n"]]

pdf(file=paste(graph_dir, "/figs/StateDependence.pdf", sep=""), width=8, height=5)
plot(xgrid, yden2, type = "l", ylab = "Density",
     xlab = "State Dependence", lty = 2, col = "red")
lines(xgrid, yden1)
legend("topright", c("Gound Coffee", "K-Cup Adjustment"),
       lty=c(2,1), col=c("red","black"))
dev.off()

xgrid = seq(-4, 12, 0.001)
dtab = matrix(rep(0, 9*length(xgrid)), ncol=9)
for (i in 1:9){
  dtab[,i] = dnorm(xgrid, mean = x_var[["mu"]][i, 3], sd = sqrt(x_var[["sig"]][3,3,i]))
}
yden = dtab%*%x_var[["n"]]

pdf(file=paste(graph_dir, "/figs/KeurigP.pdf", sep=""), width=8, height=5)
plot(xgrid, yden, type = "l", ylab = "Density", xlim=c(-4, 12),
     xlab = "K-Cup Preference", lty = 1, col = "black")
dev.off()


scatter(drawd[,1], drawd[,2])
smoothScatter(drawd[,1], drawd[,2], bandwidth=0.1)

p_table = data.table(market = big_markets_name, dma_code = big_markets, p_table)
write.csv(p_table, file = paste(graph_dir, "/tabs/mean_post_prefer.csv", sep=""), row.names = F)

if (ndiff!=0){
  m_list = setdiff(alist, bnames)
  for (k in 1:length(m_list)) {
    numk = as.integer(substr(m_list[k], 2, nchar(m_list[k])))-2
    bhatd = cbind(bhatd[, 1:numk], rep(0,nrow(bhatd)), 
                  bhatd[, (numk+1):ncol(bhatd)])
  }
}


mu_state = colMeans(bhatd[indx, 34:35])
sig_state = rowMeans(sigd[34:35, 34:35, indx], dims=2)
xgrid = seq(-6, 6, 0.01)
g_den = dnorm(xgrid, mu_state[1], sd = sqrt(sig_state[1,1]))
k_den = dnorm(xgrid, mu_state[2], sd = sqrt(sig_state[2,2]))
pdf(file=paste(graph_dir, "/figs/DMA-", mcode, "-StateDepend-By-Type.pdf", sep=""), width=8, height=5)
plot(xgrid, k_den, type = "l", ylab = "Density",
     xlab = "State Dependence", lty = 2, col = "red")
lines(xgrid, g_den)
legend("topright", c("K-Cups", "Gound Coffee"),
       lty=c(2,1), col=c("red","black"))
dev.off()

j = 0 
for (xn in xnames){
  if (j!=0){
    if (grepl("KEURIG", xn)){
      p_table[i,j] = median(bhatd[indx, j] + bhatd[indx, 24])
      p_table[i,(j+nv)] = quantile(bhatd[indx, j] + bhatd[indx, 24], 0.05) 
      p_table[i,(j+2*nv)] = quantile(bhatd[indx, j] + bhatd[indx, 24], 0.95) 
    } else{
      p_table[i,j] = median(bhatd[indx, j])
      p_table[i,(j+nv)] = quantile(bhatd[indx, j], 0.05) 
      p_table[i,(j+2*nv)] = quantile(bhatd[indx, j], 0.95) 
    }
  }
  j = j+1
}

