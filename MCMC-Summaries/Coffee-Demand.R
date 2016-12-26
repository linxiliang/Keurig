#####################################################################################################
#
# MCMC Summaries For the First Stage Estimation
# Xiliang Lin
# June, 2016
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
input_dir = "Data/Bayes-MCMC/"
graph_dir = "Tabfigs/MCMC-Summaries"

#----------------------------------------------------------------------------------------------------#
# Load Estimation Results
#big_markets = c(501, 602, 803, 504, 539, 506, 524, 613, 623, 753) # Big Markets
big_markets = c(501, 602, 803, 504, 539, 506, 613, 623, 753) 
#big_markets_name = c("New York", "Chicago", "Los Angeles", "Philadelphia", "Tampa - St. Petersburg", 
#                     "Boston", "Atlanta", "Minneapolis - St. Paul", "Dallas - Fort Worth", "Phoenix")
big_markets_name = c("New York", "Chicago", "Los Angeles", "Philadelphia", "Tampa - St. Petersburg", 
                     "Boston", "Minneapolis - St. Paul", "Dallas - Fort Worth", "Phoenix")

brands = c("CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GLORIA JEAN'S KEURIG", "GREEN MOUNTAIN KEURIG",
           "MAXWELL HOUSE", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")
xnames = c("0OTHER", "CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GLORIA JEAN'S KEURIG", "GREEN MOUNTAIN KEURIG",
           "MAXWELL HOUSE", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS",
           "STARBUCKS KEURIG", "TULLY'S KEURIG", "keurig", "brand_lag_ground",
           "brand_lag_keurig", "price_coef", "budget")
alist = paste0("a", 2:24)
nv = 36

x_var = list(n=rep(1,9), mu=matrix(1:27,nrow=9), sig=array(rep(1,3*3*9), dim=c(3,3,9)))
p_table = matrix(rep(0, length(big_markets)*(nv)*3), nrow=length(big_markets))

i = 0
for (mcode in big_markets){
  i = i+1 
  load(paste(input_dir, "/Normal-MCMC-", mcode,".RData", sep=""))
  
  # Thin the draws by 5
  indx = seq(3000, 8000, 5)
  
  # Fill in the 0 if the brand intercept is dropped
  ndiff = length(setdiff(alist, bnames))

  # Obtain the Distribution of State Dependence
  xind = c(34, 35, 24) - ndiff

  # Store Relevant Variables 
  x_var[["mu"]][i, ] = colMeans(bhatd[indx, xind])
  x_var[["sig"]][,,i] = rowMeans(sigd[xind, xind, indx], dims=2)
  x_var[["n"]][i] = length(hh_index_list)
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

