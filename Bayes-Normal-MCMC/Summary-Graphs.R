##########################################################################################################
#
# This script takes the MCMC results and generate summaries
# Xiliang Lin
# Dec, 2015
# 
##########################################################################################################

# Settings
rm(list = ls())               # Clear workspace
run_on_Linux        = TRUE    # Determines the path settings

#--------------------------------------------------------------------------------------------------------#
# Load Required packages
require(data.table)
setNumericRounding(0)
require(bayesm)

#---------------------------------------------------------------------------------------------------------#
# Set working directory and corresponding dir 
if (run_on_Linux){
  setwd("~/Keurig")
} else{
  setwd("D:/Cygwin64/home/xlin0/Keurig")
}

code_dir = "Scripts/Bayes-Normal-MCMC/"
input_dir = "Data/MLogit-Data/"
output_dir = "Data/Bayes-MCMC/"
fig_dir = "Tabfigs/Bayes-MCMC/figs/"
tab_dir = "Tabfigs/Bayes-MCMC/tabs/"

#---------------------------------------------------------------------------------------------------------#
# Load MCMC draws
load(paste(output_dir, "Normal-MCMC-64cores.RData", sep = ""))

# Load the original estimation data for brand identification
load(paste(input_dir, "Keurig_julia.RData", sep = ""))

#---------------------------------------------------------------------------------------------------------#

# Identify the brand names and convert the mcmc draws to brand names
brand_num = c(3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15, 17)
brand_names = unique(hh_rms_panel[, .(brand, brand_descr)])
setkey(brand_names, brand)
xnames = c(brand_names[.(brand_num), brand_descr], "Price", "Pack Size", "Brand Lag", "Cumulative Brand Purchased")

bhatd = data.table(bhatd)
orig_names = paste0("V", 1:length(xnames))
setnames(bhatd, orig_names, xnames)

# Plot price sensitivity 
bhatd[, plot(Price, type = "l")]
plot(bhatd$"GREEN MOUNTAIN COFFEE KEURIG", type = "l")

# Seems that the draws didn't reach convergence until about 1000 draws which need 1000*20 + 1000 = 21000
bhatd = bhatd[1000:(nrow(bhatd)),]

# Now I want four plots for each variable - regular plot showing mixing, distribution histogram, 
# acf and kernal density plot
lnames = c("Price", "Pack Size", "Brand Lag", "Cumulative Brand Purchased", brand_names[.(brand_num), brand_descr])
for (v in lnames){
  setnames(bhatd, v, "vx")
  pdf(file = paste(fig_dir, gsub(" ","-",v), ".pdf", sep=""), width = 9, height = 6)
  par(mfrow=c(2,2))
  print(bhatd[, {plot(vx, type="l", ylab="")
                 hist(vx, main="", xlab=v)
                 acf(vx, main="")
                 plot(density(vx), main="")
                 title(v, line = -2, outer = TRUE)}])
  par(mfrow=c(1,1))
  dev.off()
  setnames(bhatd, "vx", v)
}

pdf(file = paste(fig_dir, "MCMC-Beta-Draws.pdf", sep=""), width = 9, height = 6)
par(mfrow=c(2,2))
for (v in lnames){
  setnames(bhatd, v, "vx")
  print(bhatd[, {plot(vx, type="l", ylab="")
    hist(vx, main="", xlab=v)
    acf(vx, main="")
    plot(density(vx), main="")
    title(v, line = -2, outer = TRUE)}])
  setnames(bhatd, "vx", v)
}
par(mfrow=c(1,1))
dev.off()


#---------------------------------------------------------------------------------------------------------# 

