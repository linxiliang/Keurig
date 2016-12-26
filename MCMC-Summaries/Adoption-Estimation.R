#####################################################################################################
#
# MCMC Summaries For the First Stage Estimation
# Xiliang Lin
# March, 2016
#
#####################################################################################################

# Settings
rm(list = ls())

# Packages
library(plot3D)
library(data.table)
setNumericRounding(0)

# Set Working Folder Path Here
setwd("~/Keurig")
input_dir = "Data/Bayes-MCMC/"
graph_dir = "Tabfigs/MCMC-Summaries"

#----------------------------------------------------------------------------------------------------#

# Load Estimation Results
that = fread(paste(input_dir, "Adoption-Coef-NoArrival.csv", sep=""))

# Plot the MCMC Draws
pdf(file=paste(graph_dir, "/figs/Machine-Adoption-Parameters.pdf", sep=""), 
    width=8, height=5)
par(mfrow=c(2,1), mai = c(1.2, 1.5, 0.1, 0.7))
that[, plot(V1, xlab = "", ylab = expression(tau), type = "l")]
that[, plot(V2, xlab="Draw", ylab = expression(vartheta), type = "l")]
par(mfrow=c(1,1))
dev.off()

# Load Estimation Results
pb = read.table(paste(input_dir, "ccp-grid.csv", sep=""), sep = "\t")
pb = as.matrix(pb)

# Create the grid
pgrid = seq(100, 180, 1)
dgrid = seq(0, 6, 0.1)

persp3D(z = pb, x = pgrid, y = dgrid)

surf3D(ccp$price, ccp$delta, ccp$prob)
# Plot the MCMC Draws
pdf(file=paste(graph_dir, "/figs/Machine-Adoption-Parameters.pdf", sep=""), 
    width=8, height=5)
par(mfrow=c(2,1), mai = c(1.2, 1.5, 0.1, 0.7))
that[, plot(V1, xlab = "", ylab = expression(tau), type = "l")]
that[, plot(V2, xlab="Draw", ylab = expression(vartheta), type = "l")]
par(mfrow=c(1,1))
dev.off()

x <- seq(-pi, pi, by = 0.2)
y <- seq(-pi, pi, by = 0.3)
grid <- mesh(x, y)
z <- with(grid, cos(x) * sin(y))
persp3D(z = z, x = x, y = y)
persp3D(z = z, x = x, y = y, facets = FALSE, curtain = TRUE)

x = NULL
for (i in 1:100){
  x = c(x, paste("R", i))
}



dtx = data.table(x=c(1,1,1), y=c(2,2,2))
