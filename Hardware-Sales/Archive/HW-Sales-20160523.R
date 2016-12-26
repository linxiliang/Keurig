#####################################################################################################
#
# Hardware Purchases - RMS and HMS
# Xiliang Lin
# Sept, 2015
#
#####################################################################################################
# Settings
rm(list = ls())
run_on_Linux        = FALSE    # Determines the path settings

# Load Necessary Packages
library(data.table)
setNumericRounding(0)

# Directories
setwd("~/Keurig")
HMS_input_dir = "Data/HMS-Transactions"
RMS_input_dir = "Data/MLogit-Data"
meta_dir  = "Data/Meta-Data"
graph_dir = "Tabfigs/HW-Summary"

#---------------------------------------------------------------------------------------------------#
# Count Households 
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))

# A quick count of households
first_week_end = as.Date("2004-05-08")
max_sfirst_date = hh_list[, max(sfirst_date)]
hh_list[sfirst_date<=first_week_end, week_end:=first_week_end]
cweek = first_week_end
while (cweek < max_sfirst_date){
  cweek = cweek + 7
  hh_list[sfirst_date<=cweek & is.na(week_end), week_end:=cweek]
}

hh_list[, month:=substr(pmin(hfirst_date, sfirst_date), 1, 7)]
hh_list[, month:=paste0(month, "-01")]
hh_list[, month:=as.Date(month)]

# Plot the weekly sales (National level)
h_adopt = hh_list[, .(nhh=.N), by = "week_end"]
setkey(h_adopt, week_end)
h_adopt[, holiday:=FALSE]
for (yr in 2006:2013){
  h_adopt[week_end>=as.Date(paste(yr, 11, 15, sep="-")) & week_end<=as.Date(paste(yr, 12, 31, sep="-")), holiday:=TRUE]
}

h_adopt = h_adopt[week_end>=as.Date("2009-01-01") & week_end<=as.Date("2012-12-31"), ]
pdf(file=paste(graph_dir, "/figs/HMS-Adopters-By-Week.pdf", sep=""), width=10, height=5)
h_adopt[week_end>="2005-12-31",  plot(week_end, nhh, type = "o", xlab = "Week",
                                      ylab = "Number of Adopters", cex = 0.5)]
abline(v=as.Date("2010-02-24"))
abline(v=as.Date("2010-09-26"))
abline(v=as.Date("2011-03-10"))
abline(v=as.Date("2011-11-02"))

# Mother's day
abline(v=as.Date("2009-05-10"), lty=2)
abline(v=as.Date("2010-05-09"), lty=2)
abline(v=as.Date("2011-05-08"), lty=2)
abline(v=as.Date("2012-05-13"), lty=2)
abline(v=as.Date("2013-05-12"), lty=2)

# Father's day
abline(v=as.Date("2009-06-21"), lty=2)
abline(v=as.Date("2010-06-20"), lty=2)
abline(v=as.Date("2011-06-19"), lty=2)
abline(v=as.Date("2012-06-17"), lty=2)
abline(v=as.Date("2013-06-16"), lty=2)

grid(NA, 20, lwd = 1) # grid only in y-direction
dev.off()

# Define the month
h_adopt = hh_list[, .(nhh=.N), by = "month"]
setkey(h_adopt, month)
pdf(file=paste(graph_dir, "/figs/HMS-Adopters-By-Month.pdf", sep=""), width=10, height=5)
h_adopt[month>="2005-12-31",  plot(month, nhh, type = "o", xlab = "Week",
                                      ylab = "Number of Adopters")]
dev.off()

#---------------------------------------------------------------------------------------------------#
# RMS Prices and Sales

# Plot sales and prices
load(paste(RMS_input_dir, "/rms_hw.RData", sep=""))
setkeyv(rms_hw_prices, c("dma_code", "series", "week_end"))

# Flag End of Year as Holiday Season - From Nov 15th to end of year
# rms_hw_prices = rms_hw_prices[week_end>=as.Date("2009-01-01") & week_end<=as.Date("2012-12-31"), ]
rms_hw_prices[, holiday:=FALSE]
for (yr in 2006:2013){
  rms_hw_prices[week_end>=as.Date(paste(yr, 11, 15, sep="-")) & week_end<=as.Date(paste(yr, 12, 31, sep="-")), holiday:=TRUE]
}

# List of markets
mkt_list = c("Chicago", "NewYork", "Minneapolis", "Denver")
dma_code_list = c(602, 501, 613, 751)
 
# Graph for New York and series 3
pdf(file=paste(graph_dir, "/figs/RMS-Price-Units-By-Week-NewYork-Series3.pdf", sep=""), width=10, height=5)
par(mar=c(5,4,4,5)+.1)
rms_hw_prices[dma_code==501 & series==3 & week_end>="2008-07-11", 
              plot(week_end, price, type="o", col="red", xlab="Week", 
                   main = paste("Series", i), cex=0.5)]
abline(v=as.Date("2010-02-24"))
abline(v=as.Date("2010-09-26"))
abline(v=as.Date("2011-03-10"))
abline(v=as.Date("2011-11-02"))
par(new=TRUE)
rms_hw_prices[dma_code==501 & series==3, 
              plot(week_end, units, type="o",col="blue", xaxt="n",yaxt="n",xlab="",ylab="",cex=0.5)]
grid(NA, 20, lwd = 1) # grid only in y-direction
axis(4)
mtext("units",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("Price","Units"))
dev.off()


for (mkt in 1:length(mkt_list)){
  pdf(file=paste(graph_dir, "/figs/RMS-Price-Units-By-Week-", mkt_list[mkt],".pdf", sep=""), width=10, height=5)
  for (i in 2:5){
    par(mar=c(5,4,4,5)+.1)
    rms_hw_prices[dma_code==dma_code_list[mkt] & series==i, 
                  plot(week_end, price, type="o", col="red", xlab="Week", 
                       main = paste("Series", i), cex=0.5)]
    abline(v=as.Date("2010-02-24"))
    abline(v=as.Date("2010-09-26"))
    abline(v=as.Date("2011-03-10"))
    abline(v=as.Date("2011-11-02"))
    
    # Mother's day
    abline(v=as.Date("2009-05-10"), lty=2)
    abline(v=as.Date("2010-05-09"), lty=2)
    abline(v=as.Date("2011-05-08"), lty=2)
    abline(v=as.Date("2012-05-13"), lty=2)
    abline(v=as.Date("2013-05-12"), lty=2)
    
    # Father's day
    abline(v=as.Date("2009-06-21"), lty=2)
    abline(v=as.Date("2010-06-20"), lty=2)
    abline(v=as.Date("2011-06-19"), lty=2)
    abline(v=as.Date("2012-06-17"), lty=2)
    abline(v=as.Date("2013-06-16"), lty=2)
    
    par(new=TRUE)
    rms_hw_prices[dma_code==dma_code_list[mkt] & series==i, 
                  plot(week_end, units, type="o",col="blue", xaxt="n",yaxt="n",xlab="",ylab="",cex=0.5)]
    grid(NA, 20, lwd = 1) # grid only in y-direction
    axis(4)
    mtext("units",side=4,line=3)
    legend("topleft",col=c("red","blue"),lty=1,legend=c("Price","Units"))
  }
  
  for (i in 2:5){
    par(mar=c(5,4,4,5)+.1)
    rms_hw_prices[dma_code==dma_code_list[mkt] & series==i & holiday==FALSE, 
                  plot(week_end, price, type="o", col="red", xlab="Week",
                       main = paste("Series", i, "Excluding Holiday"), cex=0.5)]
    abline(v=as.Date("2010-02-24"))
    abline(v=as.Date("2010-09-26"))
    abline(v=as.Date("2011-03-10"))
    abline(v=as.Date("2011-11-02"))
    
    # Mother's day
    abline(v=as.Date("2009-05-10"), lty=2)
    abline(v=as.Date("2010-05-09"), lty=2)
    abline(v=as.Date("2011-05-08"), lty=2)
    abline(v=as.Date("2012-05-13"), lty=2)
    abline(v=as.Date("2013-05-12"), lty=2)
    
    # Father's day
    abline(v=as.Date("2009-06-21"), lty=2)
    abline(v=as.Date("2010-06-20"), lty=2)
    abline(v=as.Date("2011-06-19"), lty=2)
    abline(v=as.Date("2012-06-17"), lty=2)
    abline(v=as.Date("2013-06-16"), lty=2)
    
    par(new=TRUE)
    rms_hw_prices[dma_code==dma_code_list[mkt] & series==i & holiday==FALSE, 
                  plot(week_end, units, type="o",col="blue", xaxt="n",yaxt="n",xlab="",ylab="",cex=0.5)]
    grid(NA, 20, lwd = 1) # grid only in y-direction
    axis(4)
    mtext("units",side=4,line=3)
    legend("topleft",col=c("red","blue"),lty=1,legend=c("Price","Units"))
  }
  dev.off()
}
