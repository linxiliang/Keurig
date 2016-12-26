#####################################################################################################
#
# Hardware Purchases - RMS and HMS
# Xiliang Lin
# May, 2016
#
#####################################################################################################
# Settings
rm(list = ls())
run_on_Linux        = FALSE    # Determines the path settings

# Load Necessary Packages
library(data.table)
setNumericRounding(0)

# Directories
setwd("~")
HMS_input_dir = "Keurig/Data/HMS-Transactions"
RMS_input_dir = "!Data/Nielsen/RMS-Raw-R/Coffee-Related/Movement"
meta_dir  = "Keurig/Data/Meta-Data"
output_dir = "Keurig/Data/RMS-Movement"
tabfig_dir = "Keurig/Tabfigs/RMS-Products-Summary"
graph_dir = "Keurig/Tabfigs/HW-Summary"

#---------------------------------------------------------------------------------------------------#

#Identify Hardware Series
#Load the Products Data
load(paste(meta_dir, "/Products.RData", sep=""))

products[.("064964520029", 1), upc_descr:="KK-C CF BRWR K45/GCSC&WF="] # is actually K45
products[.("064964500440", 1), upc_descr:="KRIG CF BRWR B44"] # is actually B44

# Classify products based on features, series and prices
c_list = as.list(1:5)
c_list[[1]] = c("B30", "K130")
c_list[[2]] = c("B31", "K10")
c_list[[3]] = c("B40", "K40", "K45", "B44")
c_list[[4]] = c("B60", "K60", "K65", "B66", "B50")
c_list[[5]] = c("B70", "K70", "B77", "K75")
#c_list[[4]] = c("B66", "B50")
#c_list[[4]] = c("B60", "K60", "K65")
#c_list[[5]] = c("K70/GCSC", "B77/GCSC=")
#c_list[[6]] = c("B70/FGCSC&TSC", "K70/GCSC&WF=", "K70/GCSC=")

products[, series:= as.integer(NA)]
for (i in c(1:5)){
  for (b in c_list[[i]]){
    products[grepl(b, upc_descr) & product_module_code == 7755 & ptype=="KEURIG", series:= i ]
  }
}
products = products[!is.na(series), ]

#Load the Store Data
load('!Data/Nielsen/RMS-Raw-R/Meta-Data/Stores.RData')
stores = stores[,.(store_code_uc, panel_year, dma_code, store_zip3, parent_code)]

# RMS Prices and Sales
# Plot sales and prices
move_file = paste(RMS_input_dir, "/", 7755, ".RData", sep="")
load(move_file)
setkeyv(move, c("store_code_uc", "panel_year"))
move = stores[move, nomatch=0L]
setkeyv(move, c("upc", "upc_ver_uc"))
move = move[products[, .(upc, upc_ver_uc, series)], nomatch=0L]

move = move[, list(units=sum(units, na.rm=TRUE),
                   revenue_RMS=sum(units*price/prmult, na.rm=TRUE),
                   price=median(price), 
                   panel_year = panel_year[1]),
            by = c("parent_code", "dma_code", "series", "week_end")]
move[, price_avg := revenue_RMS/units]
move = move[week_end>="2008-01-01", ]
setkey(move, dma_code, parent_code, series, week_end)

# Hardware Sales -- right before and right after announcement of Folgers and Starbucks
warehouse_hw = move[parent_code==9104&dma_code==501&week_end>="2008-01-01", ]
warehouse_hw[, holiday:=FALSE]
for (yr in 2006:2013){
  warehouse_hw[week_end>=as.Date(paste(yr, 11, 15, sep="-")) & week_end<=as.Date(paste(yr, 12, 31, sep="-")), holiday:=TRUE]
}

for (i in c(1,4,5)){
  pdf(file=paste(graph_dir, "/figs/RMS-Price-Units-WH-NewYork-Series", 
                 i, ".pdf", sep=""), width=10, height=5)
  par(mar=c(5,4,4,5)+.1)
  warehouse_hw[series==i, plot(week_end, price, type="o", col="red", xlab="Week", cex=0.5)]
  abline(v=as.Date("2010-02-24"), lty=2)
  abline(v=as.Date("2010-09-26"))
  abline(v=as.Date("2011-03-10"), lty=2)
  abline(v=as.Date("2011-11-02"))
  
  par(new=TRUE)
  warehouse_hw[series==i,plot(week_end, units, type="o",col="blue", 
                              xaxt="n",yaxt="n",xlab="",ylab="",cex=0.5)]
  grid(NA, 20, lwd = 1) # grid only in y-direction
  axis(4)
  mtext("units",side=4,line=3)
  legend("topleft",col=c("red","blue"),lty=1,legend=c("Price","Units"))
  dev.off()
}

for (i in c(1,4,5)){
  pdf(file=paste(graph_dir, "/figs/RMS-Price-Units-NoHoliday-WH-NewYork-Series", 
                 i, ".pdf", sep=""), width=10, height=5)
  par(mar=c(5,4,4,5)+.1)
  warehouse_hw[series==i & holiday==FALSE, 
                plot(week_end, price, type="o", col="red", xlab="Week", cex=0.5)]
  abline(v=as.Date("2010-02-24"), lty=2)
  abline(v=as.Date("2010-09-26"))
  abline(v=as.Date("2011-03-10"), lty=2)
  abline(v=as.Date("2011-11-02"))
  
  par(new=TRUE)
  warehouse_hw[series==i & holiday==FALSE, 
                plot(week_end, units, type="o",col="blue", xaxt="n",yaxt="n",xlab="",ylab="",cex=0.5)]
  grid(NA, 20, lwd = 1) # grid only in y-direction
  axis(4)
  mtext("units",side=4,line=3)
  legend("topleft",col=c("red","blue"),lty=1,legend=c("Price","Units"))
  dev.off()
}

# Flag End of Year as Holiday Season - From Nov 15th to end of year
# Use only one big mass merchandiser 
rms_hw_prices = move[parent_code==6901, ]
# rms_hw_prices = rms_hw_prices[week_end>=as.Date("2009-01-01") & week_end<=as.Date("2012-12-31"), ]
rms_hw_prices[, holiday:=FALSE]
for (yr in 2006:2013){
  rms_hw_prices[week_end>=as.Date(paste(yr, 11, 15, sep="-")) & week_end<=as.Date(paste(yr, 12, 31, sep="-")), holiday:=TRUE]
}

# List of markets
mkt_list = c("Chicago", "NewYork", "Minneapolis", "Denver")
dma_code_list = c(602, 501, 613, 751)

for (mkt in 1:length(mkt_list)){
  for (i in 2:5){
    pdf(file=paste(graph_dir, "/figs/RMS-Price-Units-", mkt_list[mkt],
                   "-Series", i, ".pdf", sep=""), width=10, height=5)
    par(mar=c(5,4,4,5)+.1)
    rms_hw_prices[dma_code==dma_code_list[mkt] & series==i, 
                  plot(week_end, price, type="o", col="red", xlab="Week", cex=0.5)]
    abline(v=as.Date("2010-02-24"), lty=2)
    abline(v=as.Date("2010-09-26"))
    abline(v=as.Date("2011-03-10"), lty=2)
    abline(v=as.Date("2011-11-02"))
    
    par(new=TRUE)
    rms_hw_prices[dma_code==dma_code_list[mkt] & series==i, 
                  plot(week_end, units, type="o",col="blue", xaxt="n",yaxt="n",xlab="",ylab="",cex=0.5)]
    grid(NA, 20, lwd = 1) # grid only in y-direction
    axis(4)
    mtext("units",side=4,line=3)
    legend("topleft",col=c("red","blue"),lty=1,legend=c("Price","Units"))
    dev.off()
  }
  
  for (i in 2:5){
    pdf(file=paste(graph_dir, "/figs/RMS-Price-Units-NoHoliday-", mkt_list[mkt],
                   "-Series", i, ".pdf", sep=""), width=10, height=5)
    par(mar=c(5,4,4,5)+.1)
    rms_hw_prices[dma_code==dma_code_list[mkt] & series==i & holiday==FALSE, 
                  plot(week_end, price, type="o", col="red", xlab="Week", cex=0.5)]
    abline(v=as.Date("2010-02-24"), lty=2)
    abline(v=as.Date("2010-09-26"))
    abline(v=as.Date("2011-03-10"), lty=2)
    abline(v=as.Date("2011-11-02"))
    
    par(new=TRUE)
    rms_hw_prices[dma_code==dma_code_list[mkt] & series==i & holiday==FALSE, 
                  plot(week_end, units, type="o",col="blue", xaxt="n",yaxt="n",xlab="",ylab="",cex=0.5)]
    grid(NA, 20, lwd = 1) # grid only in y-direction
    axis(4)
    mtext("units",side=4,line=3)
    legend("topleft",col=c("red","blue"),lty=1,legend=c("Price","Units"))
    dev.off()
  }
}


# Look at the introduction Starbucks and Folgers -- annoucement, any effect? 
# Look at warehouse - 9104
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

