#####################################################################################################
#
# MCMC Summaries For the First Stage Estimation
# Xiliang Lin
# March, 2016
#
#####################################################################################################

# Settings
rm(list = ls())               # Clear workspace

# Packages
library(survival)
library(data.table)
setNumericRounding(0)

# Set Working Folder Path Here
setwd("~/Keurig")
meta_dir = "Data/Meta-Data/"
input_dir = "Data/MLogit-Data/"
machine_input_dir = "Data/Machine-Adoption/"
graph_dir = "Tabfigs/MCMC-Summaries/"


#----------------------------------------------------------------------------------------------------#
# Load Estimation Data
load(paste(input_dir,"MDC-Cond-Purchase-Flavors.RData",sep=""))
load(paste(meta_dir,"HH.RData",sep=""))

# Obtain the list of DMAs interested in. 
hh_num = hh_demo[, .(nh = .N), by = "dma_code"]
hh_num = hh_num[order(-nh), ]
big_markets = hh_num[1:30, dma_code]
big_markets_name = NULL
for (m in big_markets){
  big_markets_name = c(big_markets_name, hh[dma_code==m, unique(dma_descr)])
}
big_markets_name
# Trim the names
big_markets_name = c("New York", "Chicago", "Los Angeles", "Philadelphia", "Tampa - St. Petersburg", 
                     "Boston - Manchester", "Dallas - Ft Worth","Atlanta", "Minneapolis - St. Paul", 
                     "Phoenix", "Houston", "Detroit", "Seattle - Tacoma", "Denver", 
                     "Orlando - Daytona Beach", "Charlotte", "Sacramento - Stockton", "Washingotn DC", 
                     "St Louis", "San Francisco - Bay Area", "Cleveland", "Columbus", "Pittsburgh", 
                     "West Palm Beach - Fort Pierce", "San Antonio", "Buffalo", "Baltimore", 
                     "Hartford - New Haven", "Harrisburg Area PA", "Providence - New Bedford")

brands = c("0OTHER", "CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GREEN MOUNTAIN KEURIG", "MAXWELL HOUSE", 
           "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")
features = c("brand", "keurig", "flavored", "lightR", "medDR", "darkR", "assorted", "kona", "colombian", "sumatra", "wb")
xnames = c(brands, features[2:length(features)], "brand_lag_keurig", "brand_lag", 'ground_alpha', 'keurig_alpha')
alist = paste0("a", c(2:(length(brands)+1)))
nv = 20

p_table = matrix(rep(0, length(big_markets)*13), nrow=length(big_markets))
i = 0
for (mcode in big_markets){
  i = i+1
  hh_in_market = hh_demo[dma_code==mcode, unique(household_code)]
  temp_dt = hh_market_prod[household_code%in%hh_in_market, ]

  p_table[i, 1] = temp_dt[, length(unique(household_code))]
  p_table[i, 2] = temp_dt[keurig==1, length(unique(household_code))]
  p_table[i, 3] = temp_dt[, length(unique(trip_code_uc))]
  p_table[i, 4] = temp_dt[, (length(unique(trip_code_uc * (1-keurig)))-1)]
  p_table[i, 5] = temp_dt[, (length(unique(trip_code_uc * keurig))-1)]
  p_table[i, 6] = temp_dt[, sum((1-keurig)*purchased*size)/(length(unique(trip_code_uc * (1-keurig)))-1)]
  p_table[i, 7] = temp_dt[, sum(keurig*purchased*size)/(length(unique(trip_code_uc * keurig))-1)]
  p_table[i, 8] = temp_dt[, sum((1-keurig)*purchased*price*size)/
                            (length(unique(trip_code_uc * (1-keurig)))-1)]
  p_table[i, 9] = temp_dt[, sum(keurig*purchased*price*size)/
                            (length(unique(trip_code_uc * keurig))-1)]
  p_table[i, 10] = temp_dt[, sum((1-keurig)*purchased*price*size)/
                             sum((1-keurig)*purchased*size)]
  p_table[i, 11] = temp_dt[, sum(keurig*purchased*price*size)/
                             sum(keurig*purchased*size)]
  brand_freq = temp_dt[brand_descr!="0OTHER", .(revenue = sum(purchased*size*price)), by = c("brand_descr", "keurig")]
  p_table[i, 12] =  brand_freq[keurig==0 & brand_descr!="CTL BR", ][revenue==max(revenue), brand_descr]
  p_table[i, 13] =  brand_freq[keurig==1 & brand_descr!="CTL BR", ][revenue==max(revenue), brand_descr]
}
p_table = data.table(market = big_markets_name, dma_code = big_markets, p_table)
write.csv(p_table, file = paste(graph_dir, "/tabs/market_summary_stats.csv", sep=""), row.names = F)

#----------------------------------------------------------------------------------------------------#
# Fit a survival model to see whether utility is predictive of adotpion
load(paste(machine_input_dir, "HW-MU-Panel.RData",sep=""))
hw_market_panel = as.data.frame(hw_market_panel)
hw_market_panel$SurvObj <- with(hw_market_panel, Surv(t, purchased==1))

res.cox1 <- coxph(SurvObj ~ price +  mu_diff, data =  hw_market_panel)
summary(res.cox1)





