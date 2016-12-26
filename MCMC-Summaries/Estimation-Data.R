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
input_dir = "Data/MLogit-Data/"
graph_dir = "Tabfigs/MCMC-Summaries/"


#----------------------------------------------------------------------------------------------------#
# Load Estimation Data

load(paste(input_dir,"Coffee-Panel-Cond-Purchase.RData",sep=""))

big_markets = c(501, 602, 803, 504, 539, 506, 524, 613, 623, 753) # Big Markets
big_markets_name = c("New York", "Chicago", "Los Angeles", "Philadelphia", "Tampa - St. Petersburg", 
                     "Boston", "Atlanta", "Minneapolis - St. Paul", "Dallas - Fort Worth", "Phoenix")

brands = c("CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GLORIA JEAN'S KEURIG", "GREEN MOUNTAIN KEURIG",
           "MAXWELL HOUSE", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")

xnames = c("0OTHER", "CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GLORIA JEAN'S KEURIG", "GREEN MOUNTAIN KEURIG",
           "MAXWELL HOUSE", "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS",
           "STARBUCKS KEURIG", "TULLY'S KEURIG", "keurig", "brand_lag_ground", "brand_lag_keurig", "price_coef", "budget")

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
  p_table[i, 4] = temp_dt[, sum((1-keurig)*purchased)]
  p_table[i, 5] = temp_dt[, sum(keurig*purchased)]
  p_table[i, 6] = temp_dt[, sum((1-keurig)*purchased*size1_amount)/
                                   sum((1-keurig)*purchased)]
  p_table[i, 7] = temp_dt[, sum(keurig*purchased*size1_amount)/
                                   sum(keurig*purchased)]
  p_table[i, 8] = temp_dt[, sum((1-keurig)*purchased*price)/
                                   sum((1-keurig)*purchased)]
  p_table[i, 9] = temp_dt[, sum(keurig*purchased*price)/
                                   sum(keurig*purchased)]
  p_table[i, 10] = temp_dt[, sum((1-keurig)*purchased*price)/
                                   sum((1-keurig)*purchased*size1_amount)]
  p_table[i, 11] = temp_dt[, sum(keurig*purchased*price)/
                                    sum(keurig*purchased*size1_amount)]
  brand_freq = temp_dt[, .(npurch = sum(purchased)), by = c("brand_descr", "keurig")]
  p_table[i, 12] =  brand_freq[keurig==0 & brand_descr!="CTL BR", ][npurch==max(npurch), brand_descr]
  p_table[i, 13] =  brand_freq[keurig==1 & brand_descr!="CTL BR", ][npurch==max(npurch), brand_descr]
}

p_table = data.table(market = big_markets_name, dma_code = big_markets, p_table)
write.csv(p_table, file = paste(graph_dir, "/tabs/market_summary_stats.csv", sep=""), row.names = F)

#----------------------------------------------------------------------------------------------------#
# Fit a survival model to see whether utility is predictive of adotpion
load(paste(input_dir,"HH-HW-Panel.RData",sep=""))
hw_market_panel = as.data.frame(hw_market_panel)
hw_market_panel$SurvObj <- with(hw_market_panel, Surv(t, purchased==1))

res.cox1 <- coxph(SurvObj ~ price +  mu_diff, data =  hw_market_panel)
summary(res.cox1)





