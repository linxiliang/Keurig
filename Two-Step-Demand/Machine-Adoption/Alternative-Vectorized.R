#####################################################################################################
#
# Take Consumer Tastes and Compute the Expected Utility Gain
# Xiliang Lin
# December, 2016
#
#####################################################################################################

# Settings
rm(list = ls())

coffee_modules      = c(1463)
maker_modules       = 7755
big_markets = c(501, 602, 803, 504, 539, 506, 524, 613, 623, 753)

# Load Necessary Packages
library(data.table)
setNumericRounding(0)

# Set Working Folder Path Here
setwd("~/Keurig")
meta_dir  = "Data/Meta-Data"
HMS_input_dir = "Data/HMS-Transactions"
mlogit_dir = "Data/MLogit-Data"

#---------------------------------------------------------------------------------------------------#
# Load Datasets

# Load Household information data
load(paste(meta_dir, "/HH.RData", sep=""))
setkey(hh, household_code, panel_year)

# Load household coffee purchasing trips data
load(paste(mlogit_dir, "/hh_trip_panel.RData", sep=""))

# Load retailer price data panel
load(paste(mlogit_dir, "/Retailer_Brand_Price_Panel.RData", sep=""))

# Load purchase data to obtain the estimates for budget 
load(paste(HMS_input_dir, "/HMS-Purchases-Coffee.RData", sep=""))

#---------------------------------------------------------------------------------------------------#
# Four Steps
# 1. Compute the probability of visiting each retailer or no visit
# 2. Probability of purchase given inventory and inventory type
# 3. Compute the distribution of coffee spending -- assuming lognormal spending distribution.
# 3. Compute the utility gain at each retailer in each week for each household before adoption


# Brand Descriptions 
brands = c("CARIBOU KEURIG", "CHOCK FULL O NUTS", "CTL BR", "DONUT HOUSE KEURIG", "DUNKIN' DONUTS",
           "EIGHT O'CLOCK", "FOLGERS", "FOLGERS KEURIG", "GREEN MOUNTAIN KEURIG", "MAXWELL HOUSE", 
           "NEWMAN'S OWN ORGANICS KEURIG", "STARBUCKS", "STARBUCKS KEURIG", "TULLY'S KEURIG")
xnames = c("0OTHER", brands, "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
           "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")

# Obtain the trip probabilities
hh_panel[, quarter:= paste(format(week_end, "%y/"), 0, 
                           sub( "Q", "", quarters(week_end) ), sep = "")]
hh_trip_prob = hh_panel[, .(ntrips = .N), by = c("household_code", "panel_year", 
                                                 "quarter", "retailer_code")]
hh_trip_prob = hh_trip_prob[!is.na(retailer_code), ]
hh_trip_prob[, tot_trips := sum(ntrips), by = c("household_code", "panel_year", "quarter")]
# Missing Retailer Code -- no visit to any retailer store that generally sells coffee to consumers.
hh_trip_prob[, tprob:=ntrips/tot_trips] 
setkey(hh_trip_prob, household_code, panel_year)
hh_trip_prob = hh[,.(household_code, panel_year, dma_code)][hh_trip_prob, nomatch=0L]
warehouse_retailers = c(9101, 9103, 9104)
hh_trip_prob[, warehouse:=as.integer(retailer_code%in%warehouse_retailers)]
hh_trip_prob[, warehouse:=as.integer(any(warehouse==1)), by = c("household_code", "quarter")]
setkey(hh_trip_prob, household_code, dma_code, quarter, retailer_code)

# Impute the expected per-period adoption value.
retailer_panel = retailer_panel[brand_descr!="0NOTHING", ]
retailer_panel[, quarter:= paste(format(week_end, "%y/"), 0, 
                                 sub( "Q", "", quarters(week_end) ), sep = "")]
retailer_panel[keurig==1,  brand_descr := paste(brand_descr, "KEURIG")]
# Adapt the panel to create the X matrix
retailer_panel[, a1:=as.integer(!brand_descr%in%brands)]
alist = paste0("a", c(2:(length(brands)+1)))
i = 0
for (a in alist){
  i = i+1
  retailer_panel[, v:=as.integer(brand_descr==brands[i])]
  setnames(retailer_panel, "v", a)
}
retailer_panel[brand_descr=="CTL BR", brand_descr:=paste(brand_descr, retailer_code, sep="_")]
retailer_panel[, `:=`(lightR = as.integer(roast==1),
                      mediumR = as.integer(roast==2),
                      medDR = as.integer(roast==3),
                      darkR = as.integer(roast==4),
                      assorted = as.integer(roast==0))]
setkey(retailer_panel, dma_code, quarter, retailer_code)

# Compute the probability of making a purchase
# Predict probability of purchase once the consumer adopt Keurig
binomP <- function(form){
  binomreg = glm(form, family = "binomial")
  bprob = predict(binomreg, type = "response") 
  return(bprob)
}
hh_panel = hh_panel[, .(coffee_trip = sum(coffee_trip, na.rm=TRUE), k_first_week_end = k_first_week_end[1], 
                        brand_type_lag = brand_type_lag[1], ptype_lag=ptype_lag[1], inv_type=inv_type[1]),
                    by = c("household_code", "week_end", "quarter")]
hh_panel[, `:=`(coffee_trip = as.integer(coffee_trip>=1), 
                keurig_lag=as.integer(grepl("KEURIG",ptype_lag)))]
hh_panel[is.na(keurig_lag), keurig_lag:=0]
hh_panel[is.na(inv_type), inv_type:=0]
hh_panel[, pprob := binomP(coffee_trip~inv_type*keurig_lag), by = c("household_code")]
setkey(hh_panel, household_code, week_end)

#Remove not needed datasets and garbage collection
gc()

# Take the last part of markov chain to estimate consumer preferences
indx = seq(6000, 8000, 3)
load('Data/Bayes-MCMC/MDCEV-MCMC-All.RData')
bindv[, ,27:28] = exp(bindv[, ,27:28])/(1+exp(bindv[, ,27:28]))
pref = colMeans(bindv[indx, ,], 2)
rownames(pref) = hh_code_list[, household_code]

# Obtain mean and variance of spending in coffee for each trip -- 
# caveat (consumer spend more after adoption !!!)
hh_spent = purchases[product_module_code==1463, .(total_price_paid = sum(total_price_paid), 
                                                  coupon_value = sum(coupon_value)),
                     by = c("household_code", "trip_code_uc")]
hh_spent = hh_spent[total_price_paid>0.01, ]
hh_spent = hh_spent[, .(E = mean(log(total_price_paid)), Esd = sd(log(total_price_paid))), 
                    by = "household_code"]
hh_spent = hh_spent[household_code%in%hh_code_list[, household_code], ]
hh_spent = hh_spent[, cbind(E, Esd, household_code)]
rownames(hh_spent) = hh_spent[, 3]
hh_spent = hh_spent[,1:2]
hh_spent = hh_spent[order(row.names(hh_spent)), ]
pref = pref[order(row.names(pref)), ]
pref = cbind(pref, hh_spent)

# Compute the adoption value consumer by consumer
xvars = c(paste0("a", 2:15), "keurig", "flavored", "lightR", "medDR", "darkR", "assorted",
          "kona", "colombian", "sumatra", "wb", "brand_lag_keurig", "brand_lag")
hhValFun<-function(i){
  hh_prob_temp = hh_trip_prob[.(i), ]
  setkey(hh_prob_temp, dma_code, quarter, retailer_code)
  hh_retailers_temp = retailer_panel[hh_prob_temp, nomatch=0L]
  setkey(hh_retailers_temp, week_end)
  hh_lags = hh_panel[.(i), ]
  setkey(hh_lags, week_end)
  hh_retailers_temp = hh_retailers_temp[hh_lags[,.(week_end, brand_type_lag, keurig_lag, pprob)], nomatch=0L]
  hh_retailers_temp[, `:=`(brand_lag=as.integer(brand_descr%in%brand_type_lag & keurig==keurig_lag),
                           brand_lag_keurig=as.integer(brand_descr%in%brand_type_lag & keurig==keurig_lag)*keurig)]
  hh_retailers_temp[is.na(brand_lag), brand_lag:=0]
  hh_retailers_temp[is.na(brand_lag_keurig), brand_lag_keurig:=0]
  xmat = as.matrix(hh_retailers_temp[, xvars, with=FALSE])
  kmat = as.matrix(hh_retailers_temp[, cbind(1-keurig, keurig)])
  
  # Compute the quality of product
  hh_retailers_temp[, zb := xmat %*% pref[as.character(i),1:26]]
  # Compute the alpha of the product
  hh_retailers_temp[, alpha := kmat %*% pref[as.character(i),27:28]]
  
  # Number of retailer and week pair 
  hh_retailers_temp[, idx:=.GRP, by = .(dma_code, retailer_code, week_end)]
  setkey(hh_retailers_temp, idx)
  nw = hh_retailers_temp[, max(idx)]
  # HH week and retailer information 
  hh_agg = hh_retailers_temp[, .(dma_code=dma_code[1], retailer_code=retailer_code[1],
                                 week_end=week_end[1], tprob=tprob[1], pprob=pprob[1]),
                             by = c("idx")]
  setkey(hh_agg, idx)
  
  
  # Create a data matrix which is 300 times the original
  hh_tab = as.data.table(expand.grid(idx=1:nw, repx=1:300))
  setkey(hh_tab, idx)
  hh_tab = hh_retailers_temp[,.(idx,zb,price,alpha,keurig)][hh_tab, allow.cartesian=TRUE]
  nobs = nrow(hh_tab)
  hh_tab[, eps:=runif(nobs)]
  
  
  hh_tab[, `:=`(UE = 1/price * exp(zb + eps)*(E/price+1)^(alpha-1),
                U0 = 1/price * exp(zb + eps))]
  hh_tab[, `:=`(filter0 = as.integer(U0>max(UE))), by = .(idx, repx)]
  dt_v0 = hh_tab[filter0==1, .(ubar=eu2(min(UE-0.00001), max(U0+0.00001), 
                                        alpha, zb, price, eps, E)), by = .(idx, repx)]
  
  hh_tab[, `:=`(filter1 = as.integer(U0>max(UE))), by = .(idx, repx, keurig)]
  dt_v1 = hh_tab[filter1==1&keurig==0, .(ubar=eu2(min(UE-0.00001), max(U0+0.00001), 
                                                  alpha, zb, price, eps, E)), by = .(idx, repx)] 
  
  
  # Monte Carlo Integration
  # Vectorization is the key
  starttime <- proc.time()
  for (j in 1:3000){
    # Simulate eps
    hh_retailers_temp[, eps:=runif(nobs)]
    # Simulate E
    E = exp(rnorm(1, mean=pref[as.character(i), 29], sd= pref[as.character(i), 30]))
    # Compute the utility in the scenario
    hh_retailers_temp[, `:=`(UE = 1/price * exp(zb + eps)*(E/price+1)^(alpha-1),
                             U0 = 1/price * exp(zb + eps))]
    hh_retailers_temp[, `:=`(filter0 = as.integer(U0>max(UE))), 
                      by = .(idx)]
    dt_v = hh_retailers_temp[filter0==1, .(ubar = eu2(min(UE-0.00001), max(U0+0.00001), alpha, zb, price, eps, E)), 
                             by = .(idx)]
    uall_cum[dt_v$idx] = uall_cum[dt_v$idx] + dt_v$ubar
    hh_retailers_temp[, `:=`(filter1 = as.integer(U0>max(UE))), 
                      by = .(dma_code, retailer_code, week_end, keurig)]
    dt_v = hh_retailers_temp[filter1==1&keurig==0,  .(ubar = eu2(min(UE-0.00001), max(U0+0.00001), 
                                                                 alpha, zb, price, eps, E)), 
                             by = .(idx)]
    ugrd_cum[dt_v$idx] = ugrd_cum[dt_v$idx] + dt_v$ubar
    cat("Processed", j, "after", proc.time()-starttime, "seconds.\n\n")
  }
  hh_agg[, `:=`(uall=uall_cum/3000, ugrd_cum=ugrd_cum/3000, idx=NULL)]
  
  # Expenditure??? How shall I characterize expenditure??? 
  
  
  return(hh_r_agg_all)
}
