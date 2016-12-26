#####################################################################################################
#
# HMS Data Summary: Variety Seeking Behavior
# Obtain reduced form evidence for variety seeking in brands (satiation and repurchase probability)
# 
# Xiliang Lin
# Feb, 2016
#
#####################################################################################################
# Settings
rm(list = ls())

coffee_modules      = c(1463)
#Module Description
#1463 - Ground and Whole Bean Coffee - including most Keurig K-Cups

maker_modules       = 7755
#Module Description
#7755 - Coffee and Tea Maker Appliances - Single Cup Serving or Dripping Machines.

# Platform which I would to focus on
platform = "KEURIG"
focal_module = 1463 # Focus on ground and whole bean coffee 

# Purchases threshold
npurch = 5 # The purchase threshold for households to be selected.

#Load Necessary Packages
library(glmnet)
library(data.table)
setNumericRounding(0)
library(parallel)

#Set Working Folder Path Here
setwd("~/Keurig")
HMS_input_dir = "Data/HMS-Transactions"
RMS_input_dir = "~/!Data/Nielsen/RMS-Raw-R/Coffee-Related/Movement"
meta_dir  = "Data/Meta-Data"
output_dir = "Data/MLogit-Data"
graph_dir = "Tabfigs/MLogit-Data"
code_dir  = "Scripts/MLogit-Data"

#Source the function file
source('Scripts/HMS-Summary/functions.R')

#---------------------------------------------------------------------------------------------------#

# Initialize Parallel Execution Environment
cores = detectCores(logical=FALSE)
cl = makeCluster(cores)
invisible(clusterEvalQ(cl,library(data.table)))
invisible(clusterEvalQ(cl, setNumericRounding(0)))

#---------------------------------------------------------------------------------------------------#
# Load appropriate data
# Load HH file with flags
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))

# Load HMS Purchase data
load(paste(HMS_input_dir, "/HMS-Purchases-Coffee.RData", sep=""))

# Load Product information
load(paste(meta_dir, "/Products.RData", sep=""))

# Load Product information
load(paste(meta_dir, "/Stores.RData", sep=""))

# Make purchases trip, product unique
pnamelist = names(purchases)
pnamelist = setdiff(pnamelist, c("total_spent", "total_price_paid", "coupon_value", "deal_flag_uc","quantity"))
purchases[upc==9955504002 & quantity == 24, quantity:=2]
purchases[upc==9955504002 & quantity == 12, quantity:=1]

purchases = purchases[, .(total_price_paid = sum(total_price_paid),
                          coupon_value = sum(coupon_value),
                          quantity = sum(quantity)), by = pnamelist]

# Merge in the type information to the purchase data
setkeyv(purchases, c("upc", "upc_ver_uc"))
purchases=purchases[products[, .(upc, upc_ver_uc, ptype, brand_descr, size1_amount, multi)], nomatch=0L]
purchases[, month:=as.factor(format(purchase_date, '%Y-%m'))]

# Restrict purchases to specific software purchases
purchases = purchases[product_module_code %in% focal_module, ]

#---------------------------------------------------------------------------------------------------#
# Focus on the platform of choice
hh_list = hh_list[ptype==platform, ]
hh_list[, ptype:=NULL]
purchases = purchases[ptype=="KEURIG"|ptype=="OTHER", ]

# Focus on households make purchases above the threshold
hh_list = hh_list[sn>= npurch, ]

# Select only purchases from the selected households
setkeyv(purchases, c("household_code"))
setkey(hh_list, household_code)

# Select only purchases made by the selected households
purchases = purchases[hh_list[, .(household_code, sfirst_date, slast_date)], nomatch=0L]

# Define week_end to be the saturday ending that week. 
first_week_end = as.Date("2004-01-03")
max_purchase_date = purchases[, max(purchase_date)]
purchases[purchase_date<=first_week_end, week_end:=first_week_end]
cweek = first_week_end
while (cweek < max_purchase_date){
  cweek = cweek + 7
  purchases[purchase_date<=cweek & is.na(week_end), week_end:=cweek]
}

# Adjust price to average price per unit of serving
purchases[, price_paid := (total_price_paid - coupon_value)/(quantity*size1_amount)]

# Merge in household projection factor
purchases = purchases[hh_list[, .(household_code, projection_factor)], nomatch=0L]
setkeyv(purchases, c("household_code", "purchase_date", "trip_code_uc"))

#---------------------------------------------------------------------------------------------------#
# Compute the repurchase probability of brands given the cumulative purchase of a brand

state_func <- function(hh_code, data=purchases){
  hpurch = data[.(hh_code), ]
  nobs = nrow(hpurch)
  ptype_lag = rep(as.character(NA), nobs)
  brand_lag = rep(as.character(NA), nobs)
  brand_cum_purch = rep(0, nobs)
  brand_cum_units = rep(0, nobs)
  brand_cum_cups = rep(0, nobs)
  for (i in 1:nobs){
    if (i==1) {
      ptype_lag[i]=as.character(NA)
      brand_lag[i]=as.character(NA)
      brand_cum_purch[i]=NA
      brand_cum_units[i]=NA
      ptype_state = hpurch[i,]$ptype
      brand_state = hpurch[i,]$brand_descr
      trip_state = hpurch[i,]$trip_code_uc
      cum_state_purch = 1
      cum_state_units = hpurch[i, quantity]
      cum_state_cups = hpurch[i, quantity*size1_amount]
    } else{
      cptype_state = hpurch[i,]$ptype
      cbrand_state = hpurch[i,]$brand_descr
      ctrip_state = hpurch[i,]$trip_code_uc
      if (ctrip_state != trip_state){
        ptype_lag[i] = ptype_state
        brand_lag[i] = brand_state
        brand_cum_purch[i] = cum_state_purch
        brand_cum_units[i] = cum_state_units
        brand_cum_cups[i] = cum_state_cups
        if (brand_state == cbrand_state){
          cum_state_cups = cum_state_cups + hpurch[i, quantity*size1_amount]
          cum_state_units = cum_state_units + hpurch[i, quantity]
          cum_state_purch = cum_state_purch + 1
        } else{
          cum_state_cups = hpurch[i, quantity*size1_amount]
          cum_state_units = hpurch[i, quantity]
          cum_state_purch = 1
        }
        ptype_state = cptype_state
        brand_state = cbrand_state
        trip_state = ctrip_state
      } else{
        ptype_lag[i] = ptype_lag[i-1]
        brand_lag[i] = brand_lag[i-1]
        brand_cum_purch[i] = brand_cum_purch[i-1]
        brand_cum_units[i] = brand_cum_units[i-1]
        brand_cum_cups[i] = brand_cum_cups[i-1]
        if (brand_state == cbrand_state){
          cum_state_cups = cum_state_cups + hpurch[i, quantity*size1_amount]
          cum_state_units = cum_state_units + hpurch[i, quantity]
          cum_state_purch = cum_state_purch + 1
        } else{
          cum_state_cups = hpurch[i, quantity*size1_amount]
          cum_state_units = hpurch[i, quantity]
          cum_state_purch = 1
        }
        ptype_state = cptype_state
        brand_state = cbrand_state
      }
    }
  }
  return(data.table(ptype_lag = ptype_lag, brand_lag = brand_lag, brand_cum_purch = brand_cum_purch, 
                    brand_cum_units = brand_cum_units, brand_cum_cups = brand_cum_cups))
}

hh_codes = unique(purchases$household_code)
clusterExport(cl, c('purchases', 'state_func'))
state_list = parLapply(cl, hh_codes, state_func)
state_list = rbindlist(state_list)
purchases = cbind(purchases, state_list)
purchases[, repurch:=as.numeric(brand_descr==brand_lag)]
purchases[, brand_cum_purch:=brand_cum_purch-1]
#---------------------------------------------------------------------------------------------------#
# Compute the repurchase probability of brands given the cumulative purchase of a brand control 
# for consumer fixed effects1
glmcoef = function(formula) coef(glm(formula, family = binomial))[2]
glmse = function(formula) summary(glm(formula, family = binomial))$coefficients[2, 2]


# Excluding consumers without variation in lag and less than required number of purchases
purchases[, `:=`(NP = .N, purchv = var(repurch, na.rm=TRUE), lagv = var(brand_cum_purch, na.rm=TRUE)),
          by = "household_code"]
purchases = purchases[NP>=5 & purchv>0 & lagv>0, ]

# Function for extracting glm coefficients
pvariety = purchases[, .(cvariety = glmcoef(repurch~log(brand_cum_cups)),
                         sevariety = glmse(repurch~log(brand_cum_cups))), by = "household_code"]

# Estimate a pooled fixed effect model
