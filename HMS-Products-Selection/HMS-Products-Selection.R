#####################################################################################################
#
# Products Selection: Machines and Coffee Pods
# Xiliang Lin
# Sept, 2015
#
#####################################################################################################
# Settings
rm(list = ls())

coffee_modules      = c(1463, 1464, 1465, 1466, 1467)
#Module Description
#1463 - Ground and Whole Bean Coffee - including most Keurig K-Cups
#1464 - Soluable Flavored Coffee - Latte, Cappuccino etc.
#1465 - Soluable Coffee - Essentially instant coffee.
#1466 - Liquid Coffee- e.g. Starbucks Frappuccino.
#1467 - Coffee Substitutes such as POSTUM and Pero, tends to be fairly small.

maker_modules       = 7755
#Module Description
#7755 - Coffee and Tea Maker Appliances - Single Cup Serving or Dripping Machines.

#Load Necessary Packages
library(data.table)
setNumericRounding(0)

#Set Working Folder Path Here
setwd("~")
input_dir = "Data/Nielsen/HMS-Raw-R"
meta_dir  = "Data/Nielsen/Common-Raw-R"
output_dir = "Keurig/Data/HMS-Transactions"

#---------------------------------------------------------------------------------------------------#
# Main Execution of the Codes

# Combine the Purchase of Coffee and Coffee Makers
module_list = c(coffee_modules, maker_modules)
pur_list = as.list(module_list)
module_counter = 1
for (module_i in module_list){
  load(paste(input_dir,"/Purchase-By-Module/", module_i, ".RData", sep=""))
  purchases[, product_module_code:=module_i]
  pur_list[[module_counter]]=purchases
  cat("Module:", module_i, "completed.", module_counter, "out of", length(module_list), "completed.\n")
  module_counter = module_counter+1
}
purchases = rbindlist(pur_list)
save(purchases, file = paste(output_dir, "/HMS-Purchases-Coffee.RData", sep=""))

# Obtain purchase information by week and type of outside option
load(paste0(meta_dir, "/Products.RData"))
hot_modules = c(1464, 1467, 1466, 1465, 1463, 1451, 1456, 1462, 1460, 1461, 
                1458, 1459, 1457, 1048)
# Need to filter
# 1484: Carbonated Soft Drink - Select the energy drinks
# Red Bull
# Monster
# Rockstar
# NOS R
# AMP R
# 8397: NUTRITIONAL SUPPLEMENTS
# 5-HOUR ENERGY
# 10 HOUR POWER
caf_modules = c(hot_modules, 1484, 1553)
dri_modules = c(caf_modules, 1033, 1030, 1034, 1032, 1040, 1038, 1031, 1036, 1042, 
                  1045, 1044, 1055, 3628, 3592, 3626, 3625, 1487)
gro_modules = products[(!is.na(department_code) & !department_code%in%c(9, 0, 8, 7, 99)), 
                       unique(product_module_code)]
gro_modules = setdiff(gro_modules, c(1463, 7755, 9999, 1274, 1272, 1277, 1282, 1276,
                                     1306, 1304, 1303, 1311, 1310, 1309, 1299, 1300,
                                     1301, 7370, 7373, 7360, 7375, 7385, 7453, 7454,
                                     7455, 7456, 7457, 7458, 7459, 6025, 7460, 7462, 
                                     7381, 7466, 7464, 7380, 6028, 7798))
all_modules = list.files(paste(input_dir,"/Purchase-By-Module/", sep=""))
all_modules = as.numeric(gsub(".RData", "", all_modules))
all_modules = setdiff(all_modules, c(1463, 7755))

# Create space holder for sales
load(paste0(input_dir, "/Meta-Data/Trips.RData"))
tnames = names(trips)
trips[, setdiff(tnames, "trip_code_uc"):=NULL]
trips[, `:=`(hot=0, caf=0, drink=0, grocery=0, rest=0)]
setkey(trips, trip_code_uc)

# Loop over modules to create the summary
module_counter = 1
for (module_i in all_modules){
  load(paste(input_dir,"/Purchase-By-Module/", module_i, ".RData", sep=""))
  sales = purchases[, .(rev = sum(total_price_paid-coupon_value)), by = "trip_code_uc"]
  setkey(sales, trip_code_uc)
  trips = sales[trips]
  trips[is.na(rev), rev:=0]
  trips[module_i%in%hot_modules, hot := hot + rev]
  trips[module_i%in%caf_modules, caf := caf + rev]
  trips[module_i%in%dri_modules, drink := drink + rev]
  trips[module_i%in%gro_modules, grocery := grocery + rev]
  trips[module_i%in%all_modules, rest := rest + rev]
  trips[, rev:=NULL]
  cat("Module:", module_i, "completed.", module_counter, "out of", length(all_modules), "completed.\n")
  module_counter = module_counter+1
}
trips_sales = copy(trips)
load(paste0(input_dir, "/Meta-Data/Trips.RData"))
setkey(trips, trip_code_uc)
trips = trips[trips_sales]
save(trips, file = "~/Desktop/Trips.RData")
