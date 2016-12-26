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
input_dir = "!Data/Nielsen/HMS-Raw-R"
meta_dir  = "Keurig/Data/Meta-Data"
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
  module_counter = module_counter+1
}

purchases = rbindlist(pur_list)
save(purchases, file = paste(output_dir, "/HMS-Purchases-Coffee.RData", sep=""))
