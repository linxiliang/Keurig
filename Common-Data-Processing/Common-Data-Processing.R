#####################################################################################################
#
# Common Data Processing: Products and Stores
# Need to be run after cleaning up HMS Transactions
# Xiliang Lin
# Sept, 2015
# Last Update: April, 2017
#
#####################################################################################################
# Settings
rm(list = ls())
run_on_Linux        = TRUE    # Determines the path settings

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
input_dir  = "Data/Nielsen/Common-Raw-R"
output_dir = "Keurig/Data/Meta-Data"
HMS_input_dir = "Keurig/Data/HMS-Transactions"
#---------------------------------------------------------------------------------------------------#
# Main Execution of the Codes

# Load HMS Purchase data
load(paste(HMS_input_dir, "/HMS-Purchases-Coffee.RData", sep=""))

# Make purchases trip, product unique
pnamelist = names(purchases)
pnamelist = setdiff(pnamelist, c("total_spent", "total_price_paid", "coupon_value", "deal_flag_uc","quantity"))
purchases = purchases[, .(total_price_paid = sum(total_price_paid),
                          coupon_value = sum(coupon_value),
                          quantity = sum(quantity)), by = pnamelist]
setkeyv(purchases, c("upc", "upc_ver_uc"))

# obtain HMS revenue by upc, upc_ver_uc
hms_rev_table = purchases[, .(HMS_revenue = sum(total_price_paid-coupon_value)), 
                          by = c("upc", "upc_ver_uc", "product_module_code")]
hms_rev_table[, HMS_share := -HMS_revenue/sum(HMS_revenue), by = c("product_module_code")]
setkey(hms_rev_table, product_module_code, HMS_share)
hms_rev_table[, HMS_cum_share := cumsum(HMS_share)-HMS_share, by = c("product_module_code")]
hms_rev_table[, `:=`(HMS_share=-HMS_share, HMS_cum_share=-HMS_cum_share, product_module_code=NULL)]
setkey(hms_rev_table, upc, upc_ver_uc)

#---------------------------------------------------------------------------------------------------#

#Break the modules into different categories 

#Portion Packs - Instant Coffee
#Portion Packs - Keurig - Green Mountain Coffee
#Portion Packs - Senseo - Sara Lee, Philips
#Portion Packs - Tassimo - Baun, Bosch
#Portion Packs - Dolce Gusto - Nescafe
#Portion Packs - Nespresso - Nestle
#Portion Packs - Other
#Ground and Whole Bean Coffee - mainly for dripping machines
#Instant Coffee - None Portion Packs
#Specialty Coffee - None Portion Packs
#Liquid Coffee - Bottles or Cans
#Coffee substitutes - relatively small set of substitutes using grains etc.

#Identify Portion Packs
#Keurig: brand description includes KEURIG|KCUP|KURG|KEURG
#Tassimo: brand description includes TSSM|TASSIMO
#Senseo: brand description includes SENSEO
#NESCAFE Dolce Gusto: brand description includes GUSTO|BONKA
ptype <- function(x){
  return (ifelse(grepl("KEURIG VUE", x)|grepl("KEURIG V", x)|grepl("KEURG V", x)|grepl("KURG VU", x)|grepl("KVUE", x), "VUE", 
                 ifelse(grepl("KEURIG", x)|grepl("KCUP", x)|grepl("KURG", x)|grepl("KEURG",x), "KEURIG",
                  ifelse(grepl("TSSM", x)|grepl("TASSIMO",x), "TASSIMO",
                  ifelse(grepl("SENSEO", x),"SENSEO",
                  ifelse(grepl("DOLCE GUSTO", x)|grepl("CAFE BONKA", x), "GUSTO", "OTHER"))))))
}

toNA <- function(v){
  if(typeof(v)=="numeric"){
    return(v)
  } else{
    return(ifelse(v=="", as.character(NA), ifelse(v=="N/A", as.character(NA), v)))
  }
}

getmode <- function(v, na.rm = TRUE) {
  vc = typeof(v)
  if(na.rm){
    v = v[!is.na(v)]
  } 
  if(length(v)==0){
    return(ifelse(vc=="character", as.character(NA), as.numeric(NA)))
  } else{
    descrs <- unique(v)
    return(descrs[which.max(table(match(v, descrs)))])
  }
}

#Load the Products Data
load(paste(input_dir, "/Products.RData", sep=""))
products = products[product_module_code%in%(c(coffee_modules, maker_modules)), ]
products[brand_descr=="FOLGERS GOURMET SELECTNS KRG V", brand_descr:="FOLGERS GOURMET SLCTNS KURG"]
products[, ptype:=ptype(brand_descr)] # Use brand description to get portion packs.
setkeyv(products, c("upc", "upc_ver_uc"))

# Use information in prod_extra to identify other Keurig compatiable K-Cups
load(paste(input_dir, "/Product_Extra.RData", sep=""))
setkeyv(prod_extra, c("upc", "upc_ver_uc"))
prod_extra = prod_extra[products[, .(upc, upc_ver_uc)], nomatch=0L]
prod_extra[, flavor_descr:= toNA(flavor_descr)]
prod_extra = prod_extra[, .(container_descr=getmode(toNA(container_descr)),
                            flavor_descr=getmode(flavor_descr),
                            style_descr=getmode(toNA(style_descr)),
                            type_descr = getmode(toNA(type_descr)),
                            organic_claim_descr = getmode(toNA(organic_claim_descr)),
                            usda_organic_seal_descr = getmode(toNA(usda_organic_seal_descr)),
                            size2_units = getmode(toNA(size2_units)),
                            size2_amount = getmode(size2_amount)), by = c("upc", "upc_ver_uc")]
prod_extra[, matched := 1]

# Only products ever sold to Homescan panelist is relevant
products = products[hms_rev_table, nomatch=0L]

# Merge with products to attach additional meaningful attributes to it.
pnames1 = names(products)
pnames2 = setdiff(names(prod_extra), c("upc","upc_ver_uc"))
products=prod_extra[products]
setcolorder(products, c(pnames1, pnames2))
products[is.na(matched), matched:=0]

# Caribou coffee error
products[.("079849340670",2), size1_amount:=12]
products[.("009955506502"), size1_amount:=22]

#convert Keurig to CT
products[ptype=="KEURIG"&size1_units=="OZ"&as.numeric(upc)<=3070040801, 
         `:=`(size1_amount=round(size1_amount/0.1816, 0),
              size1_units="CT")]
products[ptype=="KEURIG"&size1_units=="OZ"&as.numeric(upc)>3070040801, 
         `:=`(size1_amount=round(size1_amount/0.5125, 0),
              size1_units="CT")]
products[ptype=="GUSTO"&size1_units=="OZ"&product_module_code==1463, 
         `:=`(size1_amount=round(size1_amount/0.5125, 0),
              size1_units="CT")]
products[ptype=="OTHER"&size1_units=="OZ"&product_module_code==1463, 
         `:=`(size1_amount=round(size1_amount/0.5, 0),
              size1_units="CT")]
setkey(products, upc, upc_ver_uc)

#---------------------------------------------------------------------------------------------------#
# Identify other products that Keurig compatiable - only in ground coffee category
gpurch = purchases[product_module_code==1463, ]
gpurch = gpurch[products[, .(upc, upc_ver_uc, ptype, brand_descr, size1_amount, multi,
                             container_descr, organic_claim_descr, usda_organic_seal_descr)], nomatch=0L]
gpurch[, price:=total_price_paid/(quantity*size1_amount*multi)]

# Obtain the median prices and purchase count
p_median = gpurch[, .(price=median(price), npurch=.N, first_date=min(purchase_date)),
                  by = c("upc", "upc_ver_uc", "ptype", "brand_descr", "container_descr", 
                         "organic_claim_descr", "usda_organic_seal_descr")]
# Ignore the ones with less than 5 purchases
p_median = p_median[npurch>=5, ]

# Define Keurig as the ones with price greater than 0.3 and the form is individual bags/pods etc.
p_median[ptype=="OTHER"&price>0.3&grepl("INDIVIDUAL", container_descr)&
           !grepl("BAG", container_descr)&brand_descr!="ILLY", pthird:=1]
p_median[grepl("STARBUCKS", brand_descr)&ptype=="OTHER"&pthird==1&container_descr=="INDIVIDUAL POD", pthird:=0]
p_median[brand_descr=="FOLGERS HOME CAFE", pthird:=0]
p_median[brand_descr=="GODIVA", pthird:=0]

kthird_list = p_median[pthird==1, .(upc, upc_ver_uc)]
products[kthird_list, pthird:=1]
products[is.na(pthird)&ptype=="KEURIG", pthird:=0]
products[pthird==1, ptype:="KEURIG"]

products[brand_descr == "GREEN MOUNTAIN CFE RSTR KEURIG", brand_descr:="GREEN MOUNTAIN COFFEE KEURIG"]
products[brand_descr == "KEURIG K-CUP", brand_descr:="KEURIG"]
products[brand_descr == "VAN HOUTTE CAFE KEURIG", brand_descr:="VAN HOUTTE KEURIG"]
products[brand_descr == "REVV PULSE KEURIG", brand_descr:="REVV KEURIG"]

products[, brand_descr_orig:=brand_descr]

# Changes brand names so that they correspond.
bkeys = c("CAMERON'S", "WHITE COFFEE", "ENTENMANN'S", "FOLGERS", "THE COFFEE BEAN & TEA LEAF", "COMMUNITY",
          "MAXWELL HOUSE", "GEVALIA KAFFE", "YUBAN", "MELITTA", "DISTINCTIVELY YOURS", "VAN HOUTTE",
          "FRENCH MARKET", "GROVE SQUARE", "CAZA TRAIL", "CHOCK FULL O NUTS", "MR COFFEE", "DON FRANCISCO'S",
          "MILLSTONE", "SAN FRANCISCO BAY", "GREEN MOUNTAIN", "WOLFGANG PUCK", "NEWMAN'S OWN ORGANICS",
          "COFFEE PEOPLE", "TULLY'S", "DONUT HOUSE", "REVV", "CAFE ESCAPES", "CARIBOU", 
          "GLORIA JEAN'S", "KAHLUA", "DONUT SHOP", "DIEDRICH", "EMERIL'S", "TIMOTHY'S WORLD",
          "EIGHT O'CLOCK", "BARISTA PRIMA", "STARBUCKS", "CLUB COFFEE", "BROOKLYN BEAN", "PUROAST",
          "ROGERS FAMILY", "THE ORGANIC COFFEE", "CAFFE' VERGNANO", "PANERA BREAD", "BLACK MOUNTAIN GOLD",
          "KAUAI", "PEET'S", "NEW ENGLAND", "DOOR COUNTY", "MARTINSON", "BROWN GOLD", "MARLEY COFFEE",
          "CAFE ENHANCA", "COPPER MOON", "DUNKIN' DONUTS")

for (bk in bkeys){
  products[grepl(bk, brand_descr), brand_descr:=bk]
}

# Clean up flavor and roast data
load(paste(input_dir, "/Product-Flavors.RData", sep=""))

# By upc and brand descr - fill in blanks for flavors and so forth
product_flavors[, `:=`(roast = as.integer(mean(roast, na.rm=TRUE)), 
                       flavored = as.integer(mean(flavored, na.rm=TRUE)), 
                       colombian = as.integer(mean(colombian, na.rm=TRUE)),
                       wb = as.integer(mean(wb, na.rm=TRUE))), by=c("upc", "upc_ver_uc")]

# Further clean up the flavor and roast data set
product_flavors[, `:=`(flavored_temp = as.integer(flavor_descr!="REGULAR"),
                       WBean = as.integer(grepl(" WB ", upc_descr)),
                       lightR = as.integer((grepl("L-R", style_descr)|grepl(" L-R", upc_descr)|grepl("MLR", style_descr)|
                                             grepl("MASTER BLEND", style_descr)|grepl(" M-B ", upc_descr)|
                                             grepl("MILD ROAST", style_descr)|grepl(" MLR ", upc_descr))&
                                             !(grepl("MDR", style_descr))),
                       mediumR = as.integer((grepl("MDR", style_descr)|grepl(" MDR ", upc_descr)|
                                               grepl(" MOR ", upc_descr)|grepl(" MOR ", style_descr)|
                                               grepl("ORIGINAL BLEND", style_descr)|
                                               (grepl("REGULAR", style_descr)&flavor_descr=="REGULAR")|
                                               grepl("CLASSIC ROAST", style_descr)|grepl("SGN BLEND", style_descr)|
                                               grepl("MEDIUM", style_descr)|grepl("CUSTOM RSTD", style_descr)|
                                               grepl("HOUSE BLEND", style_descr)|grepl("MED ", style_descr))&
                                              !(grepl("D-R", style_descr)|grepl("MEDIUM DARK", style_descr)|
                                                  grepl("MDK", style_descr)|grepl("MEDIUM DARK", style_descr)|
                                                  grepl("MDKR", upc_descr)|grepl("FRENCH ROAST", style_descr)|
                                                  grepl("MED DARK", style_descr)|grepl(" M-B ", upc_descr)|
                                                  grepl("L-R", style_descr)|grepl("MLR", style_descr))),                      
                       medDR  = as.integer(grepl("MED D-R", style_descr)|grepl("MDKR", upc_descr)|grepl(" MDK", style_descr)|
                                             grepl("RICH FRENCH ROAST", style_descr)|grepl("MDK-FR", style_descr)|
                                             grepl("MEDIUM DARK", style_descr)|grepl(" MDK ", upc_descr)|
                                             grepl("MED DARK", style_descr)|grepl("MEDIUM FRENCH ROAST", style_descr)),
                       darkR = as.integer(((grepl("D-R", style_descr)&!grepl("MED D-R", style_descr))|
                                             (grepl("FRENCH ROAST", style_descr)&!grepl("RICH FRENCH ROAST", style_descr))|
                                             grepl("ITALIAN ROAST", style_descr)|grepl("ESPRS", style_descr)|
                                             grepl("ITLN", style_descr)|grepl(" DR ", upc_descr)|
                                             grepl("FR-R", style_descr)|(grepl("D-R", upc_descr)&!grepl("MED D-R", style_descr))|
                                             grepl("NEW ORLEANS",style_descr)|grepl("EUROPEAN ROAST", style_descr)|
                                             grepl("EUROPEAN RSTD", style_descr))&
                                            !(grepl("MDR", style_descr)|grepl("MDR", upc_descr)|grepl("LT-DR", style_descr)|
                                                grepl("MED", style_descr)|grepl("ESPRS L-R", upc_descr))),
                       colombian_temp = as.integer(grepl("CLN", style_descr)|grepl("CLSP", style_descr)|
                                                     grepl("LCL ", style_descr)|grepl("CFTS", style_descr)),
                       kona = as.integer(grepl("KONA", style_descr)),
                       sumatra = as.integer(grepl("SUMATRA", style_descr)))]

product_flavors[, `:=`(assorted=as.integer(grepl("ASSORTED", style_descr)|(lightR+mediumR+medDR+darkR>=2)))]
product_flavors[assorted==1, `:=`(roast=0, lightR=0, mediumR=0, medDR=0, darkR=0)]
product_flavors[roast==0, `:=`(assorted=1, lightR=0, mediumR=0, medDR=0, darkR=0)]
product_flavors[assorted==1, `:=`(lightR=0, mediumR=0, medDR=0, darkR=0)]
product_flavors[roast==1, `:=`(assorted=0, lightR=1, mediumR=0, medDR=0, darkR=0)]
product_flavors[roast==2, `:=`(assorted=0, lightR=0, mediumR=1, medDR=0, darkR=0)]
product_flavors[roast==3, `:=`(assorted=0, lightR=0, mediumR=0, medDR=1, darkR=0)]
product_flavors[roast==4, `:=`(assorted=0, lightR=0, mediumR=0, medDR=0, darkR=1)]
product_flavors[, tot_roast := (assorted+lightR+mediumR+medDR+darkR)]

# Fix other variables
product_flavors[is.na(flavored), flavored:=flavored_temp]
product_flavors[is.na(colombian), colombian:=colombian_temp]
product_flavors[is.na(wb), wb:=WBean]
product_flavors[, `:=`(flavored_temp = NULL, colombian_temp = NULL, WBean = NULL)]

# Assume the rest are medium roast if not flavored coffee
product_flavors[tot_roast==0&flavored==0, mediumR:=1]
product_flavors[assorted==1, roast:=0]
product_flavors[lightR==1, roast:=1]
product_flavors[mediumR==1, roast:=2]
product_flavors[medDR==1, roast:=3]
product_flavors[darkR==1, roast:=4]

# Put missing values to 0, and roast to 2 (most popular roast)
product_flavors[is.na(flavor_descr) & is.na(roast), `:=`(flavored=0, roast=2)]
product_flavors[is.na(flavor_descr), `:=`(flavored=0)]

# Put all flavored coffee to 0
product_flavors[flavored==1,`:=`(assorted=0, lightR=0, mediumR=0, medDR=0, darkR=0, roast=-1)]

# Merge the auxiliary information into product file 
setkey(products, upc, upc_ver_uc)
setkey(product_flavors, upc, upc_ver_uc)
nameorder = c(names(products), c("roast", "flavored", "assorted", "lightR", "mediumR", "medDR", 
                                 "darkR", "colombian", "kona", "sumatra", "wb"))
products = product_flavors[, .(upc, upc_ver_uc, roast, flavored, assorted, lightR, 
                               mediumR, medDR, darkR, colombian, kona, sumatra, wb)][products]
setcolorder(products, nameorder)

save(products, file=paste(output_dir, "/Products.RData", sep=""))

#---------------------------------------------------------------------------------------------------#
#RMS Store and product data
file.copy('Data/Nielsen/RMS-Raw-R/Meta-Data/Stores.RData', 
          paste(output_dir,"/Stores.RData", sep=""),
          overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)

load('Data/Nielsen/RMS-Raw-R/Meta-Data/Product_Extra.RData')
prod_extra=prod_extra[products[,.(upc, upc_ver_uc)], nomatch=0L]
setkeyv(prod_extra, c("upc","upc_ver_uc","panel_year"))
save(prod_extra, file = paste(output_dir,"/RMS_Product_Extra.RData", sep=""))

file.copy('Data/Nielsen/RMS-Raw-R/Meta-Data/RMS_Versions.RData', 
          paste(output_dir,"/RMS_Versions.RData", sep=""),
          overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)

#------------------------------------------------------------------------------------------#
#Other store and Brand information
file.copy(paste(input_dir, "/Retailers.RData", sep=""), 
          paste(output_dir,"/Retailers.RData", sep=""),
          overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
file.copy(paste(input_dir, "/Brand-Variations.RData", sep=""), 
          paste(output_dir,"/Brand-Variations.RData", sep=""),
          overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)

#------------------------------------------------------------------------------------------#
# Trip Data
# Obtain purchase information by week and type of outside option
load(paste(input_dir, "/Products.RData", sep=""))
coff_module = c(1463)
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
all_modules = list.files(paste("Data/Nielsen/HMS-Raw-R/Purchase-By-Module/", sep=""))
all_modules = as.numeric(gsub(".RData", "", all_modules))
all_modules = setdiff(all_modules, c(1463, 7755))

# Create space holder for sales
load('Data/Nielsen/HMS-Raw-R/Meta-Data/Trips.RData')
tnames = names(trips)
trips[, setdiff(tnames, "trip_code_uc"):=NULL]
trips[, `:=`(coff=0, hot=0, caf=0, drink=0, grocery=0, rest=0)]
setkey(trips, trip_code_uc)

# Loop over modules to create the summary
module_counter = 1
for (module_i in c(all_modules,coff_module)){
  load(paste("Data/Nielsen/HMS-Raw-R/Purchase-By-Module/", module_i, ".RData", sep=""))
  sales = purchases[, .(rev = sum(total_price_paid-coupon_value)), by = "trip_code_uc"]
  setkey(sales, trip_code_uc)
  trips = sales[trips]
  trips[is.na(rev), rev:=0]
  trips[module_i==coff_module, coff := coff + rev]
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
load('Data/Nielsen/HMS-Raw-R/Meta-Data/Trips.RData')
setkey(trips, trip_code_uc)
trips = trips[trips_sales]
save(trips, file = paste(output_dir,"/Trips.RData", sep=""))
#------------------------------------------------------------------------------------------#
#Other HMS Meta Data
load('Data/Nielsen/HMS-Raw-R/Meta-Data/Product_Extra.RData')
setkeyv(prod_extra, c("upc","upc_ver_uc"))
prod_extra=prod_extra[products[,.(upc, upc_ver_uc)], nomatch=0L]
setkeyv(prod_extra, c("upc","upc_ver_uc","panel_year"))
save(prod_extra, file = paste(output_dir,"/HMS_Product_Extra.RData", sep=""))

file.copy('Data/Nielsen/HMS-Raw-R/Meta-Data/HH.RData', 
          paste(output_dir,"/HH.RData", sep=""),
          overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)