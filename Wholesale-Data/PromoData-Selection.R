#####################################################################################################
#
# Select relevant wholesale price data 
# Xiliang Lin
# Jan, 2017
#
#####################################################################################################

# Settings
rm(list = ls())
library(data.table)
library(bit64)

# Select all products in modules 
nielsen_modules = c(1463, 1464, 1465, 1466, 1467, 7755)
promo_modules = c(7010, 7020, 7030)
# 7010 Ground, 7020 Instant, 7030 Wholebean 

#####################################################################################################
# Load products
load('~/Keurig/Data/Meta-Data/Products.RData')
products = products[product_module_code%in%nielsen_modules, ]
upc_list_nielsen = products[, unique(upc)]

# Load wholesale data
load('~/data/Nielsen-Data/Projects/Price-Promotion-Analysis/PromoData/Processed/PromoData.RData')

# New UPC for wholesale data -- add 0s in the front to make 12 digits.
upc_info[, upc_new := paste0("00", upc)]

# Remove version changes due to item description
products[, `:=`(nversion=.N, version_min = min(upc_ver_uc)), c("upc", "size1_units", "size1_amount")]
products = products[upc_ver_uc==version_min, ]
products[, nversion:=.N, by = c("upc")]
products[ptype=="OTHER", size1_amount:=size1_amount/2] # Convert count to OZ to prep for merging

# Reconcile the two upc version definitions
upc_info = upc_info[category_code%in%promo_modules, ]
upc_info[, item_size := gsub('CT|OZ|PK|QT','',item_size)]
upc_info[grepl("L", item_size), item_size := gsub('CT|OZ|PK|QT','',item_size)]
upc_info[grepl("L|#", item_size), item_size := as.character(as.numeric(gsub('LB|L|#','',item_size))*2)]
upc_info[, item_size_numeric := as.numeric(item_size)]
setnames(upc_info, c("upc", "upc_new", "upc_ver_uc"), c("upc_old", "upc", "upc_ver_uc_old"))
upc_info[, `:=`(brand_descr=NULL, category_code=NULL)]
# Merge Data
setkeyv(upc_info, c("upc"))
setkeyv(products, c("upc"))
upc_info_merged = upc_info[products, nomatch=0L, allow.cartesian=T]
upc_info_merged[, `:=`(nversions_matched=.N, size_diff = abs(size1_amount-item_size_numeric),
                       size_diff_min = min(abs(size1_amount-item_size_numeric))), 
                by = c("upc_old", "upc_ver_uc_old")]
upc_info_merged = upc_info_merged[size_diff<=(size_diff_min+0.0001), ] # for numeric comparison purpose.
setnames(upc_info_merged,  c("upc_old", "upc", "upc_ver_uc_old", "upc_ver_uc"), 
         c("upc", "upc_new", "upc_ver_uc", "upc_ver_uc_new"))
upc_info_merged[, `:=`(item_size=NULL, nversions_matched=NULL, size_diff=NULL,
                       size_diff_min=NULL, nversion=NULL, version_min=NULL)]

# Select products if in the promo_modules or in upc_list
setkey(upc_info_merged, upc, upc_ver_uc)
setkey(wholesale, upc, upc_ver_uc)
wholesale = wholesale[upc_info_merged[,.(upc,upc_ver_uc, upc_new, upc_ver_uc_new)], nomatch=0L]
setnames(wholesale, c("upc", "upc_new", "upc_ver_uc", "upc_ver_uc_new"),
         c("upc_old", "upc", "upc_ver_uc_old", "upc_ver_uc"))
wnames = union(c("upc", "upc_ver_uc", "upc_old", "upc_ver_uc_old"), copy(names(wholesale)))
setcolorder(wholesale, wnames)
setnames(upc_info_merged, c("upc", "upc_new", "upc_ver_uc", "upc_ver_uc_new"),
         c("upc_old", "upc", "upc_ver_uc_old", "upc_ver_uc"))
unames = union(c("upc", "upc_ver_uc", "upc_old", "upc_ver_uc_old"), copy(names(upc_info_merged)))
setcolorder(upc_info_merged, unames)
save(wholesale, upc_info_merged, file = "~/Keurig/Data/Wholesale-Data/Wholesale-Matched.RData")