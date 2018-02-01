#####################################################################################################
#
# Aggregate Data to Market Level
# Xiliang Lin
# September, 2017
#
#####################################################################################################

# Settings
rm(list = ls())

# Load Necessary Packages
library(parallel)
library(data.table)
library(bit64)
setNumericRounding(0)

# Set Working Folder Path Here
setwd("~/Keurig")
meta_dir  = "Data/Meta-Data"
HMS_input_dir = "Data/HMS-Transactions"
RMS_input_dir = "Data/RMS-Movement"
mlogit_dir = "Data/MLogit-Data"
input_dir = "Data/Machine-Adoption"
output_dir = "Data/Counterfactual"

# Set seed
RNGkind("L'Ecuyer-CMRG")
set.seed(12345)

#---------------------------------------------------------------------------------------------------#
# Aggregate market share -- by consumer purchase and by scanner sales
#Load Purchase data
load(paste0(meta_dir, "/Products.RData"))
load(paste0(HMS_input_dir, "/HMS-Purchases-Coffee.RData"))
load(paste0(RMS_input_dir, "/RMS-Agg-Region-Week.RData"))
move_agg = move_agg[geo_level=="DMA" | geo_level == "NATIONAL", ] # Ideally, should use subset function
move_agg = move_agg[week_end>="2008-01-01", ]
gc()
#---------------------------------------------------------------------------------------------------#
# Match UPC
convert_upc = function(x){
  x_str = as.character(x)
  nx = nchar(x_str)
  if (nx<12) x_str = paste(c(rep("0", 12-nx), x_str), collapse = "")
  return(x_str)
}
upc_list = move_agg[, unique(upc)]
upc_table = data.table(upc = upc_list)
upc_table[, upc_char := sapply(upc, convert_upc)]
setkey(upc_table, upc)
setkey(move_agg, upc)
knames = copy(names(move_agg))
move_agg = upc_table[move_agg]
move_agg[, upc:=NULL]
setnames(move_agg, "upc_char", "upc")
#---------------------------------------------------------------------------------------------------#
# Merge products with product info
products = products[product_module_code==1463, ]
setkey(move_agg, upc, upc_ver_uc)
setkey(products, upc, upc_ver_uc)
p_ind_var = c("brand_descr", "ptype", "roast", "flavored", "lightR", "medDR", 
              "darkR", "assorted", "kona", "colombian", "sumatra", "wb")
move_agg = move_agg[products[, c("upc", "upc_ver_uc", "multi", "size1_amount", p_ind_var), 
                             with = F], nomatch=0L]
move_agg[, volume := units*size1_amount*multi]
move_agg = move_agg[, .(volume = sum(volume), revenue_RMS=sum(revenue_RMS)), 
                    by = c("panel_year", "geo_code", "geo_level", "week_end", p_ind_var)]
setkey(move_agg, panel_year, geo_code, geo_level, week_end, ptype, brand_descr)

# Drop all other types except ground coffee and Keurig
move_agg = move_agg[ptype %in% c("OTHER", "KEURIG"), ]
gc()

# Overall revenue and overall sales
move_agg[, tot_volume := sum(volume), by = c("panel_year", "geo_code", "geo_level", "week_end")]
move_agg[, inside_share := volume/tot_volume, by = c("panel_year", "geo_code", "geo_level", "week_end")]

# 
