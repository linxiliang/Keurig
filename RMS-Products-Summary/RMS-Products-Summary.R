#####################################################################################################
#
# RMS Products Summary: Machines and Coffee Pods
# Xiliang Lin
# Sept, 2015
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
library(stargazer)
setNumericRounding(0)

#Set Working Folder Path Here
setwd("~")
input_dir = "!Data/Nielsen/RMS-Raw-R/Coffee-Related/Movement"
meta_dir  = "Keurig/Data/Meta-Data"
output_dir = "Keurig/Data/RMS-Movement"
tabfig_dir = "Keurig/Tabfigs/RMS-Products-Summary"
#---------------------------------------------------------------------------------------------------#
# Main Execution of the Codes

#-----------------------------
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
#Load the Products Data
load(paste(meta_dir, "/Products.RData", sep=""))

#Load the Store Data
load('!Data/Nielsen/RMS-Raw-R/Meta-Data/Stores.RData')
stores = stores[,.(store_code_uc, panel_year, dma_code, store_zip3, parent_code)]

#For each coffee module, obtain the number of products available by platform, 
#and sales in quantity and $.
move_agg = as.list(c(coffee_modules, maker_modules))
start_time = proc.time()[3]
module_counter = 1
for (module_i in c(coffee_modules, maker_modules)){
  cat("\n----------------------------------------------------------------------\n")
  cat("Processing module ", module_i, " (", module_counter, "/", 
      length(c(coffee_modules, maker_modules)), ")\n\n", sep="")
  
  move_file = paste(input_dir, "/", module_i, ".RData", sep="")
  load(move_file)
  setkeyv(move, c("store_code_uc", "panel_year"))
  move = stores[move, nomatch=0L]
  
  #Sales by week, geographical level.
  natl = move[, list(units=sum(units, na.rm=TRUE),
                     revenue_RMS=sum(units*price/prmult, na.rm=TRUE),
                     panel_year = panel_year[1]),
              by = c("upc", "upc_ver_uc", "week_end")]
  natl[, geo_level:="NATIONAL"]
  natl[, geo_code := 0]
  dma = move[, list(units=sum(units, na.rm=TRUE),
                    revenue_RMS=sum(units*price/prmult, na.rm=TRUE),
                    panel_year = panel_year[1]),
             by = c("upc", "upc_ver_uc", "week_end", "dma_code")]
  dma[, geo_level:="DMA"]
  setnames(dma, "dma_code", "geo_code")
  zip3 = move[, list(units=sum(units, na.rm=TRUE),
                     revenue_RMS=sum(units*price/prmult, na.rm=TRUE),
                     panel_year = panel_year[1]),
              by = c("upc", "upc_ver_uc", "week_end", "store_zip3")]
  zip3[, geo_level:="ZIP3"]
  setnames(zip3, "store_zip3", "geo_code")
  
  move_agg[[module_counter]] = rbindlist(list(natl, dma, zip3)) 
  
  cat("\nRMS Module Processed ", module_i, " (", module_counter, "/", length(coffee_modules),
      "). Total time elapsed: ", proc.time()[3] - start_time, "\n\n", sep="")
  
  module_counter = module_counter + 1
}

move_agg = rbindlist(move_agg)
move_agg_natl = move_agg[geo_level=="NATIONAL",]
move_agg = move_agg[geo_level!="NATIONAL",]
move_agg[, geo_code_t:=units]
move_agg[, units:=NULL]
move_agg = rbindlist(list(move_agg_natl, move_agg))

setkeyv(move_agg, c("upc", "upc_ver_uc", "week_end", "geo_level", "geo_code"))
save(move_agg, file=paste(output_dir, "/RMS-Agg-Region-Week.RData", sep=""))

load(paste(output_dir, "/RMS-Agg-Region-Week.RData", sep=""))
setkeyv(move_agg, c("upc", "upc_ver_uc"))
platform_summary = move_agg[products, nomatch=0L]
platform_summary = platform_summary[ptype=="KEURIG", .(units=sum(units*size1_amount*multi), 
                                                       revenue_RMS=sum(revenue_RMS),
                                                       N_prods=length(upc), 
                                                       N_brands=length(unique(brand_code_uc)),
                                                       first_date_observed=min(week_end), 
                                                       last_date_observed=max(week_end)),
                                    by = c("ptype", "brand_descr", "product_module_code", 
                                           "geo_level", "geo_code", "week_end")]
platform_summary[, price:=revenue_RMS/units]
setkeyv(platform_summary, c("ptype", "brand_descr", "product_module_code", 
                            "geo_level", "geo_code", "week_end"))

#Platform summary - with brand dummies
platform_summary[, grp_id:=.GRP, by = c("ptype", "brand_descr", "product_module_code", 
                                        "geo_level", "geo_code")]
grp_list = unique(platform_summary[, c("ptype", "brand_descr", "product_module_code", 
                                       "geo_level", "geo_code", "grp_id"), with=FALSE])
p_temp = platform_summary[, c("product_module_code", "grp_id", "week_end"), 
                          with=FALSE]

#Needs improvement - need to upgrade to deal with missing observations... ?
b_week= as.data.table(expand.grid(
  grp_id = unique(platform_summary[product_module_code==1463, grp_id]),
  week_end   = unique(platform_summary[, week_end])))
setkeyv(p_temp, c("grp_id", "week_end"))
setkeyv(b_week, c("grp_id", "week_end"))
setkeyv(grp_list, c("grp_id"))
b_week = p_temp[b_week]
b_week[, `:=`(indicator=as.numeric(!is.na(product_module_code)),
              product_module_code=NULL)]
b_week = grp_list[b_week] 
b_week[, grp_id:=NULL]
b_week = dcast(b_week, ptype+geo_level+geo_code+ week_end ~ brand_descr, 
               value.var = "indicator")
#Replace NA with 0
nato0<-function(x){
  return(ifelse(is.na(x),0,x))
}

vlist = unique(grp_list[product_module_code==1463, brand_descr])
b_week[, (vlist) := lapply(.SD, nato0), .SDcols = vlist]

#Summary by platform and geographical level
p_all_summary = platform_summary[, .(units=sum(units), 
                                     revenue_RMS=sum(revenue_RMS),
                                     N_prods=sum(N_prods), 
                                     N_brands=sum(N_brands),
                                     N_b = length(brand_descr)),
                                 by = c("ptype", "product_module_code", 
                                        "geo_level", "geo_code","week_end")]
p_all_summary[, price:=revenue_RMS/units]

#Merge in brand information
setkeyv(p_all_summary, c("ptype", "geo_level", "geo_code","week_end"))
setkeyv(b_week, c("ptype", "geo_level", "geo_code","week_end"))
b_week = p_all_summary[product_module_code==1463, 
                       .(ptype, geo_level, geo_code, week_end, price, 
                         N_prods, N_brands, N_b)][b_week, nomatch=0L]
setnames(b_week, "price", "ground_price")

save(platform_summary, p_all_summary, b_week, 
     file=paste(output_dir, "/Platform_Summary.RData", sep=""))

#Take out time trend, time fixed effect. 
#Filter out dmas with very low number of sales before 12/31/2007.
load(paste(output_dir, "/Platform_Summary.RData", sep=""))
filter_level = 40000
xt_dma=p_all_summary[geo_level=="DMA" & product_module_code==7755 & week_end>="2008-01-01", ]

xt_dma[, filter:= sum(units, na.rm=TRUE),  
       by = c("ptype", "geo_code")]
xt_dma=xt_dma[!is.nan(filter), ]
xt_dma=xt_dma[!is.nan(filter), ]
xt_dma=xt_dma[filter>=filter_level, ] # top 20 markets

xt_dma[, t:=as.numeric(week_end-min(week_end))/7, by = c("ptype", "geo_code")]
xt_dma[, `:=`(m=month(week_end), y=year(week_end))]
#xt_dma=xt_dma[y>=2007,]
xt_dma[, `:=`(N_prods=NULL, N_brands=NULL, N_b=NULL)]

# Merge hardware sales with software
setkeyv(xt_dma, c("ptype", "geo_level", "geo_code","week_end"))
setkeyv(b_week, c("ptype", "geo_level", "geo_code","week_end"))
xt_dma = xt_dma[b_week, nomatch=0L]

#Brands and their effects on sales of Keurig Machines -- endogeneity and other variables make it difficult to identify.
b_names = names(xt_dma)[17:69]
v_names = paste0("V", 1:length(b_names))
setnames(xt_dma, b_names, v_names)
p_i = 7
form = as.formula(paste("log(units)~", paste(v_names, collapse="+"),
                        "+log(price)+log(ground_price)+log(N_brands)",
                        "+t+I(t^2)+I(t^3)+I(t^4)+I(t^5)+I(t^6)+I(t^7)+factor(geo_code)", sep=""))
k_reg=lm(form, data=xt_dma)


form = as.formula(paste("log(units)~", paste(v_names, collapse="+"),
                        "+log(price)+log(ground_price)+log(N_brands)",
                        "+t+I(t^2)+I(t^3)+I(t^4)+I(t^5)+I(t^6)+I(t^7)", sep=""))
k_reg=lm(form, data=xt_dma[geo_code==501, ])
