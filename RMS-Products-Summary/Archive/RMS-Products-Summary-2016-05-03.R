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
if (run_on_Linux) {
  setwd("~")
  input_dir = "!Data/Nielsen/RMS-Raw-R/Coffee-Related/Movement"
  meta_dir  = "!Data/Nielsen/Common-Raw-R"
  output_dir = "Keurig/Data/RMS-Movement"
  tabfig_dir = "Keurig/Tabfigs/RMS-Products-Summary"
} else{
  setwd("D:/cygwin64/home/xlin0")
  input_dir = "!Data/Nielsen/RMS-Raw-R/Coffee-Related/Movement"
  meta_dir  = "!Data/Nielsen/Common-Raw-R"
  output_dir = "Keurig/Data/RMS-Movement"
  tabfig_dir = "Keurig/Tabfigs/RMS-Products-Summary"
}
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
#Keurig: brand description includes KEURIG|KCUP|KURG|KEURG
#Tassimo: brand description includes TSSM|TASSIMO
#Senseo: brand description includes SENSEO
#NESCAFE Dolce Gusto: brand description includes GUSTO|BONKA
ptype <- function(x){
   return ( ifelse(grepl("KEURIG", x)|grepl("KCUP", x)|
                   grepl("KURG", x)|grepl("KEURG",x),"KEURIG",
            ifelse(grepl("TSSM", x)|grepl("TASSIMO",x), "TASSIMO",
            ifelse(grepl("SENSEO", x),"SENSEO",
            ifelse(grepl("DOLCE GUSTO", x)|grepl("CAFE BONKA", x), "GUSTO", "OTHER")))) )
}


#Load the Products Data
load(paste(meta_dir, "/Products.RData", sep=""))
products[, `:=`(upc_descr = as.character(upc_descr),
                brand_descr = as.character(brand_descr),
                size1_units = as.character(size1_units))]
products[, `:=`(upc_descr = gsub("^\\s+|\\s+$", "", upc_descr),
                brand_descr = gsub("^\\s+|\\s+$", "", brand_descr),
                size1_units = gsub("^\\s+|\\s+$", "", size1_units))]
products[, ptype:=ptype(brand_descr)]
#convert Keurig to CT
products[ptype=="KEURIG"&size1_units=="OZ"&upc<=3070040801, 
         `:=`(size1_amount=round(size1_amount/0.40, 0),
              size1_units="CT")]
products[ptype=="KEURIG"&size1_units=="OZ"&upc>3070040801, 
         `:=`(size1_amount=round(size1_amount/0.40, 0),
              size1_units="CT")]
products[ptype=="GUSTO"&size1_units=="OZ"&product_module_code==1463, 
         `:=`(size1_amount=round(size1_amount/0.40, 0),
              size1_units="CT")]
products[ptype=="OTHER"&size1_units=="OZ"&product_module_code==1463, 
         `:=`(size1_amount=round(size1_amount/0.40, 0),
              size1_units="CT")]
products = products[, .(upc, upc_ver_uc, product_module_code, ptype, brand_code_uc, 
                        brand_descr, multi, size1_units, size1_amount)]
products = products[product_module_code %in% c(coffee_modules, maker_modules), ]
setkey(products, upc, upc_ver_uc)

#Coffee Brands dummies
#Keurig and Keurig Vue will be classified as GMCR 
#Bonka and GUSTO will be classified as "NESTLE"
blist = c("folgers", "maxwell", "green mountain", "seattle", "van houtte", 
           "wolfgang", "newman", "coffee people", 
          "tully", "donut house", "revv", "cafe escapes", "caribou", "gloria", 
          "kahlua", "donut shop","millstone", "diedrich", "emeril",
          "timothy", "eight", "barista", "dunkin", "starbucks", "gevalia",
          "king of joe", "carte noire", "yuban", "senseo", "nestle")
products[, bname:=""]
vlist = NULL
for (v in blist){
  cv = toupper(v)
  v = gsub(" ","_",v)
  if (v=="green_mountain"){
    products[(grepl(cv, brand_descr) | 
                brand_descr=="KEURIG" | 
                brand_descr=="KEURIG VUE"), bname:=toupper(v)]
  } else if (v == "nestle"){
    products[(grepl("NESTLE", brand_descr) | 
               grepl("NESCAFE", brand_descr)), bname:=toupper(v)]
  } else{
    products[grepl(cv, brand_descr), bname:=toupper(v)]
  }
  vlist = c(vlist, toupper(v))
}
products[bname=="", bname:="OTHER"]

#Load the Store Data
load('!Data/Nielsen/RMS-Raw-R/Meta-Data/Stores.RData')
stores = stores[,.(store_code_uc, panel_year, dma_code, store_zip3)]

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
platform_summary = platform_summary[, .(units=sum(units*size1_amount*multi), 
                                        revenue_RMS=sum(revenue_RMS),
                                        N_prods=length(upc), 
                                        N_brands=length(unique(brand_code_uc)),
                                        first_date_observed=min(week_end), 
                                        last_date_observed=max(week_end)),
                                    by = c("ptype", "bname", "product_module_code", 
                                           "geo_level", "geo_code", "week_end")]
platform_summary[, price:=revenue_RMS/units]
setkeyv(platform_summary, c("ptype", "bname", "product_module_code", 
                            "geo_level", "geo_code", "week_end"))

#Platform summary - with brand dummies
platform_summary[, grp_id:=.GRP, by = c("ptype", "bname", "product_module_code", 
                                        "geo_level", "geo_code")]
grp_list = unique(platform_summary[, c("ptype", "bname", "product_module_code", 
                                       "geo_level", "geo_code", "grp_id"), with=FALSE])
p_temp = platform_summary[, c("product_module_code", "grp_id", "week_end"), 
                          with=FALSE]
#Needs improvement - need to upgrade to deal with missing observations... ?
b_week= as.data.table(expand.grid(
  grp_id = unique(platform_summary[, grp_id]),
  week_end   = unique(platform_summary[, week_end])))
setkeyv(p_temp, c("grp_id", "week_end"))
setkeyv(b_week, c("grp_id", "week_end"))
setkeyv(grp_list, c("grp_id"))
b_week = p_temp[b_week]
b_week[, `:=`(indicator=as.numeric(!is.na(product_module_code)),
              product_module_code=NULL)]
b_week = grp_list[b_week] 
b_week[, grp_id:=NULL]
b_week = dcast(b_week, ptype+product_module_code+geo_level+geo_code+ week_end ~ bname, 
      value.var = "indicator")
#Replace NA with 0
nato0<-function(x){
  return(ifelse(is.na(x),0,x))
}
vlist = c(vlist, "OTHER")
vlist = setdiff(vlist, "CARTE_NOIRE")
b_week[, (vlist) := lapply(.SD, nato0), .SDcols = vlist]

#Summary by platform and geographical level
p_all_summary = platform_summary[, .(units=sum(units), 
                                     revenue_RMS=sum(revenue_RMS),
                                     N_prods=sum(N_prods), 
                                     N_brands=sum(N_brands),
                                     N_b = length(bname)),
                                 by = c("ptype", "product_module_code", 
                                        "geo_level", "geo_code","week_end")]
p_all_summary[, price:=revenue_RMS/units]

#Merge in brand information
setkeyv(p_all_summary, c("ptype", "product_module_code", 
                         "geo_level", "geo_code","week_end"))
setkeyv(b_week, c("ptype", "product_module_code", 
                         "geo_level", "geo_code","week_end"))
p_all_summary=p_all_summary[b_week, nomatch=0L]

save(platform_summary, p_all_summary,
     file=paste(output_dir, "/Platform_Summary.RData", sep=""))

#Take out time trend, time fixed effect. 
#Filter out dmas with very low number of sales before 12/31/2007.
load(paste(output_dir, "/Platform_Summary.RData", sep=""))
filter_level = 1000
xt_dma=p_all_summary[geo_level=="DMA", ]
xt_dma=xt_dma[ptype!="GUSTO"]
xt_dma[, filter:= mean(units[week_end<="2007-12-31"], na.rm=TRUE),  
       by = c("ptype", "geo_code")]
xt_dma=xt_dma[!is.nan(filter), ]
xt_dma=xt_dma[!is.nan(filter), ]
xt_dma=xt_dma[filter>=filter_level, ]
xt_dma[, t:=as.numeric(week_end-min(week_end))/7, by = c("ptype", "geo_code")]
xt_dma[, `:=`(m=month(week_end), y=year(week_end))]
#xt_dma=xt_dma[y>=2007,]

#Brands and their effects on sales of K-Cups
b_names = setdiff(names(xt_dma)[12:41], c("OTHER", "SENSEO", "NESTLE"))
p_i = 7
form = as.formula(paste("log(units)~", paste(b_names, collapse="+"),
                         "log(price)+factor(y)+factor(m)+t+I(t^2)+I(t^3)",
                         "I(t^4)+I(t^5)+I(t^6)+I(t^7)+factor(geo_code)",
                         sep="+"))
k_reg=lm(form, data=xt_dma[ptype=="KEURIG", ])
t_reg=lm(form, data=xt_dma[ptype=="TASSIMO", ])


#Plot time trend
k_ct = as.matrix(coef(k_reg)[c(1, (length(b_names)+21):(length(b_names)+21+p_i-1))])
t_ct = as.matrix(coef(t_reg)[c(1, (length(b_names)+21):(length(b_names)+21+p_i-1))])

t = seq(1:400)
tmat = matrix(rep(t, p_i+1), ncol=p_i+1)
tmat[,1]=1
for (j in 2:(p_i+1)){
  tmat[,j] = t^(j-1)
}
pdf(file=paste(tabfig_dir, "/figs/RMS-Weekly-Sales-Control.pdf", sep=""),
    width=16, height=8)
par(mfrow=c(1,2))
#Add GMCR fixed effects - otherwise, make no sense.
plot(t, exp(tmat%*%k_ct+4.411), type="l", xlab="Week", ylab="Weekly Sales",
     main="Keurig - Controlled")
plot(t, exp(tmat%*%t_ct), type="l", xlab="Week", ylab="Weekly Sales",
     main="Tassimo - Controlled")
par(mfrow=c(1,1))
dev.off()


#Plot average sales over time
mean_sales=xt_dma[, mean(units, na.rm=TRUE), by=c("ptype", "week_end")]
pdf(file=paste(tabfig_dir, "/figs/RMS-Average-Weekly-Sales.pdf", sep=""),
    width=16, height=8)
par(mfrow=c(1,2))
plot(mean_sales[ptype=="KEURIG", V1], type="l", xlab="Week", 
     ylab="Average Weekly Sales", main="Keurig")
plot(mean_sales[ptype=="TASSIMO", V1], type="l", xlab="Week", 
     ylab="Average Weekly Sales", main="Tassimo")
par(mfrow=c(1,1))
dev.off()

#Now the table of Fixed Effects
stargazer(k_reg, t_reg, align=TRUE, no.space=TRUE)


#Number of products
norder = 9
form = as.formula(paste("log(units)~", "N_prods+I(N_prods^2)+I(N_prods^3)+I(N_prods^4)",
                        "I(N_prods^5)+I(N_prods^6)+I(N_prods^7)+I(N_prods^8)+I(N_prods^9)",
                        "log(price)+factor(y)+factor(m)+t+I(t^2)+I(t^3)",
                        "I(t^4)+I(t^5)+I(t^6)+I(t^7)+factor(geo_code)",
                        sep="+"))
k_reg=lm(form, data=xt_dma[ptype=="KEURIG", ])
t_reg=lm(form, data=xt_dma[ptype=="TASSIMO", ])

#Plot trend with n-products
k_ct = as.matrix(coef(k_reg)[2:(norder+1)])
t_ct = as.matrix(coef(t_reg)[2:(norder+1)])
t = seq(1:5)
tmat = matrix(rep(t, norder), ncol=norder)
tmat[,1]=1
for (j in 1:norder){
  tmat[,j] = t^(j)
}
pdf(file=paste(tabfig_dir, "/figs/RMS-Sales-NProducts.pdf", sep=""),
    width=16, height=8)
par(mfrow=c(1,2))
#Add GMCR fixed effects - otherwise, make no sense.
plot(t, exp(tmat%*%k_ct), type="l", xlab="No of Products", ylab="Sales",
     main="Keurig - Controlled")
plot(t, exp(tmat%*%t_ct), type="l", xlab="No of Products", ylab="Sales",
     main="Tassimo - Controlled")
par(mfrow=c(1,1))
dev.off()

#Plot the number of products available and price of Keurig K-Cups
pdf(file=paste(tabfig_dir, "/figs/RMS-Price-Volume.pdf.pdf", sep=""),
    width=10, height=7, onefile=TRUE)
par(mfrow=c(2,2))
p_all_summary[.("KEURIG",1463,"NATIONAL","0"), {plot(week_end, log10(units), type="l")
  plot(week_end, price, type="l")
  plot(week_end, N_prods, type="l")
  plot(week_end, N_brands, type="l")
  mtext("KEURIG", side=3, outer=TRUE, line=-3)}]
p_all_summary[.("TASSIMO",1463,"NATIONAL","0"), {plot(week_end, log10(units), type="l")
  plot(week_end, price, type="l")
  plot(week_end, N_prods, type="l")
  plot(week_end, N_brands, type="l")
  mtext("TASSIMO", side=3, outer=TRUE, line=-3)}]
p_all_summary[.("SENSEO",1463,"NATIONAL","0"), {plot(week_end, log10(units), type="l")
  plot(week_end, price, type="l")
  plot(week_end, N_prods, type="l")
  plot(week_end, N_brands, type="l")
  mtext("SENSEO", side=3, outer=TRUE, line=-3)}]
par(mfrow=c(1,1))
dev.off()

#Get annual sales number by platform in RMS
p_year_sales = p_all_summary[geo_level=="NATIONAL", ]
p_year_sales[, yr:=year(week_end)]
p_year_sales=p_year_sales[, .(units=sum(units), revenue_RMS=sum(revenue_RMS)), 
                          by = c("ptype","yr")]

#Plot annual sales by platform
pdf(file=paste(tabfig_dir, "/figs/RMS-Revenue-Ptype-Year.pdf", sep=""),
    width=8, height=5)
p_year_sales[ptype=="KEURIG", plot(yr, log10(revenue_RMS), ylim=c(5,9), 
                                   type="o", lty=1, xlab="Year")]
p_year_sales[ptype=="TASSIMO", lines(yr, log10(revenue_RMS),
                                     type="o", lty=3)]
p_year_sales[ptype=="SENSEO"&yr<=2011, lines(yr, log10(revenue_RMS),
                                     type="o", lty=5)]
dev.off()

pdf(file=paste(tabfig_dir, "/figs/RMS-Units-Ptype-Year.pdf", sep=""),
    width=8, height=5)
p_year_sales[ptype=="KEURIG", plot(yr, log10(units), ylim=c(6,9.5), 
                                   type="o", lty=1, xlab="Year")]
p_year_sales[ptype=="TASSIMO", lines(yr, log10(units),
                                     type="o", lty=3)]
p_year_sales[ptype=="SENSEO"&yr<=2011, lines(yr, log10(units),
                                    type="o", lty=5)]
dev.off()

# Create the Brand panel 
brand_list = purchases[ptype=="KEURIG" & week_end>="2008-07-11" & product_module_code==1463, 
                       .(np = .N), by = c("dma_code", "brand_descr")]

