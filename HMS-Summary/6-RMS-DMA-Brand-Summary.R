
# Load Auxiliary Data Sets 
load(paste(output_dir, "/HMS-Summary-Auxiliary.RData", sep=""))
load(paste(output_dir, "/HH-Adoption-Share.RData", sep=""))

# Load and create the panel about brands
load("Data/RMS-Movement/RMS-Agg-Region-Week.RData")
move_agg_dma = move_agg[geo_level=="DMA", ]
rm(move_agg)
gc()

# Merge in product information
products[, `:=`(upc_c = upc, upc = as.numeric(upc))]
setkey(move_agg_dma, upc, upc_ver_uc)
setkey(products, upc, upc_ver_uc)
move_agg_dma = move_agg_dma[products[, .(upc, upc_ver_uc, upc_c, product_module_code, 
                                         brand_descr, brand_descr_orig, ptype)], nomatch=0L]
move_agg_dma[, `:=`(upc = upc_c, upc_c = NULL)]
setnames(move_agg_dma, "geo_code", "dma_code")
move_agg_dma[, dma_code := as.integer(dma_code)]

rms_dma_panel = move_agg_dma[ptype=="KEURIG", .(nbrand = length(unique(brand_descr)), 
                                                nbrand_orig = length(unique(brand_descr_orig)),
                                                tot_rev = sum(revenue_RMS)),
                             by = c("dma_code", "week_end")]

rms_brand_rev = move_agg_dma[ptype=="KEURIG", .(nbrand = length(unique(brand_descr)),
                                                nprod = length(unique(upc)),
                                                nbrand_orig = length(unique(brand_descr_orig)),
                                                tot_rev = sum(revenue_RMS)),
                             by = c("dma_code", "week_end", "brand_descr")]

rms_brand_panel=data.table(expand.grid(dma_code = unique(rms_brand_rev$dma_code),
                                       week_end = unique(rms_brand_rev$week_end), 
                                       brand_descr = unique(rms_brand_rev[tot_rev>=1000, 
                                                                          brand_descr])))
setkey(rms_brand_rev, dma_code, week_end, brand_descr)
setkey(rms_brand_panel, dma_code, week_end, brand_descr)
rms_brand_panel = rms_brand_rev[, .(dma_code, week_end, brand_descr, nbrand)][rms_brand_panel]
setkey(rms_brand_panel, brand_descr)
rms_brand_panel[, brand := .GRP, by = "brand_descr"]
rms_brand_panel[, avail:=as.integer(!is.na(nbrand))]
rms_brand_panel[, nbrand:=NULL]

rms_brand_avail = dcast(rms_brand_panel, dma_code + week_end ~ brand,
                        value.var = c("avail"))
setkey(rms_dma_panel, dma_code, week_end)
setkey(rms_brand_avail, dma_code, week_end)
rms_dma_panel = rms_dma_panel[rms_brand_avail, nomatch=0L]
save(rms_brand_panel, rms_dma_panel, rms_brand_rev, 
     file = "Data/RMS-Movement/RMS-DMA-Brand-Summary.RData")

dma_panel = hh_adpt_panel[rms_dma_panel, nomatch=0L]
# Impute Holidays - Thanksgiving, Christmas, New Year, Mother's Day, Father's Day, "Halloween",
#                   Valentine's Day.
require(timeDate)
thanks_list = as.Date(holiday(year = c(2006:2013), Holiday = c("USThanksgivingDay")))
christmas_list = as.Date(holiday(year = c(2006:2013), Holiday = c("USChristmasDay")))
mother_list = as.Date(c("2006-05-14", "2007-05-13", "2008-05-11", "2009-05-10", "2010-05-09", 
                        "2011-05-08", "2012-05-13", "2013-05-12"))
father_list = as.Date(c("2006-06-18", "2007-06-17", "2008-06-15", "2009-06-21", "2010-06-20", 
                        "2011-06-19", "2012-06-17", "2013-06-16"))

for (i in thanks_list){
  edate = i+2
  dma_panel[week_end==edate, thanksgiving := 1]
}
dma_panel[, `:=`(thanksgiving = as.integer(!is.na(thanksgiving)))]

for (i in christmas_list){
  wd = as.POSIXlt(as.Date(i, origin="1970-01-01"))$wday
  if (wd == 0) wd = 7
  edate = i + (6-wd)
  bdate = edate - 14
  adate = edate + 21
  dma_panel[week_end<=edate & week_end>=(edate-7), christmas := 1]
  dma_panel[week_end<=bdate & week_end>=(bdate-7), bchristmas := 1]
  dma_panel[week_end<=adate & week_end>=(edate+7), achristmas := 
              as.integer(as.Date(adate, origin="1970-01-01")-week_end)/7+1]
}
dma_panel[, `:=`(christmas = ifelse(is.na(christmas), 0, christmas),
                 bchristmas = ifelse(is.na(bchristmas), 0, bchristmas),
                 achristmas = ifelse(is.na(achristmas), 0, achristmas))]

for (i in mother_list){
  edate = i - 1
  dma_panel[week_end==(edate+7), mother := 1]
}
dma_panel[, `:=`(mother = as.integer(!is.na(mother)))]

for (i in father_list){
  edate = i - 1
  dma_panel[week_end==(edate+7), father := 1]
}
dma_panel[, `:=`(father = as.integer(!is.na(father)))]

# Save the data
save(dma_panel, file = paste(output_dir, "/DMA-Panel.RData", sep=""))

