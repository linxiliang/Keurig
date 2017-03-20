#Load Purchase data
load(paste(input_dir, "/HMS-Purchases-Coffee.RData", sep=""))

#Load Product information
load(paste(meta_dir, "/Products.RData", sep=""))

#Load HH information
load(paste(meta_dir, "/HH.RData", sep=""))
hh[, projection_factor:=projection_factor/10000]

#Identify the first purchase of Keurig/Other Single Serving Coffee

#Define machine holding status
#1. The consumer reports purchase of Tassimo/Keurig/Senseo/Gusto, and at least
#   three purchases of the portion packs
#2. If the consumer purchase any portion packs twice.

#Warning: This theoretically should be more complicated since we may need a burn in period to 
#allow the possibility of carrying a machine before entering the Nielsen Panel.

#Merge in the type information to the purchase data
setkeyv(purchases, c("upc", "upc_ver_uc"))
purchases=purchases[products[, .(upc, upc_ver_uc, ptype, brand_descr, size1_amount,
                                 multi, flavored, roast, flavor_descr, style_descr,
                                 colombian, kona, sumatra, wb, pthird)], 
                    nomatch=0L]
purchases[, month_temp:=as.factor(as.character(purchase_date, '%Y-%m'))]
purchases[, month:=max(purchase_date), by = "month_temp"]
purchases[, month_temp:=NULL]

#Plot the number of people who ever purchased a single serving product - machine or portion pack
purchasers_year = unique(purchases[ptype=="KEURIG", .(household_code, panel_year)])
purchasers_year = purchasers_year[, .(n_hh=.N), by = .(panel_year)]
setkeyv(purchasers_year, "panel_year")
pdf(file=paste(graph_dir, "/figs/purchasers_by_year.pdf", sep=""), width=7, height=5)
plot(purchasers_year[, panel_year], purchasers_year[, log10(n_hh)], type="o",
     xlab = "Year", ylab="Purchasers (log 10)")
dev.off()
setkeyv(purchases, c("household_code", "purchase_date", "upc", "upc_ver_uc"))

# Now more seriously, flag the holders based on the criteria mentioned in above 
# based on purchases criteria above 
purchases[, N_pack_purchases := sum(ifelse(ptype=="KEURIG" & product_module_code!=7755,1,0)), 
          by = c("household_code")]
purchases[, N_machine_purchases := sum(ifelse(ptype=="KEURIG" & product_module_code==7755,1,0)), 
          by = c("household_code")]

#Flag holders
#18739 Households are flagged to carry one of the single cup serving machines
#4296 Households purchased the portion packs only once
#864 Households purchased the machine, but never the coffee
purchases[, ever_holder:=as.numeric(N_pack_purchases>=2)]

#Record the first date of purchasing Portion pack or machine
purchases[, `:=`(first_date = as.Date(ifelse(ptype=="KEURIG", purchase_date, 
                                       as.Date("2020-01-01")), origin="1970-01-01"),
                 last_date = as.Date(ifelse(ptype=="KEURIG", purchase_date, 
                                             as.Date("2001-01-01")), origin="1970-01-01"))]
purchases[, `:=`(first_date = min(first_date),
                 last_date = max(last_date)), by = c("household_code")]
purchases[, `:=`(holder=ifelse(ever_holder==1&purchase_date>=first_date, 1, 0), 
                 adoption_year = year(first_date))]

#----------------------------------------------------------------------------------------------#
# Construct a control and treatment group to show no time trend in variety seeking
treat_ctrl_panel = purchases[product_module_code==1463 & 
                               (!adoption_year %in% c(2008, 2009, 2012, 2013)), ]
treat_ctrl_panel = treat_ctrl_panel[panel_year %in% c(2008, 2009, 2012, 2013) & 
                                      adoption_year %in% c(2010, 2011, 2020), ]
treat_ctrl_panel[, `:=`(treat = as.integer(adoption_year %in% c(2010, 2011)),
                        after = as.integer(panel_year %in% c(2012, 2013)))]
treat_ctrl_panel[, `:=`(np =length(unique(trip_code_uc)), ny = length(unique(panel_year))), 
                 by = c("household_code", "treat", "after")]
treat_ctrl_panel = treat_ctrl_panel[np>=5, ]
treat_ctrl_panel[, ny := length(unique(panel_year)), by = c("household_code")]
# Have to exist both before and after.
treat_ctrl_panel = treat_ctrl_panel[ny>=3, ]
# For later annual spending
treat_ctrl_panel[, ny := length(unique(panel_year)), by = c("household_code", "treat", "after")]

save(purchases, treat_ctrl_panel, hh, products, 
     file = paste(output_dir, "/HMS-Summary-Auxiliary.RData", sep=""))
