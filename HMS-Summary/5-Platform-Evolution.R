
#----------------------------------------------------------------------------------------------------#
# Load Auxiliary Data Sets 
load(paste(output_dir, "/HMS-Summary-Auxiliary.RData", sep=""))

# Construct a weighted share measure by Market

first_week_end = as.Date("2004-01-03")
max_purchase_date = purchases[, max(purchase_date)]
purchases[purchase_date<=first_week_end, week_end:=first_week_end]
cweek = first_week_end
while (cweek < max_purchase_date){
  cweek = cweek + 7
  purchases[purchase_date<=cweek & is.na(week_end), week_end:=cweek]
}

hh_first_week = purchases[holder==1, .(first_adpt_week = min(week_end)), by = c("household_code")]
hh_adpt_panel = data.table(expand.grid(household_code = unique(purchases$household_code),
                                       week_end = unique(purchases$week_end)))
hh_adpt_panel[, panel_year := year(week_end)]
setkey(hh_adpt_panel, household_code, panel_year)
setkey(hh, household_code, panel_year)
hh_adpt_panel = hh_adpt_panel[hh[,.(household_code, panel_year, dma_code, projection_factor)], 
                              nomatch=0L]
setkey(hh_adpt_panel, household_code)
setkey(hh_first_week, household_code)
hh_adpt_panel = hh_first_week[hh_adpt_panel]
hh_adpt_panel[, holder:=as.integer(week_end>=first_adpt_week)]
hh_adpt_panel[is.na(holder), holder:=0]
hh_adpt_panel = hh_adpt_panel[, .(ashare = sum(holder*projection_factor)/sum(projection_factor), 
                                  nholders_pfactor = sum(holder*projection_factor), 
                                  nholders = sum(holder),
                                  tothh_pfactor = sum(projection_factor),
                                  tothh = length(projection_factor)),
                              by = c("dma_code", "week_end")]
setkey(hh_adpt_panel, dma_code, week_end)
save(hh_adpt_panel, file = paste(output_dir, "/HH-Adoption-Share.RData", sep=""))
#----------------------------------------------------------------------------------------------------#

#Look at the platform evolution of Keurig 
holders_purchases[, nbrand_purch := .N, by = c("brand_descr", "ptype")]
br_list = unique(holders_purchases[nbrand_purch>=10, brand_descr])
load("Data/RMS-Movement/RMS-Agg-Region-Week.RData")
move_agg_natl = move_agg[geo_level=="NATIONAL", ]
rm(move_agg)
gc()
setkeyv(products, c("upc", "upc_ver_uc"))
setkeyv(move_agg_natl, c("upc", "upc_ver_uc"))
move_agg_natl = move_agg_natl[products[,.(upc, upc_ver_uc, brand_descr, ptype)], nomatch=0L]
### Wait here!
move_agg_natl[, month_temp:=substr(as.character(week_end), 1, 7)]
p_brands = move_agg_natl[, .(nbrands = length(unique(brand_descr)),
                             nupcs = length(unique(upc))), by = c("ptype", "month_temp")]
p_evolution = holders_purchases[purchase_date>=as.Date("2006-12-01"), 
                                .(nhh = length(unique(household_code))), 
                                by = c("ptype", "month")]
p_evolution[, month_temp:=substr(as.character(month), 1, 7)]

setkey(p_brands, ptype, month_temp)
setkey(p_evolution, ptype, month_temp)
p_evolution = p_evolution[p_brands, nomatch=0L]
p_evolution = p_evolution[month>="2007-12-31"]
pdf(file=paste(graph_dir, "/figs/keurig_HH_brands.pdf", sep=""), width=9, height=6)
cor_val = p_evolution[.("KEURIG"), round(cor(nbrands, nhh),3)]
par(mar = c(5,5,2,5))
p_evolution[.("KEURIG"), {plot(month, nbrands, xlab="Month", ylab="Number of Brands", cex = 0.5, 
                               ylim = c(0,50), type = "o", pch=16, 
                               main = bquote(rho == .(eval(cor_val))))}]
par(new = T)
p_evolution[.("KEURIG"), {plot(month, nhh, cex = 0.5, type = "o", col = "red", 
                               axes = F, pch=16, xlab = NA, ylab = NA)}]
axis(side = 4)
mtext(side = 4, line = 3, "Number of Active Users")
legend("topleft", c("Brands", "Active Users"), 
       lty=c(1,1), pch=c(16, 16), pt.cex=c(0.5, 0.5), col=c("black", "red"))
abline(v=as.Date("2012-09-30"), lty=3, col = "blue")
text(as.Date("2011-11-30"), 4000, "K-Cup Patent Expiration", cex = 0.8)
dev.off()
#----------------------------------------------------------------------------------------------#