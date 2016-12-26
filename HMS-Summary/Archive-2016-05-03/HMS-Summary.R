#####################################################################################################
#
# HMS Data Summary: Machines and Coffee Pods
# Xiliang Lin
# Sept, 2015
#
#####################################################################################################
# Settings
rm(list = ls())
run_on_Linux        = FALSE    # Determines the path settings

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
setwd("~/Keurig")
input_dir = "Data/HMS-Transactions"
meta_dir  = "Data/Meta-Data"
output_dir = "Data/HMS-Summary"
graph_dir = "Tabfigs/HMS-Summary"

#Source the function file
source('Scripts/HMS-Summary/functions.R')
#---------------------------------------------------------------------------------------------------#
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
#   one purchase of the portion packs
#2. If the consumer purchase any portion packs twice.

#Warnin: This theoretically should be more complicated since we may need a burn in period to 
#allow the possibility of carrying a machine before entering the Nielsen Panel.

#Merge in the type information to the purchase data
setkeyv(purchases, c("upc", "upc_ver_uc"))
purchases=purchases[products[, .(upc, upc_ver_uc, ptype, brand_descr, size1_amount)], nomatch=0L]
purchases[, month:=as.factor(format(purchase_date, '%Y-%m'))]

#Plot the number of people who ever purchased a single serving product - machine or portion pack
purchasers_year = unique(purchases[ptype!="OTHER", .(household_code, panel_year)])
purchasers_year = purchasers_year[, .(n_hh=.N), by = .(panel_year)]
setkeyv(purchasers_year, "panel_year")
pdf(file=paste(graph_dir, "/figs/purchasers_by_year.pdf", sep=""), width=7, height=5)
plot(purchasers_year[, panel_year], purchasers_year[, log10(n_hh)], type="o",
     xlab = "Year", ylab="Purchasers (log 10)")
dev.off()
setkeyv(purchases, c("household_code", "purchase_date", "upc", "upc_ver_uc"))

#Now more seriously, flag the holders based on the criteria mentioned in above based on
#two purchases
purchases[, N_pack_purchases := sum(ifelse(ptype=="OTHER"|product_module_code==7755,0,1)), 
          by = c("household_code")]
purchases[, N_machine_purchases := sum(ifelse(ptype=="OTHER"|product_module_code!=7755,0,1)), 
          by = c("household_code")]
#Simply to get rid of missing space, Bad!!!! and result in inaccuracy!
#purchases[, `:=`(N_pack_purchases=mean(N_pack_purchases, na.rm=TRUE),
#                 N_machine_purchases=mean(N_machine_purchases, na.rm=TRUE)), 
#          by = c("household_code")]

#Flag holders
#18739 Households are flagged to carry one of the single cup serving machines
#4296 Households purchased the portion packs only once
#864 Households purchased the machine, but never the coffee
purchases[, holder:=as.numeric(N_pack_purchases>=3 & N_machine_purchases>=1)]
#Record the first date of purchasing Portion pack or machine
purchases[, first_date := min(ifelse(ptype=="OTHER", as.Date("2019-01-19"),
                                     purchase_date)), by = c("household_code")]
purchases[, `:=`(holder=ifelse(holder==1&purchase_date>=first_date, 1, 0),
                 first_date=NULL)]

#Just for fun (conditional on making at least 5 purchases in 1463 ground coffee)
#compute the number of brands needed to occupy at least 75% of the spending
#compute the number of brands ever purchased
#Ignore coupons
pbrands = purchases[product_module_code==1463]
pbrands[, np:= .N, by = c("household_code", "holder")]
pbrands=pbrands[np>=10, .(total_price_paid=-sum(total_price_paid, na.rm=TRUE)),
                by = c("household_code", "holder", "brand_descr")]
pbrands[, `:=`(nb = .N, tot_spent = sum(total_price_paid)), by = c("household_code")]
setkeyv(pbrands, c("household_code", "holder", "total_price_paid"))
pbrands[, `:=`(cumshare = cumsum(total_price_paid/tot_spent)), by = c("household_code")]
pbrands[, ind1:=as.numeric(cumshare<=0.5)]
pbrands[, ind2:=c(1, ind1[-.N]), by = c("household_code", "holder")]
pbrands=pbrands[, .(topb=sum(ind1==1|ind2==1), 
                    hhi=sum(((total_price_paid/tot_spent)*100)^2),
                    brand_descr=brand_descr[1]), 
                by = c("household_code", "holder", "nb")]

pdf(file=paste(graph_dir, "/figs/HMS-HH-HHI.pdf", sep=""), width=7, height=5)
hist(pbrands$hhi, main="Households HHI Distribution", xlab="HHI", xlim=c(0, 10000))
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-75NBrands-HoldersOrN.pdf", sep=""), 
    width=8, height=5)
par(mfrow=c(1,2))
barplot(pbrands[holder==1,table(topb)]/nrow(pbrands[holder==1, ]),ylim=c(0,0.6),
        main="Single Serving Holders")
barplot(pbrands[holder==0&topb<=7,table(topb)]/nrow(pbrands[holder==0&topb<=7,]),
        ylim=c(0,0.6), main="None Holders")
par(mfrow=c(1,1))
dev.off()

#Plot holders by year
holders_year = unique(purchases[holder==TRUE, .(household_code, panel_year)])
holders_year = holders_year[, .(n_hh=.N), by = .(panel_year)]
setkeyv(holders_year, "panel_year")
pdf(file=paste(graph_dir, "/figs/holders_by_year.pdf", sep=""), width=7, height=5)
plot(holders_year[, panel_year], holders_year[, log10(n_hh)], type="o",
     xlab = "Year", ylab="Holders (log 10)")
dev.off()

#Obtain holders of each type of machines, and obtain the first date
purchases[, `:=`(N_KCUP = sum(ifelse(ptype=="KEURIG"&product_module_code!=7755,1,0)),
                 N_TDISC = sum(ifelse(ptype=="TASSIMO"&product_module_code!=7755,1,0)), 
                 N_SPOD = sum(ifelse(ptype=="SENSEO"&product_module_code!=7755,1,0)), 
                 N_GCAP = sum(ifelse(ptype=="GUSTO"&product_module_code!=7755,1,0))), 
                 by = c("household_code")]

purchases[, `:=`(N_KEURIG = sum(ifelse(ptype=="KEURIG"&product_module_code==7755,1,0)),
                 N_TASSIMO = sum(ifelse(ptype=="TASSIMO"&product_module_code==7755,1,0)), 
                 N_SENSEO = sum(ifelse(ptype=="SENSEO"&product_module_code==7755,1,0)), 
                 N_GUSTO = sum(ifelse(ptype=="GUSTO"&product_module_code==7755,1,0))), 
          by = c("household_code")]

purchases[, `:=`(K_holder=(N_KCUP+N_KEURIG>=2),
                 T_holder=(N_TDISC+N_TASSIMO>=2),
                 S_holder=(N_SPOD+N_SPOD>=2),
                 G_holder=(N_GCAP+N_GCAP>=2))]

#Record the first date of purchasing Portion pack or machine
purchases[, `:=`(first_date_K = min(ifelse(ptype=="KEURIG", purchase_date,
                                           as.Date("2019-01-19"))),
                 first_date_T = min(ifelse(ptype=="TASSIMO", purchase_date,
                                           as.Date("2019-01-19"))),
                 first_date_S = min(ifelse(ptype=="SENSEO", purchase_date,
                                           as.Date("2019-01-19"))),
                 first_date_G = min(ifelse(ptype=="GUSTO", purchase_date,
                                           as.Date("2019-01-19")))),
                 by = c("household_code")]

purchases[, `:=`(K_holder=ifelse(K_holder==TRUE&purchase_date>=first_date_K, 1, 0),
                 T_holder=ifelse(T_holder==TRUE&purchase_date>=first_date_T, 1, 0),
                 S_holder=ifelse(S_holder==TRUE&purchase_date>=first_date_S, 1, 0),
                 G_holder=ifelse(G_holder==TRUE&purchase_date>=first_date_G, 1, 0),
                 first_date_K=NULL, first_date_T=NULL, first_date_S=NULL, first_date_G=NULL)]


#Plot holders by year
holders_year = as.list(c(1:4))
holders_year[[1]] = unique(purchases[K_holder==TRUE, .(household_code, panel_year)])
holders_year[[1]] = holders_year[[1]][, .(n_hh=.N), by = .(panel_year)]
holders_year[[1]][,ptype:="KEURIG"]
holders_year[[2]] = unique(purchases[T_holder==TRUE, .(household_code, panel_year)])
holders_year[[2]] = holders_year[[2]][, .(n_hh=.N), by = .(panel_year)]
holders_year[[2]][,ptype:="TASSIMO"]
holders_year[[3]] = unique(purchases[S_holder==TRUE, .(household_code, panel_year)])
holders_year[[3]] = holders_year[[3]][, .(n_hh=.N), by = .(panel_year)]
holders_year[[3]][,ptype:="SENSEO"]
holders_year[[4]] = unique(purchases[G_holder==TRUE, .(household_code, panel_year)])
holders_year[[4]] = holders_year[[4]][, .(n_hh=.N), by = .(panel_year)]
holders_year[[4]][,ptype:="GUSTO"]
holders_year = rbindlist(holders_year)
setkeyv(holders_year, "panel_year")

pdf(file=paste(graph_dir, "/figs/holders_by_type_year.pdf", sep=""), width=14, height=10)
par(mfrow=c(2,2))
xrange = c(min(holders_year[, panel_year]), max(holders_year[, panel_year]))
yrange = c(0, max(holders_year[, log10(n_hh)]))
plot(holders_year[ptype=="KEURIG", panel_year], holders_year[ptype=="KEURIG", log10(n_hh)], 
     type="o", xlab = "Year", ylab="Holders (log 10)", main="KEURIG",
     xlim=xrange, ylim=yrange)
abline(h=holders_year[ptype=="KEURIG", log10(n_hh)], col="grey10", lty="dotted") 
plot(holders_year[ptype=="TASSIMO", panel_year], holders_year[ptype=="TASSIMO", log10(n_hh)], 
     type="o", xlab = "Year", ylab="Holders (log 10)", main="TASSIMO",
     xlim=xrange, ylim=yrange)
abline(h=holders_year[ptype=="KEURIG", log10(n_hh)], col="grey10", lty="dotted") 
plot(holders_year[ptype=="SENSEO", panel_year], holders_year[ptype=="SENSEO", log10(n_hh)], 
     type="o", xlab = "Year", ylab="Holders (log 10)", main="SENSEO",
     xlim=xrange, ylim=yrange)
abline(h=holders_year[ptype=="KEURIG", log10(n_hh)], col="grey10", lty="dotted") 
plot(holders_year[ptype=="GUSTO", panel_year], holders_year[ptype=="GUSTO", log10(n_hh)], 
     type="o", xlab = "Year", ylab="Holders (log 10)", main="GUSTO",
     xlim=xrange, ylim=yrange)
abline(h=holders_year[ptype=="KEURIG", log10(n_hh)], col="grey10", lty="dotted") 
dev.off()
par(mfrow=c(1,1))  
#----------------------------------------------------------------------------------------------#
#Let's look at consumption of household carrying the machine
holders_purchases = purchases[K_holder==TRUE|T_holder==TRUE|S_holder==TRUE|G_holder==TRUE, ]
holders_purchases = holders_purchases[ptype!="OTHER"&product_module_code!=7755]
#Fist look at multihoming behavior (Sort/Who have tried two machines at least)
holders_purchases[, tot_holds := K_holder+T_holder+S_holder+G_holder]
holders_homing = holders_purchases[, .(tot_holds=max(tot_holds)), 
                                 by = c("household_code", "panel_year")]

holders_homing = holders_homing[, .(n_hh=.N), by = c("panel_year", "tot_holds")]
setkeyv(holders_homing, c("tot_holds", "panel_year"))
#Plot Homing By Year
pdf(file=paste(graph_dir, "/figs/homing_status_year.pdf", sep=""), width=7, height=5)
xrange = c(min(holders_homing[, panel_year]), max(holders_homing[, panel_year]))
yrange = c(0, max(holders_homing[, log10(n_hh)]))
plot(0,0, xlim = xrange ,ylim = yrange, xlab = "Year", ylab="No. of HH (log 10)", type = "n")
lines(holders_homing[.(1), panel_year], holders_homing[.(1), log10(n_hh)], type = "o", lty=1)
lines(holders_homing[.(2), panel_year], holders_homing[.(2), log10(n_hh)], type = "o", lty=2)
lines(holders_homing[.(3), panel_year], holders_homing[.(3), log10(n_hh)], type = "o", lty=3)
lines(holders_homing[.(4), panel_year], holders_homing[.(4), log10(n_hh)], type = "o", lty=4)
legend("topleft", legend = 1:4, lty=1:4, pch=1) # optional legend
dev.off()

#Now look at pattern after obtaining the machine/first recorded holder status
#Ignore multihoming for now.
#I need to account for projection factor to use it as weights
#The issue is that a household's projection factor could change from year to year.
#1. Within 90 days, number of portion packs, number of upcs, and number of brands purchased
#2. For each consecutive 90 days period, record the same thing.
TimeTrendPlot(holders_purchases, min.period=12, days=30, cond=FALSE, platform="KEURIG")
TimeTrendPlot(holders_purchases, min.period=12, days=30, cond=FALSE, platform="SENSEO")
TimeTrendPlot(holders_purchases, min.period=12, days=30, cond=FALSE, platform="TASSIMO")
TimeTrendPlot(holders_purchases, min.period=12, days=30, cond=FALSE, platform="ALL")


#Repeat excercise conditional on existence for at least 12 periods
TimeTrendPlot(holders_purchases, min.period=12, days=30, cond=TRUE, platform="KEURIG")
TimeTrendPlot(holders_purchases, min.period=12, days=30, cond=TRUE, platform="SENSEO")
TimeTrendPlot(holders_purchases, min.period=12, days=30, cond=TRUE, platform="TASSIMO")
TimeTrendPlot(holders_purchases, min.period=12, days=30, cond=TRUE, platform="ALL")

#----------------------------------------------------------------------------------------------#
#Now, let's summarize the variety of products being purchased by month.
setkey(holders_purchases, household_code, panel_year)
setkey(hh, household_code, panel_year)
holders_purchases = holders_purchases[hh[,.(household_code, panel_year, projection_factor)], 
                                      nomatch=0L]

PlatformTrend(holders_purchases)
#----------------------------------------------------------------------------------------------------#
#Look at the platform evolution of Keurig
p_evolution = holders_purchases[purchase_date>=as.Date("2007-01-01"), 
                                .(nhh = length(unique(household_code)),
                                  nbrands = length(unique(brand_descr)),
                                  nupcs = length(unique(upc))), 
                                by = c("ptype", "month")]
setkey(p_evolution, ptype, month)
pdf(file=paste(graph_dir, "/figs/keurig_HH_brands_upcs.pdf", sep=""), width=9, height=6)
par(mfrow=c(1,2))
p_evolution[.("KEURIG"), {plot(nbrands, nhh, xlab="No. of Brands",
                               ylab="Active Users")}]
p_evolution[.("KEURIG"), {plot(nupcs, nhh, xlab="No. of UPCs",
                               ylab="")}]
par(mfrow=c(1,1))
dev.off()

#----------------------------------------------------------------------------------------------------#
#Now look at entry after Starbucks
#Do a simple comparison for customers whose first purchase of Keurig happens
#1. Between OCT 1st, 2010 to Dec 31st, 2010 (Before Starbucks join Keurig)
#2. Between OCT 1st, 2011 to Dec 31st, 2011 (After Starbucks join Keurig)

holders_purchases[, `:=`(cohort1=ifelse(min(purchase_date)>=as.Date("2010-10-01")&
                                          min(purchase_date)<=as.Date("2010-12-31"),
                                        1, 0),
                         cohort2=ifelse(min(purchase_date)>=as.Date("2011-10-01")&
                                          min(purchase_date)<=as.Date("2011-12-31"),
                                        1, 0)), by = "household_code"]
s_cohort = holders_purchases[cohort1==1|cohort2==1, ]
s_cohort[, `:=`(cohort=ifelse(cohort1==1, 0, 1),
                cohort1=NULL, cohort2=NULL)]
s_cohort=s_cohort[purchase_date>=as.Date("2012-01-01")&purchase_date<=as.Date("2012-06-30"), ]
table(unique(s_cohort[, .(household_code, cohort)])[, cohort])
s_cohort[, .N*sum(total_price_paid*projection_factor)/sum(projection_factor), by="cohort"]
s_cohort[brand_descr=="STARBUCKS KEURIG", 
         .N*sum(total_price_paid*projection_factor)/sum(projection_factor), by="cohort"]

s_cohort[, sum(total_price_paid), by="cohort"]
s_cohort[brand_descr=="STARBUCKS KEURIG", sum(total_price_paid), by="cohort"]

#Look at some purchase history
setkeyv(purchases, c("upc", "upc_ver_uc"))
setkeyv(products, c("upc", "upc_ver_uc"))

purchases_brands=purchases[products[, .(upc, upc_ver_uc, brand_descr)], nomatch=0L]




