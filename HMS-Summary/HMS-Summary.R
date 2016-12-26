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
library(gridExtra)

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
purchases[, ever_holder:=as.numeric(N_pack_purchases>=3)]

#Record the first date of purchasing Portion pack or machine
purchases[, first_date:=as.Date(ifelse(ptype=="KEURIG", purchase_date, 
                                       as.Date("2020-01-01")), origin="1970-01-01"), 
          by = c("household_code")]
purchases[, first_date := min(first_date), by = c("household_code")]
purchases[, `:=`(holder=ifelse(ever_holder==1&purchase_date>=first_date, 1, 0), 
                 adoption_year = year(first_date))]

#Just for fun (conditional on making at least 10 purchases in 1463 ground coffee)
#compute the number of brands needed to occupy at least 75% of the spending
#compute the number of brands ever purchased
#Inclusive of coupons
pbrands = purchases[product_module_code==1463]
pbrands[, np:= .N, by = c("household_code", "ever_holder", "holder")]
pbrands=pbrands[np>=5, .(total_brand_paid=-sum((total_price_paid - coupon_value), na.rm=T)),
                by = c("household_code", "ever_holder", "holder", "brand_descr")]
pbrands[, `:=`(nb = .N, tot_spent = sum(total_brand_paid)), 
        by = c("household_code", "ever_holder", "holder")]
setkeyv(pbrands, c("household_code", "ever_holder", "holder", "total_brand_paid"))
pbrands[, `:=`(share = total_brand_paid/tot_spent, cumshare = 
                 cumsum(total_brand_paid/tot_spent), rank = 1:.N), 
        by = c("household_code", "ever_holder", "holder")]
pbrands[, `:=`(last_cumshare = cumshare - share)]
pbrands[, ind1:=as.numeric(last_cumshare<=0.75)]
pbrands=pbrands[, .(topb=sum(ind1), 
                    hhi=sum(((share)*100)^2),
                    c1 = sum(share * as.integer(rank<=1)),
                    c2 = sum(share * as.integer(rank<=2)),
                    c3 = sum(share * as.integer(rank<=3)),
                    brand_descr=brand_descr[1]), 
                by = c("household_code", "ever_holder", "holder", "nb")]

# HHI Before Adoption

hist(pbrands[ever_holder==0, hhi], xlab="HHI", xlim=c(0, 10000), nclass=20, 
     ylim=c(0, 0.0003), freq=F, main = "Never Held SSCM")
hist(pbrands[ever_holder==1 & holder==0, hhi], xlab="HHI", xlim=c(0, 10000), 
     nclass=20, ylim=c(0, 0.0003), freq=F, main = "Held SSCM")

pdf(file=paste(graph_dir, "/figs/HMS-HHI-BeforeAdoption.pdf", sep=""), width=8, height=5)
pbrands[,`:=`(Status = ifelse(ever_holder==0, 'Never Adopted', 'Before Adoption'))]
ggplot(pbrands[holder==0, .(hhi, Status)], aes(hhi, fill = Status)) + labs(x = "HHI") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=400, position = 'identity')+
  theme(legend.justification=c(0,1), legend.position=c(0,1))
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-HHI-HH-BAfter.pdf", sep=""), width=8, height=5)
pbrands[,`:=`(Status = ifelse(holder==0, 'Before', 'After'))]
ggplot(pbrands[ever_holder==1, .(hhi, Status)], aes(hhi, fill = Status)) + labs(x = "HHI") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=400, position = 'identity')+
  theme(legend.justification=c(0,1), legend.position=c(0,1))
dev.off()

# HHI Panel
pdf(file=paste(graph_dir, "/figs/HMS-HHI-Panel.pdf", sep=""), width=8, height=5)
par(mfrow=c(2,2))
hist(pbrands[ever_holder==0, hhi], xlab="HHI", xlim=c(0, 10000), nclass=20, 
     ylim=c(0, 0.0003), freq=F, main = "Never Held SSCM")
hist(pbrands[ever_holder==1 & holder==0, hhi], xlab="HHI", xlim=c(0, 10000), 
     nclass=20, ylim=c(0, 0.0003), freq=F, main = "Held SSCM")
hist(pbrands[ever_holder==1 & holder==0, hhi], xlab="HHI", xlim=c(0, 10000), 
     nclass=20, ylim=c(0, 0.00035), freq=F, main = "Before SSCM")
hist(pbrands[ever_holder==1 & holder==1, hhi], xlab="HHI", xlim=c(0, 10000), 
     nclass=20, ylim=c(0, 0.00035), freq=F, main = "After SSCM")
par(mfrow=c(1,1))
dev.off()


pdf(file=paste(graph_dir, "/figs/HMS-HH-C1-BeforeAdoption.pdf", sep=""), 
    width=8, height=5)
par(mfrow=c(1,2))
hist(pbrands[ever_holder==0, c1], xlab="Share", xlim=c(0, 1), nclass=20, 
     ylim=c(0, 3.5), freq=F, main = "Never Held SSCM")
hist(pbrands[ever_holder==1 & holder==0, c1], xlab="Share", xlim=c(0, 1), 
     nclass=20, ylim=c(0, 3.5), freq=F, main = "Held SSCM")
par(mfrow=c(1,1))
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-HH-C2-BeforeAdoption.pdf", sep=""), 
    width=8, height=5)
par(mfrow=c(1,2))
hist(pbrands[ever_holder==0, c2], xlab="Share", xlim=c(0, 1), nclass=20, 
     ylim=c(0, 8), freq=F, main = "Never Held SSCM")
hist(pbrands[ever_holder==1 & holder==0, c2], xlab="Share", xlim=c(0, 1), 
     nclass=20, ylim=c(0, 8), freq=F, main = "Held SSCM")
par(mfrow=c(1,1))
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-HH-C3-BeforeAdoption.pdf", sep=""), 
    width=8, height=5)
par(mfrow=c(1,2))
hist(pbrands[ever_holder==0, c3], xlab="Share", xlim=c(0, 1), nclass=20, 
     ylim=c(0, 12), freq=F, main = "Never Held SSCM")
hist(pbrands[ever_holder==1 & holder==0, c3], xlab="Share", xlim=c(0, 1), 
     nclass=20, ylim=c(0, 12), freq=F, main = "Held SSCM")
par(mfrow=c(1,1))
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-HH-C1-BAfter.pdf", sep=""), 
    width=8, height=5)
par(mfrow=c(1,2))
hist(pbrands[ever_holder==1 & holder==0, c1], xlab="Share", xlim=c(0, 1), 
     nclass=20, ylim=c(0, 2.5), freq=F, main = "Before SSCM")
hist(pbrands[ever_holder==1 & holder==1, c1], xlab="Share", xlim=c(0, 1), 
     nclass=20, ylim=c(0, 2.5), freq=F, main = "After SSCM")
par(mfrow=c(1,1))
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-HH-C2-BAfter.pdf", sep=""), 
    width=8, height=5)
par(mfrow=c(1,2))
hist(pbrands[ever_holder==1 & holder==0, c2], xlab="Share", xlim=c(0, 1), 
     nclass=20, ylim=c(0, 6.0), freq=F, main = "Before SSCM")
hist(pbrands[ever_holder==1 & holder==1, c2], xlab="Share", xlim=c(0, 1), 
     nclass=20, ylim=c(0, 6.0), freq=F, main = "After SSCM")
par(mfrow=c(1,1))
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-HH-C3-BAfter.pdf", sep=""), 
    width=8, height=5)
par(mfrow=c(1,2))
hist(pbrands[ever_holder==1 & holder==0, c3], xlab="Share", xlim=c(0, 1), 
     nclass=20, ylim=c(0, 10), freq=F, main = "Before SSCM")
hist(pbrands[ever_holder==1 & holder==1, c3], xlab="Share", xlim=c(0, 1), 
     nclass=20, ylim=c(0, 10), freq=F, main = "After SSCM")
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
treat_ctrl=treat_ctrl_panel[, .(total_brand_paid=-sum((total_price_paid-coupon_value), na.rm=T),
                                ny = ny[1]),by=c("household_code", "treat", "after", "brand_descr")]
treat_ctrl[, `:=`(nb = .N, tot_spent = sum(total_brand_paid)), 
           by = c("household_code", "treat", "after")]
setkeyv(treat_ctrl, c("household_code", "treat", "after", "total_brand_paid"))
treat_ctrl[, `:=`(share = total_brand_paid/tot_spent, 
                  cumshare=cumsum(total_brand_paid/tot_spent), rank = 1:.N), 
           by = c("household_code", "treat", "after")]
treat_ctrl[, `:=`(last_cumshare = cumshare - share)]
treat_ctrl[, ind1:=as.numeric(last_cumshare<=0.75)]
treat_ctrl=treat_ctrl[, .(topb=sum(ind1), 
                          hhi=sum(((share)*100)^2),
                          c1 = sum(share * as.integer(rank<=1)),
                          c2 = sum(share * as.integer(rank<=2)),
                          c3 = sum(share * as.integer(rank<=3)),
                          annual_spent = -tot_spent[1]/ny, 
                          brand_descr=brand_descr[1]), 
                      by = c("household_code", "treat", "after")]

# HHI Before Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-HHI-Before-Treat.pdf", sep=""), width=8, height=5)
par(mfrow=c(1,2))
hist(treat_ctrl[treat==0 & after==0, hhi], xlab="HHI", xlim=c(0, 10000), 
     nclass=20, ylim=c(0, 0.0004), freq=F, main = "Control")
hist(treat_ctrl[treat==1 & after==0, hhi], xlab="HHI", xlim=c(0, 10000), 
     nclass=20, ylim=c(0, 0.0004), freq=F, main = "Treatment")
par(mfrow=c(1,1))
dev.off()

# HHI After Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-HHI-After-Treat.pdf", sep=""), width=8, height=5)
par(mfrow=c(1,2))
hist(treat_ctrl[treat==0 & after==1, hhi], xlab="HHI", xlim=c(0, 10000), 
     nclass=20, ylim=c(0, 0.0004), freq=F, main = "Control")
hist(treat_ctrl[treat==1 & after==1, hhi], xlab="HHI", xlim=c(0, 10000), 
     nclass=20, ylim=c(0, 0.0004), freq=F, main = "Treatment")
par(mfrow=c(1,1))
dev.off()

# HHI Panel 
pdf(file=paste(graph_dir, "/figs/HMS-HHI-Panel.pdf", sep=""), width=8, height=5)
treat_ctrl[,`:=`(Status = ifelse(treat==0, "Control", "Treatment"),
                 bafter = ifelse(after==0, "Years 2008 and 2009", "Years 2012 and 2013"))]
ggplot(treat_ctrl[, .(hhi, Status, bafter)], aes(hhi, fill = Status)) + labs(x = "HHI") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=200, position = 'identity')+
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + facet_grid(~bafter)
dev.off()


# Spending Before Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-Spending-Before-Treat.pdf", sep=""), width=8, height=5)
par(mfrow=c(1,2))
hist(treat_ctrl[treat==0 & after==0 & annual_spent<=500, annual_spent], xlab="Annual Spending", 
     xlim=c(0, 500), nclass=25, ylim=c(0, 0.012), freq=F, main = "Control")
abline(v=median(treat_ctrl[treat==0 & after==0, annual_spent]), col = "red")
hist(treat_ctrl[treat==1 & after==0 & annual_spent<=500, annual_spent], xlab="Annual Spending", 
     xlim=c(0, 500), nclass=25, ylim=c(0, 0.012), freq=F, main = "Treat")
abline(v=median(treat_ctrl[treat==1 & after==0, annual_spent]), col = "red")
par(mfrow=c(1,1))
dev.off()

# Spending After Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-Spending-After-Treat.pdf", sep=""), width=8, height=5)
par(mfrow=c(1,2))
hist(treat_ctrl[treat==0 & after==1 & annual_spent<=500, annual_spent], xlab="Annual Spending", 
     xlim=c(0, 500), nclass=25, ylim=c(0, 0.012), freq=F, main = "Control")
abline(v=median(treat_ctrl[treat==0 & after==1, annual_spent]), col = "red")
hist(treat_ctrl[treat==1 & after==1 & annual_spent<=500, annual_spent], xlab="Annual Spending", 
     xlim=c(0, 500), nclass=25, ylim=c(0, 0.012), freq=F, main = "Treat")
abline(v=median(treat_ctrl[treat==1 & after==1, annual_spent]), col = "red")
par(mfrow=c(1,1))
dev.off()

# Spending B/After Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-Spending-BAfter.pdf", sep=""), width=8, height=5)
par(mfrow=c(1,2))
hist(treat_ctrl[treat==1 & after==0 & annual_spent<=500, annual_spent], xlab="Annual Spending", 
     xlim=c(0, 500), nclass=25, ylim=c(0, 0.012), freq=F, main = "Before Adoption")
abline(v=median(treat_ctrl[treat==1 & after==0, annual_spent]), col = "red")
hist(treat_ctrl[treat==1 & after==1 & annual_spent<=500, annual_spent], xlab="Annual Spending", 
     xlim=c(0, 500), nclass=25, ylim=c(0, 0.012), freq=F, main = "After Adoption")
abline(v=median(treat_ctrl[treat==1 & after==1, annual_spent]), col = "red")
par(mfrow=c(1,1))
dev.off()

# Spending After Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-Spending-Panel.pdf", sep=""), width=8, height=5)
treat_ctrl[,`:=`(Status = ifelse(treat==0, "Control", "Treatment"),
                 bafter = ifelse(after==0, "Years 2008 and 2009", "Years 2012 and 2013"))]
md00 = round(median(treat_ctrl[annual_spent<=500 & treat==0 & after==0, annual_spent]), 2)
md10 = round(median(treat_ctrl[annual_spent<=500 & treat==1 & after==0, annual_spent]), 2)
md01 = round(median(treat_ctrl[annual_spent<=500 & treat==0 & after==1, annual_spent]), 2)
md11 = round(median(treat_ctrl[annual_spent<=500 & treat==1 & after==1, annual_spent]), 2)
ggplot(treat_ctrl[annual_spent<=500, .(annual_spent, Status, bafter)], 
       aes(annual_spent, fill = Status)) + labs(x = "Annual Spending") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=10, position = 'identity') +
  annotate("text", x = 300, y = 0.004, 
           label = c(paste("Control median:", md00), paste("Control median:", md01)), 
           hjust=0, size=2.5) +
  annotate("text", x = 300, y = 0.0035, 
           label = c(paste("Treatment median:", md10), paste("Treatment median:", md11)), 
           hjust=0, size=2.5) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + facet_grid(~bafter)
par(mfrow=c(1,1))
dev.off()
#----------------------------------------------------------------------------------------------#
# Temporal Switching
setkey(treat_ctrl_panel, household_code, treat, after, purchase_date, trip_code_uc)
T_treat_ctrl = treat_ctrl_panel[, .(br_list = paste0(unique(brand_descr), collapse=" ")),
                    by = c("household_code", "treat", "after", "purchase_date", "trip_code_uc")]
T_treat_ctrl[, br_list_lag := c(NA, br_list[1:(length(br_list)-1)]), 
             by = c("household_code", "treat", "after")]
T_treat_ctrl = T_treat_ctrl[!is.na(br_list_lag), ]
T_treat_ctrl[, indicator := as.integer(grepl(br_list, br_list_lag)), 
             by = c("household_code", "treat", "after", "trip_code_uc")]
T_treat_ctrl = T_treat_ctrl[, .(Percent=mean(indicator)), 
                            by = c("household_code", "treat", "after")]

# Percentage of trips that buys a subset of brands of the last trip.
pdf(file=paste(graph_dir, "/figs/HMS-Temporal-Switching.pdf", sep=""), width=8, height=5)
T_treat_ctrl[,`:=`(Status = ifelse(treat==0, "Control", "Treatment"),
                   bafter = ifelse(after==0, "Years 2008 and 2009", "Years 2012 and 2013"))]
md00 = round(median(T_treat_ctrl[treat==0 & after==0, Percent]), 2)
md10 = round(median(T_treat_ctrl[treat==1 & after==0, Percent]), 2)
md01 = round(median(T_treat_ctrl[treat==0 & after==1, Percent]), 2)
md11 = round(median(T_treat_ctrl[treat==1 & after==1, Percent]), 2)
ggplot(T_treat_ctrl[, .(Percent, Status, bafter)], aes(Percent, fill = Status)) + 
  labs(x = "Percent of Trips Choosing a Subset of Brands of Last Trip") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=0.05, position = 'identity') +
  annotate("text", x = 0.5, y = 3.5, 
           label = c(paste("Control median:", md00),paste("Control median:", md01)), 
           hjust=0, size = 3) +
  annotate("text", x = 0.5, y = 3.25, 
           label = c(paste("Treatment median:", md10), paste("Treatment median:", md11)), 
           hjust=0, size = 3) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) + facet_grid(~bafter)
dev.off()

#----------------------------------------------------------------------------------------------#
# Discrete Choice Assumption
# Generate different description 
purchases[, `:=`(b_fr_descr = .GRP), 
          by = c("brand_descr", "ptype", "flavor_descr", "style_descr")]
purchases[, `:=`(b_fr = .GRP), by = c("brand_descr", "ptype", "flavored", "roast")]
purchases[, `:=`(b_type = .GRP), by = c("brand_descr", "ptype")]

nbrands = purchases[product_module_code==1463, 
                    .(ntype = length(unique(ptype)),
                      nb = length(unique(brand_descr)),
                      nbtype = length(unique(b_type)),
                      nbfr = length(unique(b_fr)),
                      nbfrd = length(unique(b_fr_descr)),
                      nupc = length(unique(upc)),
                      quantity = sum(quantity),
                      tot_units = sum(quantity*size1_amount),
                      ktrip = any(ptype == "KEURIG")),
                    by = c("household_code", "ever_holder", "holder", "trip_code_uc")]

# Discrete Choice of Brands Before and After Adoption
nbrands[ever_holder==1, Status:=ifelse(holder==0, "Before Adoption", "After Adoption")]
plot_nbrands =  nbrands[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "nb")]
plot_nbrands[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbrands[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoiceBrand-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbrands[nb<=5, ], aes(x=nb, y=Percent, fill=Status)) + 
  labs(x = "Number of Brands") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

# Discrete Choice of Products (unique UPCs) Before and After Adoption
plot_nbrands =  nbrands[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "nupc")]
plot_nbrands[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbrands[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoiceUPC-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbrands[nupc<=5, ], aes(x=nupc, y=Percent, fill=Status)) + 
  labs(x = "Number of UPCs Purchased") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

# Discrete Choice of Quantity Before and After Adoption
plot_nbrands =  nbrands[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "quantity")]
plot_nbrands[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbrands[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoiceQuantity-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbrands[quantity<=5, ], aes(x=quantity, y=Percent, fill=Status)) + 
  labs(x = "Number of Units Purchased") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

# Discrete Choice of Type (Keurig/Other) Before and After Adoption
plot_nbrands =  nbrands[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "ntype")]
plot_nbrands[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbrands[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoiceType-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbrands, aes(x=ntype, y=Percent, fill=Status)) + 
  labs(x = "Types Purchased") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 2.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

# Discrete Choice of Brand and Type (Keurig/Other) Before and After Adoption
plot_nbrands =  nbrands[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "nbtype")]
plot_nbrands[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbrands[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoicenBType-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbrands, aes(x=nbtype, y=Percent, fill=Status)) + 
  labs(x = "Types Purchased") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

# Discrete Choice of Brand Type and Flavor Before and After Adoption
plot_nbrands =  nbrands[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "nbfr")]
plot_nbrands[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbrands[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoicenBFlavor-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbrands, aes(x=nbfr, y=Percent, fill=Status)) + 
  labs(x = "Types Purchased") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

# Discrete Choice of Brand Type and Flavor Description Before and After Adoption
plot_nbrands =  nbrands[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "nbfrd")]
plot_nbrands[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbrands[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoicenBFlavorDescr-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbrands, aes(x=nbfrd, y=Percent, fill=Status)) + 
  labs(x = "Types Purchased") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoice-EverAdopt.pdf", sep=""), width=8, height=5)
nbrands[holder==0, Status:=ifelse(ever_holder==0, "Never Adopted", "Before Adoption")]
plot_nbrands =  nbrands[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "nb")]
plot_nbrands[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbrands[, Status:=factor(Status, c("Never Adopted", "Before Adoption"))]
ggplot(data=plot_nbrands[nb<=3, ], aes(x=nb, y=Percent, fill=Status)) + 
  labs(x = "Number of Brands") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()
#----------------------------------------------------------------------------------------------#
# Quantity Choice Decision
vsize = nbrands[, .(covar = sd(tot_units)/mean(tot_units), ntrip = .N),
                by = c("household_code", "ever_holder", "holder")]
vsize = vsize[ntrip>=5, ]

pdf(file=paste(graph_dir, "/figs/HMS-CoefVariation-BAfter.pdf", sep=""), width=8, height=5)
vsize_adoption = vsize[ever_holder==1,]
vsize_adoption[,`:=`(Status = ifelse(holder==0, 'Before Adoption', 'After Adoption'))]
md0 = round(median(vsize_adoption[holder==0, covar]), 3)
md1 = round(median(vsize_adoption[holder==1, covar]), 3)
ggplot(vsize_adoption[covar<=2, .(covar, Status)], aes(covar, fill = Status)) + 
  labs(x = "Coefficient of Variation") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=0.05, position = 'identity')+
  theme(legend.justification=c(0,1), legend.position=c(0,1))+
  annotate("text", x = 1.5, y = 0.5, label = paste("Before Adoption:", md0), hjust=0, size = 3)+
  annotate("text", x = 1.5, y = 0.4, label = paste("After Adoption:", md1), hjust=0, size = 3)
dev.off()

#----------------------------------------------------------------------------------------------#
#Let's look at consumption of household carrying the machine
holders_purchases = purchases[holder==1, ]
holders_purchases = holders_purchases[ptype=="KEURIG"&product_module_code!=7755]
#Fist look at multihoming behavior (Sort/Who have tried two machines at least)

#Now look at pattern after obtaining the machine/first recorded holder status
#Ignore multihoming for now.
#I need to account for projection factor to use it as weights
#The issue is that a household's projection factor could change from year to year.
#1. Within 90 days, number of portion packs, number of upcs, and number of brands purchased
#2. For each consecutive 90 days period, record the same thing.
TimeTrendPlot(holders_purchases, min.period=12, days=30, cond=FALSE, platform="KEURIG")

#Repeat excercise conditional on existence for at least 12 periods
TimeTrendPlot(holders_purchases, min.period=12, days=30, cond=TRUE, platform="KEURIG")

#----------------------------------------------------------------------------------------------#
#Now, let's summarize the variety of products being purchased by month.
setkey(holders_purchases, household_code, panel_year)
setkey(hh, household_code, panel_year)
holders_purchases = holders_purchases[hh[,.(household_code, panel_year, projection_factor)], 
                                      nomatch=0L]
PlatformTrend(holders_purchases)

#----------------------------------------------------------------------------------------------#
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
#----------------------------------------------------------------------------------------------------#
#Look at some purchase history
setkeyv(purchases, c("upc", "upc_ver_uc"))
setkeyv(products, c("upc", "upc_ver_uc"))

purchases_brands=purchases[products[, .(upc, upc_ver_uc, brand_descr)], nomatch=0L]

# Construct a weighted share measure by Market.

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

