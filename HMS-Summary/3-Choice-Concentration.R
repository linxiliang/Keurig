# Load Auxiliary Data Sets 
load(paste(output_dir, "/HMS-Summary-Auxiliary.RData", sep=""))

# Plot holders by year
holders_year = unique(purchases[holder==TRUE, .(household_code, panel_year)])
holders_year = holders_year[, .(n_hh=.N), by = .(panel_year)]
setkeyv(holders_year, "panel_year")
pdf(file=paste(graph_dir, "/figs/holders_by_year.pdf", sep=""), width=7, height=5)
plot(holders_year[, panel_year], holders_year[, log10(n_hh)], type="o",
     xlab = "Year", ylab="Holders (log 10)")
dev.off()

# Concentration by Flavor and Brands 
pbr_flavors = purchases[product_module_code==1463]
pbr_flavors[, np:= .N, by = c("household_code", "ever_holder", "holder")]
pbr_flavors=pbr_flavors[np>=5, .(total_brand_paid=-sum((total_price_paid - coupon_value), na.rm=T)),
                        by = c("household_code", "ever_holder", "holder", "brand_descr", "flavored", "roast")]
pbr_flavors[, `:=`(nb = .N, tot_spent = sum(total_brand_paid)), 
            by = c("household_code", "ever_holder", "holder")]
setkeyv(pbr_flavors, c("household_code", "ever_holder", "holder", "total_brand_paid"))
pbr_flavors[, `:=`(share = total_brand_paid/tot_spent, rank = 1:.N, 
                   cumshare = cumsum(total_brand_paid/tot_spent)), 
            by = c("household_code", "ever_holder", "holder")]
pbr_flavors[, `:=`(last_cumshare = cumshare - share)]
pbr_flavors[, ind1:=as.numeric(last_cumshare<=0.75)]
pbr_flavors=pbr_flavors[, .(topb=sum(ind1), 
                            hhi=sum(((share)*100)^2),
                            c1 = sum(share * as.integer(rank<=1)),
                            c2 = sum(share * as.integer(rank<=2)),
                            c3 = sum(share * as.integer(rank<=3)),
                            brand_descr=brand_descr[1]), 
                        by = c("household_code", "ever_holder", "holder", "nb")]

# HHI Before Adoption
pdf(file=paste(graph_dir, "/figs/HMS-Flavor-HHI-BeforeAdoption.pdf", sep=""), width=8, height=5)
pbr_flavors[,`:=`(Status = ifelse(ever_holder==0, 'Never Adopted', 'Before Adoption'))]
ggplot(pbr_flavors[holder==0, .(hhi, Status)], aes(hhi, fill = Status)) + labs(x = "HHI") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=400, position = 'identity')+
  theme(legend.justification=c(0,1), legend.position=c(0,1))
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-Flavor-HHI-HH-BAfter.pdf", sep=""), width=8, height=5)
pbr_flavors[,`:=`(Status = ifelse(holder==0, 'Before', 'After'))]
ggplot(pbr_flavors[ever_holder==1, .(hhi, Status)], aes(hhi, fill = Status)) + labs(x = "HHI") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=400, position = 'identity')+
  theme(legend.justification=c(0,1), legend.position=c(0,1))
dev.off()

# HHI Panel
pbr_temp = copy(pbr_flavors)
pbr_temp[, `:=`(Status = factor(ifelse(ever_holder==0, "Ground Purchases", 
                                       "Before and After Adoption"),
                                levels = c("Ground Purchases", "Before and After Adoption")),
                Type = factor(ifelse(holder==0&ever_holder==0, "Never Adopted", 
                                     ifelse(holder==0, "Before Adoption", "After Adoption")),
                              levels = c("Never Adopted", "Before Adoption", "After Adoption")))]
pbr_temp2=pbr_temp[ever_holder==1&holder==0, ]
pbr_temp2[, Status := "Ground Purchases"]
pbr_temp = rbindlist(list(pbr_temp, pbr_temp2))

pdf(file=paste(graph_dir, "/figs/HMS-Flavor-HHI-BAPanel.pdf", sep=""), width=8, height=5)
ggplot(pbr_temp, aes(hhi, fill = Type)) + labs(x = "HHI") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=250, position = 'identity') +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + facet_grid(~Status)
dev.off()
rm(pbr_temp, pbr_temp2)

# Concentration Ratio
pbr_temp = pbr_flavors[ever_holder==1, .(household_code, holder, c1, c2, c3)]
pbr_temp = melt(pbr_temp, id.vars = c("household_code", "holder"), 
                measure.vars = c("c1", "c2", "c3"), value.name = "Share")
pbr_temp[, `:=`(Status = factor(ifelse(holder==0, "Before Adoption", "After Adoption"),
                                levels=c("Before Adoption", "After Adoption")),
                Type = toupper(variable))]

pdf(file=paste(graph_dir, "/figs/HMS-Flavor-CR-BAdoption.pdf", sep=""), 
    width=8, height=5)
ggplot(pbr_temp, aes(Share, fill = Status)) + labs(x = "Expenditure Share") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=0.05, position = 'identity') +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + facet_grid(~Type)
dev.off()
rm(pbr_temp)


# Same analysis as above for brands
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
pdf(file=paste(graph_dir, "/figs/HMS-HHI-BeforeAdoption.pdf", sep=""), width=8, height=5)
pbrands[,`:=`(Status = ifelse(ever_holder==0, 'Never Adopted', 'Before Adoption'))]
ggplot(pbrands[holder==0, .(hhi, Status)], aes(hhi, fill = Status)) + labs(x = "HHI") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=400, position = 'identity')+
  theme(legend.justification=c(0,1), legend.position=c(0,1))
dev.off()

# HHI Before and After Adoption
pdf(file=paste(graph_dir, "/figs/HMS-HHI-HH-BAfter.pdf", sep=""), width=8, height=5)
pbrands[,`:=`(Status = ifelse(holder==0, 'Before', 'After'))]
ggplot(pbrands[ever_holder==1, .(hhi, Status)], aes(hhi, fill = Status)) + labs(x = "HHI") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=400, position = 'identity')+
  theme(legend.justification=c(0,1), legend.position=c(0,1))
dev.off()

# HHI Panel
pbr_temp = copy(pbrands)
pbr_temp[, `:=`(Status = factor(ifelse(ever_holder==0, "Ground Purchases", 
                                       "Before and After Adoption"),
                                levels = c("Ground Purchases", "Before and After Adoption")),
                Type = factor(ifelse(holder==0&ever_holder==0, "Never Adopted", 
                                     ifelse(holder==0, "Before Adoption", "After Adoption")),
                              levels = c("Never Adopted", "Before Adoption", "After Adoption")))]
pbr_temp2=pbr_temp[ever_holder==1&holder==0, ]
pbr_temp2[, Status := "Ground Purchases"]
pbr_temp = rbindlist(list(pbr_temp, pbr_temp2))

pdf(file=paste(graph_dir, "/figs/HMS-HHI-BAPanel.pdf", sep=""), width=8, height=5)
ggplot(pbr_temp, aes(hhi, fill = Type)) + labs(x = "HHI") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=250, position = 'identity') +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + facet_grid(~Status)
dev.off()
rm(pbr_temp, pbr_temp2)

# Concentration Ratio
pbr_temp = pbrands[ever_holder==1, .(household_code, holder, c1, c2, c3)]
pbr_temp = melt(pbr_temp, id.vars = c("household_code", "holder"), 
                measure.vars = c("c1", "c2", "c3"), value.name = "Share")
pbr_temp[, `:=`(Status = factor(ifelse(holder==0, "Before Adoption", "After Adoption"),
                                levels=c("Before Adoption", "After Adoption")),
                Type = toupper(variable))]
pdf(file=paste(graph_dir, "/figs/HMS-CR-BAdoption.pdf", sep=""), 
    width=8, height=5)
ggplot(pbr_temp, aes(Share, fill = Status)) + labs(x = "Expenditure Share") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=0.05, position = 'identity') +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + facet_grid(~Type)
dev.off()
rm(pbr_temp)

#----------------------------------------------------------------------------------------------#
# Show no time trend in variety seeking in a constructed control and treatment group
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
     nclass=20, ylim=c(0, 0.0004), freq=F, main = "Reference Group")
hist(treat_ctrl[treat==1 & after==0, hhi], xlab="HHI", xlim=c(0, 10000), 
     nclass=20, ylim=c(0, 0.0004), freq=F, main = "Adoption Group")
par(mfrow=c(1,1))
dev.off()

# HHI After Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-HHI-After-Treat.pdf", sep=""), width=8, height=5)
par(mfrow=c(1,2))
hist(treat_ctrl[treat==0 & after==1, hhi], xlab="HHI", xlim=c(0, 10000), 
     nclass=20, ylim=c(0, 0.0004), freq=F, main = "Reference Group")
hist(treat_ctrl[treat==1 & after==1, hhi], xlab="HHI", xlim=c(0, 10000), 
     nclass=20, ylim=c(0, 0.0004), freq=F, main = "Adoption Group")
par(mfrow=c(1,1))
dev.off()

# HHI Panel 
pdf(file=paste(graph_dir, "/figs/HMS-HHI-Panel.pdf", sep=""), width=8, height=5)
treat_ctrl[,`:=`(Status = ifelse(treat==0, "Reference Group", "Adoption Group"),
                 bafter = ifelse(after==0, "Years 2008 and 2009", "Years 2012 and 2013"))]
ggplot(treat_ctrl[, .(hhi, Status, bafter)], aes(hhi, fill = Status)) + labs(x = "HHI") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=200, position = 'identity')+
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + facet_grid(~bafter)
dev.off()

# Spending Before Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-Spending-Before-Treat.pdf", sep=""), width=8, height=5)
par(mfrow=c(1,2))
hist(treat_ctrl[treat==0 & after==0 & annual_spent<=500, annual_spent], xlab="Annual Spending", 
     xlim=c(0, 500), nclass=25, ylim=c(0, 0.012), freq=F, main = "Reference Group")
abline(v=median(treat_ctrl[treat==0 & after==0, annual_spent]), col = "red")
hist(treat_ctrl[treat==1 & after==0 & annual_spent<=500, annual_spent], xlab="Annual Spending", 
     xlim=c(0, 500), nclass=25, ylim=c(0, 0.012), freq=F, main = "Adoption Group")
abline(v=median(treat_ctrl[treat==1 & after==0, annual_spent]), col = "red")
par(mfrow=c(1,1))
dev.off()

# Spending After Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-Spending-After-Treat.pdf", sep=""), width=8, height=5)
par(mfrow=c(1,2))
hist(treat_ctrl[treat==0 & after==1 & annual_spent<=500, annual_spent], xlab="Annual Spending", 
     xlim=c(0, 500), nclass=25, ylim=c(0, 0.012), freq=F, main = "Reference Group")
abline(v=median(treat_ctrl[treat==0 & after==1, annual_spent]), col = "red")
hist(treat_ctrl[treat==1 & after==1 & annual_spent<=500, annual_spent], xlab="Annual Spending", 
     xlim=c(0, 500), nclass=25, ylim=c(0, 0.012), freq=F, main = "Adoption Group")
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
           label = c(paste("Reference median:", md00), paste("Reference median:", md01)), 
           hjust=0, size=2.5) +
  annotate("text", x = 300, y = 0.0035, 
           label = c(paste("Adoption median:", md10), paste("Adoption median:", md11)), 
           hjust=0, size=2.5) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + facet_grid(~bafter)
dev.off()
#----------------------------------------------------------------------------------------------#
