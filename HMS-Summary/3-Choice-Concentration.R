# Load Auxiliary Data Sets 
load(paste(output_dir, "/HMS-Summary-Auxiliary.RData", sep=""))

# Plot holders by year
holders_year = unique(purchases[holder==TRUE, .(household_code, panel_year)])
holders_year = holders_year[, .(n_hh=.N), by = .(panel_year)]
setkeyv(holders_year, "panel_year")
pdf(file=paste(graph_dir, "/figs/holders_by_year.pdf", sep=""), width=8, height=5)
plot(holders_year[, panel_year], holders_year[, log10(n_hh)], type="o",
     xlab = "Year", ylab="Holders (log 10)")
dev.off()

# Number of Brands Purchased !!! Remains to be worked out!!!
nbr_flavors = purchases[product_module_code==1463]
nbr_flavors[, np:= .N, by = c("household_code", "ever_holder", "holder")]
nbr_flavors=pbr_flavors[np>=5, .(total_brand_paid=-sum((total_price_paid - coupon_value), na.rm=T)),
                        by = c("household_code", "ever_holder", "holder", "brand_descr", "flavored", "roast")]
nbr_flavors[, `:=`(nb = .N, tot_spent = sum(total_brand_paid)), 
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
pbr_flavors[,`:=`(Status = ifelse(ever_holder==0, 'Never Adopted', 'Before Adoption'))]
pbr_flavors$Status <- factor(pbr_flavors$Status, levels = c("Before Adoption", "Never Adopted"))

pdf(file=paste(graph_dir, "/figs/HMS-Flavor-HHI-BeforeAdoption.pdf", sep=""), width=8, height=5)
ggplot(pbr_flavors[holder==0, .(hhi, Status)], aes(x=hhi))+
  geom_histogram(alpha=1, center=200, aes(y = ..density..), binwidth=400, col="grey30", fill="skyblue")+
  scale_x_continuous("HHI", limits = c(0, 10000)) + theme_minimal() + facet_wrap(~ Status)
dev.off()

pbr_flavors[,`:=`(Status = ifelse(holder==0, 'Before Adoption', 'After Adoption'))]
pbr_flavors$Status <- factor(pbr_flavors$Status, levels = c("Before Adoption", "After Adoption"))
pdf(file=paste(graph_dir, "/figs/HMS-Flavor-HHI-HH-BAfter.pdf", sep=""), width=8, height=5)
ggplot(pbr_flavors[ever_holder==1, .(hhi, Status)], aes(x=hhi))+
  geom_histogram(alpha=1, center=200, aes(y = ..density..), binwidth=400, col="grey30", fill="skyblue")+
  scale_x_continuous("HHI", limits = c(0, 10000)) + theme_minimal() + facet_wrap(~ Status)
dev.off()

# HHI Panel
pbr_temp = copy(pbr_flavors)
pbr_temp[, `:=`(Status = factor(ifelse(ever_holder==0, "Never Adopted", "Adopters"),
                                levels = c("Never Adopted", "Adopters")),
                Type = ifelse(holder==0, "Before Adoption", "After Adoption"))]
pbr_temp[holder==0 & ever_holder==0, Type := ""]
pbr_temp[, Type := factor(Type, levels = c("","Before Adoption", "After Adoption"))]

pdf(file=paste(graph_dir, "/figs/HMS-Flavor-HHI-BAPanel.pdf", sep=""), width=8, height=5)
ggplot(pbr_temp, aes(x=hhi))+scale_x_continuous("HHI", limits = c(0, 10000)) + theme_minimal() +
  geom_histogram(alpha=1, center=200, aes(y = ..density..), binwidth=400, col="grey30", fill="skyblue")+
  facet_wrap( ~ Status + Type)
dev.off()
rm(pbr_temp)

# Concentration Ratio
pbr_temp = pbr_flavors[ever_holder==1, .(household_code, holder, c1, c2, c3)]
pbr_temp = melt(pbr_temp, id.vars = c("household_code", "holder"), 
                measure.vars = c("c1", "c2", "c3"), value.name = "Share")
pbr_temp[, `:=`(Status = factor(ifelse(holder==0, "Before Adoption", "After Adoption"),
                                levels=c("Before Adoption", "After Adoption")),
                Type = toupper(variable))]

pdf(file=paste(graph_dir, "/figs/HMS-Flavor-CR-BAdoption.pdf", sep=""), width=8, height=12)
ggplot(pbr_temp, aes(x=Share))+ theme_minimal() +
  geom_histogram(alpha=1, center=0.025, aes(y = ..density..), binwidth=0.05, col="grey30", fill="skyblue")+
  scale_x_continuous("Expenditure Share", limits = c(0, 1)) + facet_grid(Type~Status)
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
pbrands[, Status := factor(ifelse(ever_holder==0, 'Never Adopted', 'Before Adoption'),
                              levels = c("Never Adopted", "Before Adoption"))]

pdf(file=paste(graph_dir, "/figs/HMS-HHI-BeforeAdoption.pdf", sep=""), width=8, height=5)
ggplot(pbrands[holder==0, .(hhi, Status)], aes(x=hhi))+
  geom_histogram(alpha=1, center=200, aes(y = ..density..), binwidth=400, col="grey30", fill="skyblue")+
  scale_x_continuous("HHI", limits = c(0, 10000)) + theme_minimal() + facet_wrap(~ Status)
dev.off()

# HHI Before and After Adoption
pbrands[,`:=`(Status = factor(ifelse(holder==0, 'Before Adoption', 'After Adoption'), 
                              levels = c("Before Adoption", "After Adoption")))]
pdf(file=paste(graph_dir, "/figs/HMS-HHI-HH-BAfter.pdf", sep=""), width=8, height=5)
ggplot(pbrands[ever_holder==1, .(hhi, Status)], aes(x=hhi))+
  geom_histogram(alpha=1, center=200, aes(y = ..density..), binwidth=400, col="grey30", fill="skyblue")+
  scale_x_continuous("HHI", limits = c(0, 10000)) + theme_minimal() + facet_wrap(~ Status)
dev.off()

# HHI Panel
pbr_temp = copy(pbrands)
pbr_temp[, `:=`(Status = factor(ifelse(ever_holder==0, "Never Adopted", "Adopters"),
                                levels = c("Never Adopted", "Adopters")),
                Type = ifelse(holder==0, "Before Adoption", "After Adoption"))]
pbr_temp[holder==0 & ever_holder==0, Type := ""]
pbr_temp[, Type := factor(Type, levels = c("","Before Adoption", "After Adoption"))]

pdf(file=paste(graph_dir, "/figs/HMS-HHI-BAPanel.pdf", sep=""), width=8, height=5)
ggplot(pbr_temp, aes(x=hhi))+
  geom_histogram(alpha=1, center=200, aes(y = ..density..), binwidth=400, col="grey30", fill="skyblue")+
  scale_x_continuous("HHI", limits = c(0, 10000)) + theme_minimal() + facet_wrap( ~ Status + Type)
dev.off()
rm(pbr_temp)


# Concentration Ratio
pbr_temp = pbrands[ever_holder==1, .(household_code, holder, c1, c2, c3)]
pbr_temp = melt(pbr_temp, id.vars = c("household_code", "holder"), 
                measure.vars = c("c1", "c2", "c3"), value.name = "Share")
pbr_temp[, `:=`(Status = factor(ifelse(holder==0, "Before Adoption - Ground Coffee", "After Adoption - Ground and K-Cup"),
                                levels=c("Before Adoption - Ground Coffee", "After Adoption - Ground and K-Cup")),
                Type = toupper(variable))]
pbr_temp[, fillvar := ifelse(Status=="Before Adoption - Ground Coffee", "skyblue", "hotpink")]
pdf(file=paste(graph_dir, "/figs/HMS-CR-BAdoption.pdf", sep=""), width=6, height=4)
ggplot(pbr_temp, aes(x=Share))+ theme_minimal() +
  geom_histogram(alpha=1, center=0.025, aes(y = ..density..), binwidth=0.05, col="grey30", fill="skyblue")+
  scale_x_continuous("Expenditure Share", limits = c(0, 1)) + facet_grid(Type~Status)
dev.off()

pbr_temp[, `:=`(cmed = median(Share,na.rm=T)), by = c("Type", "Status")]
pdf(file=paste(graph_dir, "/figs/HMS-C1-BAdoption.pdf", sep=""), width=6, height=4)
ggplot(pbr_temp[Type=="C1", ], aes(x=Share, fill=Status))+ theme_minimal() +
  geom_histogram(alpha=1, center=0.025, aes(y = ..density..), binwidth=0.05, col="grey30")+
  scale_x_continuous("Expenditure Share of the Most Purchased Brand", limits = c(0, 1)) + 
  facet_grid(~Status) + geom_vline(aes(xintercept = cmed), col="red")+
  scale_fill_manual(values=c("skyblue", "hotpink"), guide=F)
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-C2-BAdoption.pdf", sep=""), width=6, height=4)
ggplot(pbr_temp[Type=="C2", ], aes(x=Share, fill=Status))+ theme_minimal() +
  geom_histogram(alpha=1, center=0.025, aes(y = ..density..), binwidth=0.05, col="grey30")+
  scale_x_continuous("Expenditure Share of the Top Two Purchased Brands", limits = c(0, 1)) +
  facet_grid(~Status) + geom_vline(aes(xintercept = cmed), col="red")+
  scale_fill_manual(values=c("skyblue", "hotpink"), guide=F)
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-C3-BAdoption.pdf", sep=""), width=6, height=4)
ggplot(pbr_temp[Type=="C3", ], aes(x=Share, fill=Status))+ theme_minimal() +
  geom_histogram(alpha=1, center=0.025, aes(y = ..density..), binwidth=0.05, col="grey30")+
  scale_x_continuous("Expenditure Share of the Top Three Purchased Brands", limits = c(0, 1)) + 
  facet_grid(~Status)+ geom_vline(aes(xintercept = cmed), col="red")+
  scale_fill_manual(values=c("skyblue", "hotpink"), guide=F)
dev.off()
rm(pbr_temp)

# HHI Two Years After Adoption versus Before Adoption
purchases[purchase_date<first_date, two_year_status:=0]
purchases[purchase_date>=first_date&purchase_date<(first_date+730), two_year_status:=1]
purchases[purchase_date>=(first_date+730), two_year_status:=2]
pbrands_learn = purchases[product_module_code==1463]
pbrands_learn[, np:= .N, by = c("household_code", "ever_holder", "two_year_status")]
pbrands_learn=pbrands_learn[np>=5, .(total_brand_paid=-sum((total_price_paid - coupon_value), na.rm=T)),
                by = c("household_code", "ever_holder", "two_year_status", "brand_descr")]
pbrands_learn[, `:=`(nb = .N, tot_spent = sum(total_brand_paid)), 
        by = c("household_code", "ever_holder", "two_year_status")]
setkeyv(pbrands_learn, c("household_code", "ever_holder", "two_year_status", "total_brand_paid"))
pbrands_learn[, `:=`(share = total_brand_paid/tot_spent, cumshare = 
                 cumsum(total_brand_paid/tot_spent), rank = 1:.N), 
        by = c("household_code", "ever_holder", "two_year_status")]
pbrands_learn[, `:=`(last_cumshare = cumshare - share)]
pbrands_learn[, ind1:=as.numeric(last_cumshare<=0.75)]
pbrands_learn=pbrands_learn[, .(topb=sum(ind1), 
                    hhi=sum(((share)*100)^2),
                    c1 = sum(share * as.integer(rank<=1)),
                    c2 = sum(share * as.integer(rank<=2)),
                    c3 = sum(share * as.integer(rank<=3)),
                    brand_descr=brand_descr[1]), 
                by = c("household_code", "ever_holder", "two_year_status", "nb")]
pbrands_learn[,`:=`(Status = factor(ifelse(two_year_status==0, 'Before Adoption', 
                                           ifelse(two_year_status==1, "Within 2 Years of Adoption", 
                                                  '2 Years after Adoption')), 
                              levels = c("Before Adoption", "Within 2 Years of Adoption", 
                                         "2 Years after Adoption")))]

pdf(file=paste(graph_dir, "/figs/HMS-HHI-HH-BALearn.pdf", sep=""), width=6, height=4)
ggplot(pbrands_learn[ever_holder==1, .(hhi, Status)], aes(x=hhi, fill=Status))+
  geom_histogram(alpha=1, center=200, aes(y = ..density..), binwidth=400, col="grey30")+
  scale_x_continuous("HHI", limits = c(0, 10000)) + theme_minimal() + facet_wrap(~ Status)+
  scale_fill_manual(values=c("skyblue", "hotpink", "red"), guide=F)
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-C1-HH-BALearn.pdf", sep=""), width=6, height=4)
ggplot(pbrands_learn[ever_holder==1, .(c1, Status)], aes(x=c1, fill=Status))+ theme_minimal() +
  geom_histogram(alpha=1, center=0.025, aes(y = ..density..), binwidth=0.05, col="grey30")+
  scale_x_continuous("Expenditure Share of the Top Purchased Brand", limits = c(0, 1)) +
  facet_grid(~Status)+scale_fill_manual(values=c("skyblue", "hotpink", "red"), guide=F)
dev.off()

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
treat_ctrl[, `:=`(treat_labels = factor(ifelse(treat==0, "Reference Group", "Adoption Group"),
                                    levels = c("Reference Group", "Adoption Group")),
                  bafter = ifelse(after==0, "Years 2008 and 2009, Before Adoption", "Years 2012 and 2013, After Adoption"))]

# HHI Before Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-HHI-Before-Treat.pdf", sep=""), width=8, height=5)
ggplot(treat_ctrl[after==0,], aes(x=hhi))+theme_minimal()+
  geom_histogram(alpha=1, center=200, aes(y = ..density..), binwidth=400, col="grey30", fill="skyblue")+
  scale_x_continuous("HHI", limits = c(0, 10000)) + facet_wrap(~ treat_labels)
dev.off()

# HHI After Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-HHI-After-Treat.pdf", sep=""), width=8, height=5)
ggplot(treat_ctrl[after==1,], aes(x=hhi))+theme_minimal()+
  geom_histogram(alpha=1, center=200, aes(y = ..density..), binwidth=400, col="grey30", fill="skyblue")+
  scale_x_continuous("HHI", limits = c(0, 10000)) + facet_wrap(~ treat_labels)
dev.off()

# HHI Panel 
pdf(file=paste(graph_dir, "/figs/HMS-HHI-Panel.pdf", sep=""), width=8, height=12)
ggplot(treat_ctrl, aes(x=hhi))+theme_minimal()+
  geom_histogram(alpha=1, center=200, aes(y = ..density..), binwidth=400, col="grey30", fill="skyblue")+
  scale_x_continuous("HHI", limits = c(0, 10000)) + facet_grid(treat_labels ~ bafter)
dev.off()

# Spending Before Adoption 
treat_ctrl[annual_spent<=500, med:=median(annual_spent), by = c("treat", "after")]
pdf(file=paste(graph_dir, "/figs/HMS-Spending-Before-Treat.pdf", sep=""), width=6, height=4)
ggplot(treat_ctrl[after==0 & annual_spent<=500, ], aes(x=annual_spent, fill = factor(treat)))+
  geom_histogram(alpha=1, center=-10, aes(y = ..density..), binwidth=20, col="grey30")+
  geom_vline(aes(xintercept = med), col="red")+theme_minimal()+
  scale_x_continuous("Annual Spending", limits = c(0, 500)) + facet_wrap(~ treat_labels)+
  scale_fill_manual(values=c("skyblue", "hotpink"), guide=F)
dev.off()

# Spending After Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-Spending-After-Treat.pdf", sep=""), width=6, height=4)
ggplot(treat_ctrl[after==1 & annual_spent<=500, ], aes(x=annual_spent, fill = factor(treat)))+
  geom_histogram(alpha=1, center=-10, aes(y = ..density..), binwidth=20, col="grey30")+
  geom_vline(aes(xintercept = med), col="red")+theme_minimal()+
  scale_x_continuous("Annual Spending", limits = c(0, 500)) + facet_wrap(~ treat_labels)+
  scale_fill_manual(values=c("skyblue", "hotpink"), guide=F)
dev.off()

# Spending B/After Adoption 
pdf(file=paste(graph_dir, "/figs/HMS-Spending-BAfter.pdf", sep=""), width=6, height=4)
ggplot(treat_ctrl[treat==1 & annual_spent<=500, ], aes(x=annual_spent, fill = factor(after)))+
  geom_histogram(alpha=1, center=-10, aes(y = ..density..), binwidth=20, col="grey30")+
  geom_vline(aes(xintercept = med), col="red")+theme_minimal()+
  scale_x_continuous("Annual Spending", limits = c(0, 500)) + facet_wrap(~ treat_labels)+
  scale_fill_manual(values=c("skyblue", "hotpink"), guide=F)
dev.off()

# Spending panel 
pdf(file=paste(graph_dir, "/figs/HMS-Spending-Panel-Before.pdf", sep=""), width=3.375, height=4.5)
ggplot(treat_ctrl[annual_spent<=500 & after==0, ], aes(x = annual_spent, fill = factor(interaction(treat, after)))) + 
  geom_histogram(alpha=1, center=10, aes(y = ..density..), binwidth=20, col="grey30")+
  scale_x_continuous("Annual Spending", limits = c(0, 500)) + geom_vline(aes(xintercept = med), col="red")+
  theme_minimal() + facet_grid(treat_labels ~ bafter) + 
  scale_fill_manual(values=c("skyblue", "skyblue", "hotpink", "hotpink"), guide=F)
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-Spending-Panel.pdf", sep=""), width=6.75, height=4.5)
ggplot(treat_ctrl[annual_spent<=500, ], aes(x = annual_spent, fill = factor(interaction(treat, after)))) + 
  geom_histogram(alpha=1, center=10, aes(y = ..density..), binwidth=20, col="grey30")+
  scale_x_continuous("Annual Spending", limits = c(0, 500)) + geom_vline(aes(xintercept = med), col="red")+
  theme_minimal() + facet_grid(treat_labels ~ bafter) + 
  scale_fill_manual(values=c("skyblue", "skyblue", "hotpink", "hotpink"), guide=F)
dev.off()
#----------------------------------------------------------------------------------------------#
