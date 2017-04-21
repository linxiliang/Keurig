#----------------------------------------------------------------------------------------------#
# Discrete Choice Assumption
#----------------------------------------------------------------------------------------------#

# Load Auxiliary Data Sets 
load(paste(output_dir, "/HMS-Summary-Auxiliary.RData", sep=""))

# Generate different description 
purchases[, `:=`(b_fr_descr = .GRP), 
          by = c("brand_descr", "ptype", "flavor_descr", "style_descr")]
purchases[, `:=`(b_fr = .GRP), by = c("brand_descr", "ptype", "flavored", "roast")]
purchases[, `:=`(b_type = .GRP), by = c("brand_descr", "ptype")]
purchases[, keurig := as.integer(ptype=="KEURIG")]

nbt_summary = purchases[product_module_code==1463, 
                    .(ntype = length(unique(ptype)),
                      nb = length(unique(brand_descr)),
                      nbtype = length(unique(b_type)),
                      nbfr = length(unique(b_fr)),
                      nbfrd = length(unique(b_fr_descr)),
                      nupc = length(unique(upc)),
                      quantity = sum(quantity),
                      tot_units = sum(quantity*size1_amount),
                      k_units = sum(quantity*size1_amount*keurig),
                      ktrip = any(keurig == 1),
                      aktrip = all(keurig == 1),
                      gtrip = any(keurig != 1),
                      agtrip = all(keurig != 1),
                      expend = sum(total_price_paid-coupon_value), 
                      kexpend = sum((total_price_paid-coupon_value)*keurig)),
                    by = c("household_code", "ever_holder", "holder", "trip_code_uc",
                           "purchase_date", "first_date", "last_date")]

#----------------------------------------------------------------------------------------------#
# First look at whether consumer purchase ground after purchasing Keurig Machine

# Percent of trips purchasing ground coffee and other types after adoption of Keurig
trip_percent = nbt_summary[holder==1, c(mean(aktrip), mean(agtrip))]
trip_percent = c(trip_percent, 1 - sum(trip_percent))

# Percent of units purchased that are Keurig and other after adoption of Keurig
unit_percent = nbt_summary[holder==1, sum(k_units)/sum(tot_units)]
unit_percent = c(unit_percent, 1 - sum(unit_percent), 0)

# Percent of expenditure on Keurig and other after adoption of Keurig
exp_percent = nbt_summary[holder==1, sum(kexpend)/sum(expend)]
exp_percent = c(exp_percent, 1 - sum(exp_percent), 0)

# BarPlot
percent_tab = data.table(Type=factor(c(rep("Trip",3), rep("Units",3), rep("Expenditure",3)),
                                     levels = c("Expenditure", "Units", "Trip")),
                         Percent = c(trip_percent, unit_percent, exp_percent),
                         Status = factor(rep(c("Keurig", "Other", "Both"), 3),
                                         levels = c("Both", "Other", "Keurig")))
percent_tab[, pos := cumsum(Percent) - (0.5 * Percent), by = "Type"]
percent_tab[Percent<=0.000001, pos:=NA]
pdf(file=paste(graph_dir, "/figs/HH-After-Type-Percentage.pdf", sep=""), width=8, height=5)
ggplot(percent_tab, aes(x=Type, y=Percent, fill=Status))+ theme_bw() +
  geom_bar(position = "fill", stat = "identity", width=.5) + coord_flip() +
  scale_y_continuous(labels = percent) + scale_fill_brewer() +
  geom_text(aes(label = paste0(round(Percent,3)*100,"%"), y = pos), size = 3)
dev.off()

# Constrain to first Keurig purchase date and last purchase date
trip_percent = nbt_summary[holder==1&purchase_date>=first_date&purchase_date<=last_date,
                           c(mean(aktrip), mean(agtrip))]
trip_percent = c(trip_percent, 1 - sum(trip_percent))

# Percent of units purchased that are Keurig and other after adoption of Keurig
unit_percent = nbt_summary[holder==1&purchase_date>=first_date&purchase_date<=last_date, 
                           sum(k_units)/sum(tot_units)]
unit_percent = c(unit_percent, 1 - sum(unit_percent), 0)

# Percent of expenditure on Keurig and other after adoption of Keurig
exp_percent = nbt_summary[holder==1&purchase_date>=first_date&purchase_date<=last_date, 
                          sum(kexpend)/sum(expend)]
exp_percent = c(exp_percent, 1 - sum(exp_percent), 0)

# BarPlot
percent_tab = data.table(Type=factor(c(rep("Trip",3), rep("Units",3), rep("Expenditure",3)),
                                     levels = c("Expenditure", "Units", "Trip")),
                         Percent = c(trip_percent, unit_percent, exp_percent),
                         Status = factor(rep(c("Keurig", "Other", "Both"), 3),
                                         levels = c("Both", "Other", "Keurig")))
percent_tab[, pos := cumsum(Percent) - (0.5 * Percent), by = "Type"]
percent_tab[Percent<=0.000001, pos:=NA]
pdf(file=paste(graph_dir, "/figs/HH-After-Cond-Type-Percentage.pdf", sep=""), width=8, height=5)
ggplot(percent_tab, aes(x=Type, y=Percent, fill=Status))+ theme_bw() +
  geom_bar(position = "fill",stat = "identity", width=.5) + coord_flip() +
  scale_y_continuous(labels = percent) + scale_fill_brewer() +
  geom_text(aes(label = paste0(round(Percent,3)*100,"%"), y = pos), size = 3)
dev.off()

# Quantity Choice Decision
vsize = nbt_summary[, .(covar = sd(tot_units)/mean(tot_units), ntrip = .N),
                by = c("household_code", "ever_holder", "holder")]
vsize = vsize[ntrip>=5, ]

pdf(file=paste(graph_dir, "/figs/HMS-CoefVariation-BAfter.pdf", sep=""), width=8, height=5)
vsize_adoption = vsize[ever_holder==1,]
vsize_adoption[,`:=`(Status = ifelse(holder==0, 'Before Adoption', 'After Adoption'))]
vsize_adoption[, med := round(median(covar), 3), by = "holder"]
vsize_adoption$Status <- factor(vsize_adoption$Status,
                       levels = c("Before Adoption", "After Adoption"))
ggplot(vsize_adoption[covar<=2, .(covar, med, Status)], aes(x = covar)) + 
  geom_histogram(alpha = 1, center=0.05, aes(y = ..density..), binwidth=0.1, col="grey30", fill="skyblue")+
  scale_x_continuous("Coefficient of Variation", limits = c(0, 2)) + geom_vline(aes(xintercept = med), col="red")+
  theme_minimal() + facet_wrap(~ Status)
dev.off()

# Discrete Choice of Brands Before and After Adoption
nbt_summary[ever_holder==1, Status:=ifelse(holder==0, "Before Adoption", "After Adoption")]
plot_nbt_summary =  nbt_summary[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "nb")]
plot_nbt_summary[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbt_summary[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoiceBrand-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbt_summary[nb<=5, ], aes(x=nb, y=Percent, fill=Status)) + theme_minimal()+
  labs(x = "Number of Brands") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

# Discrete Choice of Products (unique UPCs) Before and After Adoption
plot_nbt_summary =  nbt_summary[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "nupc")]
plot_nbt_summary[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbt_summary[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoiceUPC-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbt_summary[nupc<=5, ], aes(x=nupc, y=Percent, fill=Status)) + theme_minimal()+
  labs(x = "Number of UPCs Purchased") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

# Discrete Choice of Quantity Before and After Adoption
plot_nbt_summary =  nbt_summary[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "quantity")]
plot_nbt_summary[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbt_summary[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoiceQuantity-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbt_summary[quantity<=5, ], aes(x=quantity, y=Percent, fill=Status)) + theme_minimal()+ 
  labs(x = "Number of Units Purchased") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

# Discrete Choice of Type (Keurig/Other) Before and After Adoption
plot_nbt_summary =  nbt_summary[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "ntype")]
plot_nbt_summary[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbt_summary[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoiceType-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbt_summary, aes(x=ntype, y=Percent, fill=Status)) + theme_minimal()+
  labs(x = "Types Purchased") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 2.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

# Discrete Choice of Brand and Type (Keurig/Other) Before and After Adoption
plot_nbt_summary =  nbt_summary[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "nbtype")]
plot_nbt_summary[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbt_summary[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoicenBType-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbt_summary, aes(x=nbtype, y=Percent, fill=Status)) + theme_minimal()+
  labs(x = "Number of Brands Purchased") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

# Discrete Choice of Brand Type and Flavor Before and After Adoption
plot_nbt_summary =  nbt_summary[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "nbfr")]
plot_nbt_summary[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbt_summary[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoicenBFlavor-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbt_summary, aes(x=nbfr, y=Percent, fill=Status)) + theme_minimal()+
  labs(x = "Brand and Flavors Purchased") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

# Discrete Choice of Brand Type and Flavor Description Before and After Adoption
plot_nbt_summary =  nbt_summary[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "nbfrd")]
plot_nbt_summary[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbt_summary[, Status:=factor(Status, c("Before Adoption", "After Adoption"))]
pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoicenBFlavorDescr-BAfter.pdf", sep=""), width=8, height=5)
ggplot(data=plot_nbt_summary, aes(x=nbfrd, y=Percent, fill=Status)) + theme_minimal() +
  labs(x = "Brand and Flavors Purchased") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

pdf(file=paste(graph_dir, "/figs/HMS-DiscreteChoice-EverAdopt.pdf", sep=""), width=8, height=5)
nbt_summary[holder==0, Status:=ifelse(ever_holder==0, "Never Adopted", "Before Adoption")]
plot_nbt_summary =  nbt_summary[!is.na(Status), .(Percent = as.numeric(.N)), 
                        by = c("ever_holder", "holder", "Status", "nb")]
plot_nbt_summary[, `:=`(Percent = Percent/sum(Percent)), by = c("ever_holder", "holder", "Status")]
plot_nbt_summary[, Status:=factor(Status, c("Never Adopted", "Before Adoption"))]
ggplot(data=plot_nbt_summary[nb<=3, ], aes(x=nb, y=Percent, fill=Status)) + theme_minimal() +
  labs(x = "Number of Brands") + scale_fill_manual(values=c('lightskyblue','red'))+ 
  geom_bar(stat="identity", position=position_dodge()) + xlim(0.5, 5.5) + ylim(0,1)+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
dev.off()

#----------------------------------------------------------------------------------------------#

# Decomposing non-discrete choice into three categories
# (i) Pure Quantity Driven
# (ii) Flavor within Brand Driven (subject to definition)
# (iii) Brand Driven

# Do the Decomposition for Ground Coffee Purchases for never adopters, 
# Do the Decomposition for Ground Coffee Purchases before adoption for ever adopters
# Do the Decomposition for Keurig Purchases Purchases after adoption.

nbt_summary[, `:=`(qdum = as.integer(quantity>=2), 
                   bdum = as.integer(nb>=2), 
                   fdum = as.integer(nbfrd>=2))]

# Conditional Plot - Conditional on Not discrete Choice
nbt_cond = nbt_summary[qdum==1, ]
nbt_cond[, `:=`(qdum = ifelse(bdum==0&fdum==0, 1, 0),
                fdum = ifelse(bdum==0&fdum==1, 1, 0))]
nbt_table = nbt_cond[, .(qdum = mean(qdum), bdum = mean(bdum), fdum = mean(fdum)), 
                     by = c("ever_holder", "holder")]
nbt_table = melt(nbt_table, id.vars = c("ever_holder", "holder"), measure.vars = c("qdum", "bdum", "fdum"),
                 value.name = "Percent")
nbt_table[, `:=`(Status = factor(ifelse(ever_holder==0, "Never Adopted", 
                                        ifelse(holder==0, "Before Adoption", "After Adoption")),
                                 levels = c("After Adoption", "Before Adoption", "Never Adopted")),
                 Type = factor(ifelse(variable=="qdum", "Quantity>1", 
                                      ifelse(variable=="fdum", "No. of Flavors>1", "No. of Brands>1")),
                               levels = c("No. of Brands>1", "No. of Flavors>1", "Quantity>1")))]
setkeyv(nbt_table, c("Status", "Type"))
nbt_table = nbt_table[order(-Type), ]
nbt_table[, pos := cumsum(Percent) - (0.5 * Percent), by = "Status"]

# BarPlot
pdf(file=paste(graph_dir, "/figs/NO-Discrete-Choice-By-Type-Cond.pdf", sep=""), width=8, height=5)
ggplot(nbt_table, aes(x=Status, y=Percent, fill=Type))+ theme_bw() +
  geom_bar(position = "fill",stat = "identity", width=.5) + coord_flip()+scale_fill_brewer()+
  scale_y_continuous(labels = percent) + guides(fill = guide_legend(reverse = TRUE))+
  geom_text(aes(label = paste0(round(Percent,3)*100,"%"), y = pos), size = 3)
dev.off()

# Unconditional 
nbt_summary[, `:=`(dc = 1-qdum,
                   qdum = ifelse(bdum==0&fdum==0&qdum==1, 1, 0),
                   fdum = ifelse(bdum==0&fdum==1, 1, 0))]
nbt_table = nbt_summary[, .(dc=mean(dc), qdum = mean(qdum), bdum = mean(bdum), fdum = mean(fdum)), 
                        by = c("ever_holder", "holder")]
nbt_table = melt(nbt_table, id.vars = c("ever_holder", "holder"), 
                 measure.vars = c("dc", "qdum", "bdum", "fdum"),
                 value.name = "Percent")
nbt_table[, `:=`(Status=factor(ifelse(ever_holder==0, "Never Adopted", 
                                      ifelse(holder==0, "Before Adoption", "After Adoption")),
                               levels = c("After Adoption", "Before Adoption", "Never Adopted")),
                 Type=factor(ifelse(variable=="dc", "Discrete Choice", 
                                    ifelse(variable=="qdum", "Quantity>1", 
                                           ifelse(variable=="fdum", "No. of Flavors>1", 
                                                  "No. of Brands>1"))),
                             levels = c("No. of Brands>1", "No. of Flavors>1", 
                                        "Quantity>1", "Discrete Choice")))]
setkeyv(nbt_table, c("Status", "Type"))
nbt_table = nbt_table[order(-Type), ]
nbt_table[, pos := cumsum(Percent) - (0.5 * Percent), by = "Status"]
nbt_table[Percent<=0.000001, pos:=NA]

# BarPlot
pdf(file=paste(graph_dir, "/figs/NO-Discrete-Choice-By-Type.pdf", sep=""), width=8, height=5)
ggplot(nbt_table, aes(x=Status, y=Percent, fill=Type))+ theme_bw() + 
  geom_bar(position = "fill",stat = "identity", width=.5) + coord_flip() +
  scale_y_continuous(labels = percent) + scale_fill_brewer() + guides(fill = guide_legend(reverse = T))+
  geom_text(aes(label = paste0(round(Percent,3)*100,"%"), y = pos), size = 3)
dev.off()

# End of File

