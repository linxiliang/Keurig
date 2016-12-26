# Load Auxiliary Data Sets 
load(paste(output_dir, "/HMS-Summary-Auxiliary.RData", sep=""))

# Temporal Switching Behavior - Brand Level
# Individual Before and After 
purchases[, np:=length(unique(trip_code_uc)), by = c("household_code", "product_module_code")]
setkey(purchases, household_code, ever_holder, holder, purchase_date, trip_code_uc)
T_purchases=purchases[np>=5 & product_module_code==1463, .(br_list = paste0(unique(brand_descr), collapse=" ")),
                      by = c("household_code", "ever_holder", "holder","purchase_date", "trip_code_uc")]
T_purchases[, br_list_lag := c(NA, br_list[1:(length(br_list)-1)]), 
             by = c("household_code")]
T_purchases = T_purchases[!is.na(br_list_lag), ]
T_purchases[, indicator := as.integer(grepl(br_list, br_list_lag)), 
             by = c("household_code", "ever_holder", "holder","trip_code_uc")]
sbrand_trips = unique(T_purchases[, trip_code_uc])
T_purchases = T_purchases[, .(Percent=mean(indicator)), 
                            by = c("household_code", "ever_holder", "holder")]

# Percentage of trips that buys a subset of brands of the last trip.
T_purchases[,`:=`(Status = factor(ifelse(holder==0&ever_holder==0, "Never Adopted", 
                                         ifelse(holder==0, "Before Adoption", "After Adoption")),
                                  levels = c("Never Adopted", "Before Adoption", "After Adoption")))]
md1 = round(median(T_purchases[holder==0&ever_holder==0, Percent]), 2)
md2 = round(median(T_purchases[holder==0&ever_holder==1, Percent]), 2)
md3 = round(median(T_purchases[holder==1&ever_holder==1, Percent]), 2)

pdf(file=paste(graph_dir, "/figs/HMS-Temporal-Switching-BAfter.pdf", sep=""), width=8, height=5)
ggplot(T_purchases[, .(Percent, Status)], aes(Percent, fill = Status)) + 
  labs(x = "Percent of Trips Choosing a Subset of Brands of Last Trip") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=0.05, position = 'identity') +
  annotate("text", x = c(0.70,0.70,0.70), y=c(1.9, 1.8, 1.7), 
           label = c(paste("Never Adopted Median:", md1), paste("Before Adoption Median:", md2),
                     paste("After Adoption Median:", md3)), hjust=0, size = 3) +
  theme(legend.justification=c(0,1), legend.position=c(0.70,1))
dev.off()


# Treatment versus control
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
T_treat_ctrl[,`:=`(Status = ifelse(treat==0, "Reference", "Adopter"),
                   bafter = ifelse(after==0, "Years 2008 and 2009", "Years 2012 and 2013"))]
md00 = round(median(T_treat_ctrl[treat==0 & after==0, Percent]), 2)
md10 = round(median(T_treat_ctrl[treat==1 & after==0, Percent]), 2)
md01 = round(median(T_treat_ctrl[treat==0 & after==1, Percent]), 2)
md11 = round(median(T_treat_ctrl[treat==1 & after==1, Percent]), 2)
pdf(file=paste(graph_dir, "/figs/HMS-Temporal-Switching.pdf", sep=""), width=8, height=5)
ggplot(T_treat_ctrl[, .(Percent, Status, bafter)], aes(Percent, fill = Status)) + 
  labs(x = "Percent of Trips Choosing a Subset of Brands of Last Trip") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=0.05, position = 'identity') +
  annotate("text", x = 0.5, y = 3.5, 
           label = c(paste("Reference median:", md00),paste("Reference median:", md01)), 
           hjust=0, size = 3) +
  annotate("text", x = 0.5, y = 3.25, 
           label = c(paste("Adopter median:", md10), paste("Adopter median:", md11)), 
           hjust=0, size = 3) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) + facet_grid(~bafter)
dev.off()

#----------------------------------------------------------------------------------------------#
# Define Switching as Changing Flavors or Brands
# Individual Before and After 
setkey(purchases, household_code, ever_holder, holder, purchase_date, trip_code_uc)
purchases[product_module_code==1463, flavor_brand_temp := .GRP, 
          by = c("brand_descr", "ptype", "flavor_descr", "style_descr")]
purchases[, nrow := 1:.N]
purchases[, flavor_brand := paste("fb", flavor_brand_temp, "_", sep=""), by = "nrow"]
purchases[, `:=`(flavor_brand_temp=NULL, nrow=NULL)]
T_purchases=purchases[np>=5&product_module_code==1463, .(br_list = paste0(unique(flavor_brand), collapse=" ")),
                      by = c("household_code", "ever_holder", "holder", "purchase_date", "trip_code_uc")]
T_purchases[, br_list_lag := c(NA, br_list[1:(length(br_list)-1)]), 
            by = c("household_code")]
T_purchases = T_purchases[!is.na(br_list_lag), ]
T_purchases[, indicator := as.integer(grepl(br_list, br_list_lag)), 
            by = c("household_code", "ever_holder", "holder","trip_code_uc")]
T_purchases = T_purchases[, .(Percent=mean(indicator)), 
                          by = c("household_code", "ever_holder", "holder")]

# Percentage of trips that buys a subset of brands of the last trip.
T_purchases[,`:=`(Status = factor(ifelse(holder==0&ever_holder==0, "Never Adopted", 
                                         ifelse(holder==0, "Before Adoption", "After Adoption")),
                                  levels = c("Never Adopted", "Before Adoption", "After Adoption")))]
md1 = round(median(T_purchases[holder==0&ever_holder==0, Percent]), 2)
md2 = round(median(T_purchases[holder==0&ever_holder==1, Percent]), 2)
md3 = round(median(T_purchases[holder==1&ever_holder==1, Percent]), 2)

pdf(file=paste(graph_dir, "/figs/HMS-Temporal-Switching-Flavor-BAfter.pdf", sep=""), width=8, height=5)
ggplot(T_purchases[, .(Percent, Status)], aes(Percent, fill = Status)) + 
  labs(x = "Percent of Trips Choosing a Subset of Brands and Flavors of Last Trip") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=0.05, position = 'identity') +
  annotate("text", x = c(0.75,0.75,0.75), y=c(4, 3.8, 3.6), 
           label = c(paste("Never Adopted Median:", md1), paste("Before Adoption Median:", md2),
                     paste("After Adoption Median:", md3)), hjust=0, size = 3) +
  theme(legend.justification=c(0,1), legend.position=c(0.8,1))
dev.off()

# Treatment versus control
setkey(treat_ctrl_panel, household_code, treat, after, purchase_date, trip_code_uc)
treat_ctrl_panel[, flavor_brand_temp := .GRP, by = c("brand_descr", "ptype", "flavor_descr", "style_descr")]
treat_ctrl_panel[, nrow := 1:.N]
treat_ctrl_panel[, flavor_brand := paste("fb", flavor_brand_temp, "_", sep=""), by = "nrow"]
treat_ctrl_panel[, `:=`(flavor_brand_temp=NULL, nrow=NULL)]

T_treat_ctrl = treat_ctrl_panel[, .(br_list = paste0(unique(flavor_brand), collapse=" ")),
                                by = c("household_code", "treat", "after", "purchase_date", "trip_code_uc")]
T_treat_ctrl[, br_list_lag := c(NA, br_list[1:(length(br_list)-1)]), 
             by = c("household_code", "treat", "after")]
T_treat_ctrl = T_treat_ctrl[!is.na(br_list_lag), ]
T_treat_ctrl[, indicator := as.integer(grepl(br_list, br_list_lag)), 
             by = c("household_code", "treat", "after", "trip_code_uc")]
T_treat_ctrl = T_treat_ctrl[, .(Percent=mean(indicator)), 
                            by = c("household_code", "treat", "after")]

# Percentage of trips that buys a subset of brands of the last trip.
T_treat_ctrl[,`:=`(Status = ifelse(treat==0, "Reference", "Adopter"),
                   bafter = ifelse(after==0, "Years 2008 and 2009", "Years 2012 and 2013"))]
md00 = round(median(T_treat_ctrl[treat==0 & after==0, Percent]), 2)
md10 = round(median(T_treat_ctrl[treat==1 & after==0, Percent]), 2)
md01 = round(median(T_treat_ctrl[treat==0 & after==1, Percent]), 2)
md11 = round(median(T_treat_ctrl[treat==1 & after==1, Percent]), 2)
pdf(file=paste(graph_dir, "/figs/HMS-Temporal-Switching-Flavor.pdf", sep=""), width=8, height=5)
ggplot(T_treat_ctrl[, .(Percent, Status, bafter)], aes(Percent, fill = Status)) + 
  labs(x = "Percent of Trips Choosing a Subset of Brands and Flavors of Last Trip") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=0.05, position = 'identity') +
  annotate("text", x = 0.5, y = 3.5, 
           label = c(paste("Reference median:", md00),paste("Reference median:", md01)), 
           hjust=0, size = 3) +
  annotate("text", x = 0.5, y = 3.25, 
           label = c(paste("Adopter median:", md10), paste("Adopter median:", md11)), 
           hjust=0, size = 3) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) + facet_grid(~bafter)
dev.off()

#----------------------------------------------------------------------------------------------#
# Define Switching as Changing Flavors or Brands
# Individual Before and After 
setkey(purchases, household_code, ever_holder, holder, purchase_date, trip_code_uc)
purchases[product_module_code==1463, flavor_brand_temp := .GRP, 
          by = c("brand_descr", "ptype", "flavor_descr", "style_descr")]
purchases[, nrow := 1:.N]
purchases[, flavor_brand := paste("fb", flavor_brand_temp, "_", sep=""), by = "nrow"]
purchases[, `:=`(flavor_brand_temp=NULL, nrow=NULL)]
T_purchases=purchases[np>=5 & product_module_code==1463, 
                      .(br_list = paste0(unique(brand_descr), collapse=" "),
                        fl_list = paste0(unique(flavor_brand), collapse=" ")),
                      by = c("household_code", "ever_holder", "holder","purchase_date", "trip_code_uc")]
T_purchases[, `:=`(br_list_lag = c(NA, br_list[1:(length(br_list)-1)]),
                   fl_list_lag = c(NA, fl_list[1:(length(fl_list)-1)])),
            by = c("household_code")]
T_purchases = T_purchases[!is.na(br_list_lag), ]
T_purchases[, `:=`(b_ind = as.integer(grepl(br_list, br_list_lag)),
                   f_ind = as.integer(grepl(fl_list, fl_list_lag))),
            by = c("household_code", "ever_holder", "holder","trip_code_uc")]
T_purchases = T_purchases[b_ind==1, .(Percent=mean(f_ind)), 
                          by = c("household_code", "ever_holder", "holder")]

# Percentage of trips that buys a subset of brands of the last trip.
T_purchases[,`:=`(Status = factor(ifelse(holder==0&ever_holder==0, "Never Adopted", 
                                         ifelse(holder==0, "Before Adoption", "After Adoption")),
                                  levels = c("Never Adopted", "Before Adoption", "After Adoption")))]
md1 = round(median(T_purchases[holder==0&ever_holder==0, Percent]), 2)
md2 = round(median(T_purchases[holder==0&ever_holder==1, Percent]), 2)
md3 = round(median(T_purchases[holder==1&ever_holder==1, Percent]), 2)

pdf(file=paste(graph_dir, "/figs/HMS-Temporal-Switching-Flavor-BAfter-Cond.pdf", sep=""), width=8, height=5)
ggplot(T_purchases[, .(Percent, Status)], aes(Percent, fill = Status)) + 
  labs(x = "Percent of Trips Choosing a Subset of Brands and Flavors of Last Trip") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=0.05, position = 'identity') +
  annotate("text", x = c(0.7,0.7,0.7), y=c(3, 2.8, 2.6), 
           label = c(paste("Never Adopted Median:", md1), paste("Before Adoption Median:", md2),
                     paste("After Adoption Median:", md3)), hjust=0, size = 3) +
  theme(legend.justification=c(0,1), legend.position=c(0.7,1))
dev.off()

# Treatment versus control
setkey(treat_ctrl_panel, household_code, treat, after, purchase_date, trip_code_uc)
treat_ctrl_panel[, flavor_brand_temp := .GRP, by = c("brand_descr", "ptype", "flavor_descr", "style_descr")]
treat_ctrl_panel[, nrow := 1:.N]
treat_ctrl_panel[, flavor_brand := paste("fb", flavor_brand_temp, "_", sep=""), by = "nrow"]
treat_ctrl_panel[, `:=`(flavor_brand_temp=NULL, nrow=NULL)]

T_treat_ctrl = treat_ctrl_panel[trip_code_uc%in%sbrand_trips, 
                                .(br_list = paste0(unique(brand_descr), collapse=" "),
                                  fl_list = paste0(unique(flavor_brand), collapse=" ")),
                                by = c("household_code", "treat", "after", "purchase_date", "trip_code_uc")]
T_treat_ctrl[, `:=`(br_list_lag = c(NA, br_list[1:(length(br_list)-1)]),
                    fl_list_lag = c(NA, fl_list[1:(length(fl_list)-1)])),
             by = c("household_code", "treat", "after")]
T_treat_ctrl = T_treat_ctrl[!is.na(br_list_lag), ]
T_treat_ctrl[, `:=`(b_ind = as.integer(grepl(br_list, br_list_lag)),
                    f_ind = as.integer(grepl(fl_list, fl_list_lag))),
             by = c("household_code", "treat", "after", "trip_code_uc")]
T_treat_ctrl = T_treat_ctrl[b_ind==1, .(Percent=mean(f_ind)),  
                            by = c("household_code", "treat", "after")]

# Percentage of trips that buys a subset of brands of the last trip.
T_treat_ctrl[,`:=`(Status = ifelse(treat==0, "Reference", "Adopter"),
                   bafter = ifelse(after==0, "Years 2008 and 2009", "Years 2012 and 2013"))]
md00 = round(median(T_treat_ctrl[treat==0 & after==0, Percent]), 2)
md10 = round(median(T_treat_ctrl[treat==1 & after==0, Percent]), 2)
md01 = round(median(T_treat_ctrl[treat==0 & after==1, Percent]), 2)
md11 = round(median(T_treat_ctrl[treat==1 & after==1, Percent]), 2)
pdf(file=paste(graph_dir, "/figs/HMS-Temporal-Switching-Flavor-Cond.pdf", sep=""), width=8, height=5)
ggplot(T_treat_ctrl[, .(Percent, Status, bafter)], aes(Percent, fill = Status)) + 
  labs(x = "Percent of Trips Choosing a Subset of Brands and Flavors of Last Trip") + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=0.05, position = 'identity') +
  annotate("text", x = 0.5, y = 3.5, 
           label = c(paste("Reference median:", md00),paste("Reference median:", md01)), 
           hjust=0, size = 3) +
  annotate("text", x = 0.5, y = 3.25, 
           label = c(paste("Adopter median:", md10), paste("Adopter median:", md11)), 
           hjust=0, size = 3) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) + facet_grid(~bafter)
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
