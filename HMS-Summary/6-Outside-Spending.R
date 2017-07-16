# Load Auxiliary Data Sets 
load(paste(meta_dir, "/Trips.RData", sep=""))
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))
load(paste(output_dir, "/HMS-Summary-Auxiliary.RData", sep=""))

# Merge Trips information with adoption date
setkey(trips, household_code)
setkey(hh_list, household_code)

# Adopters by weeks
nhh_adopter = hh_list[!is.na(k_first_week) & !is.na(adoption_panel_year), .(nh = .N), by = "k_first_week"]
setkey(nhh_adopter, k_first_week)
nhh_adopter[, plot(k_first_week, nh, type ="o")]

# Filter households exists in 2011, 2012 and 2013. 
# If adoption households, only ones adopting in 2012
hh_list = hh_list[y2011==1 & y2012==1 & y2013==1, ]
hh_list = hh_list[is.na(k_first_date) | adoption_panel_year==2012, ]
trips = trips[hh_list[, .(household_code, k_first_date)], nomatch=0L]
trips[is.na(k_first_date), k_first_date:=as.Date("2020-12-31")]
trips[, adoption:=as.integer(purchase_date>=k_first_date)]

# Weekly Spending by Adoption
# Give date - generate weekend, ending on saturday
wkend <- function(x) x + 7 - wday(x) 
trips[, `:=`(week_end=wkend(purchase_date))]
trips[, `:=`(panel_year=year(week_end))]
trips = trips[panel_year%in%c(2011, 2013)]
spend_dt = trips[, .(coff=sum(coff), hot=sum(hot), caf=sum(caf), drink=sum(drink), grocery=sum(grocery), 
                     rest=sum(rest)), by = c("household_code", "adoption", "panel_year")]

# Create the difference
setkey(spend_dt, household_code, panel_year)
spend_dt = spend_dt[, .(adoption = adoption[2]-adoption[1], coff = coff[2]-coff[1], hot = hot[2]-hot[1], 
                        caf = caf[2]-caf[1], drink = drink[2]-drink[1], grocery = grocery[2]-grocery[1], 
                        rest = rest[2]-rest[1], rcoff = coff[2]/coff[1], rhot = hot[2]/hot[1], 
                        rcaf = caf[2]/caf[1], rdrink = drink[2]/drink[1], rgrocery = grocery[2]/grocery[1], 
                        rrest = rest[2]/rest[1]), by = c("household_code")]
spend_dt[, `:=`(Status = ifelse(adoption==1, "Adoption Households", "Reference Households"))]
spend_dt[, `:=`(rcaf_med = round(median(rcaf,na.rm=T), 3), 
                rdrink_med = round(median(rdrink,na.rm=T), 3), 
                rgrocery_med = round(median(rgrocery,na.rm=T), 3),
                rrest_med = round(median(rrest,na.rm=T), 3)), by = c("Status")]

# Plot Spending Graphs
gcaf = ggplot(spend_dt, aes(x=rcaf))+
  geom_histogram(alpha=1, center=0.1, aes(y = ..density..), binwidth=0.2, col="grey30", fill="skyblue") +
  scale_x_continuous("Caffeinated Drink Spending Ratio", limits = c(0, 4)) + theme_minimal() + facet_wrap(~ Status) +
  geom_vline(aes(xintercept = rcaf_med), col="red")
gdrink = ggplot(spend_dt, aes(x=rdrink))+
  geom_histogram(alpha=1, center=0.1, aes(y = ..density..), binwidth=0.2, col="grey30", fill="skyblue") +
  scale_x_continuous("Drink Spending Ratio", limits = c(0, 4)) + theme_minimal() + facet_wrap(~ Status) +
  theme(strip.text.x = element_blank()) + geom_vline(aes(xintercept = rdrink_med), col="red")
ggrocery = ggplot(spend_dt, aes(x=rgrocery))+
  geom_histogram(alpha=1, center=0.1, aes(y = ..density..), binwidth=0.2, col="grey30", fill="skyblue") +
  scale_x_continuous("Grocery Spending Ratio", limits = c(0, 4)) + theme_minimal() + facet_wrap(~ Status) +
  theme(strip.text.x = element_blank()) + geom_vline(aes(xintercept = rgrocery_med), col="red")
grest = ggplot(spend_dt, aes(x=rrest))+
  geom_histogram(alpha=1, center=0.1, aes(y = ..density..), binwidth=0.2, col="grey30", fill="skyblue") +
  scale_x_continuous("All Spending Ratio", limits = c(0, 4)) + theme_minimal() + facet_wrap(~ Status) +
  theme(strip.text.x = element_blank()) + geom_vline(aes(xintercept = rrest_med), col="red")
multplot = marrangeGrob(list(gcaf, gdrink, ggrocery, grest), ncol=1, nrow=4, top="")
ggsave(paste(graph_dir, "/figs/hist_outside_option.pdf", sep=""), multplot, width=10, height=14)
