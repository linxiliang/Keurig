# Load Auxiliary Data Sets 
load(paste(meta_dir, "/Trips.RData", sep=""))
load(paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))
load(paste(output_dir, "/HMS-Summary-Auxiliary.RData", sep=""))
load("~/Keurig/Data/MLogit-Data/Retailer_Price_Panel.RData")

# Number of brands 
retailer_panel = retailer_panel[brand_descr!="0NOTHING"&ptype=="KEURIG", ]
retailer_panel[, prod_id := .GRP, by = c("brand_descr_orig", "keurig", "ptype", "roast", "flavored", "kona",
                                         "colombian", "sumatra", "wb")]
prod_panel=retailer_panel[, .(dummy=1), by = c("week_end", "prod_id")]
prod_panel=prod_panel[, .(nprod=.N), by = c("week_end")]
setkey(prod_panel, week_end)

# Average purchase probabities
wkend <- function(x) x + 7 - wday(x) 
trips[, `:=`(week_end=wkend(purchase_date))]
trips[, `:=`(panel_year=year(week_end))]
trips = trips[panel_year%in%c(2011:2013)]
hh_list = hh_list[(year(k_first_week)<=2010 | is.na(k_first_week)) & y2011==1 & y2012==1 & y2013==1, ]
setkey(trips, household_code)
setkey(hh_list, household_code)
trips = trips[hh_list[, .(household_code, k_first_date)], nomatch=0L]
trips[, adoption:=as.integer(!is.na(k_first_date))]
coffee_trip_codes = unique(purchases[product_module_code==1463, trip_code_uc])
trips[, coffee_trip := as.integer(trip_code_uc%in%coffee_trip_codes)]
trips = trips[, .(coffee_trip = as.integer(any(coffee_trip==1))), by = c("household_code", "adoption", "week_end")]
trips = trips[, .(coffee_trip = sum(coffee_trip), nh = .N), by = c("adoption","week_end")]
trips[adoption==1, coffee_percent:=coffee_trip/nrow(hh_list[!is.na(k_first_date)])]
trips[adoption==0, coffee_percent:=coffee_trip/nrow(hh_list[is.na(k_first_date)])]
setkey(trips, week_end, adoption)
trips = trips[, .(coffee_percent = coffee_percent[2]/coffee_percent[1]), by = "week_end"]
setkey(trips, week_end)

# Relationship between variety and purchase prob
prob_panel = prod_panel[trips, nomatch=0L]

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
