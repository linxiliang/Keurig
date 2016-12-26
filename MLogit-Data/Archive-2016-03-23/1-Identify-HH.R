#---------------------------------------------------------------------------------------------------#
# Load appropriate data

# Load HMS Purchase data
load(paste(HMS_input_dir, "/HMS-Purchases-Coffee.RData", sep=""))
purchases[upc=="009955504002" & quantity == 24, quantity:=2] 
purchases[upc=="009955504002" & quantity == 12, quantity:=1]

# Load Product information
load(paste(meta_dir, "/Products.RData", sep=""))

# Classify products based on features, series and prices
c_list = as.list(1:5)
c_list[[1]] = c("B30", "K130")
c_list[[2]] = c("B31", "K10")
c_list[[3]] = c("B40", "K40", "K45", "B44")
c_list[[4]] = c("B60", "K60", "K65", "B66", "B50")
c_list[[5]] = c("B70", "K70", "B77", "K75")
#c_list[[4]] = c("B66", "B50")
#c_list[[4]] = c("B60", "K60", "K65")
#c_list[[5]] = c("K70/GCSC", "B77/GCSC=")
#c_list[[6]] = c("B70/FGCSC&TSC", "K70/GCSC&WF=", "K70/GCSC=")

products[, series:= as.integer(NA)]
for (i in c(1:5)){
  for (b in c_list[[i]]){
    products[grepl(b, upc_descr) & product_module_code == 7755 & ptype=="KEURIG", series:= i ]
  }
}

# Load HH information
load(paste(meta_dir, "/HH.RData", sep=""))
hh[, projection_factor:=projection_factor/10000]

# Make purchases trip, product unique
pnamelist = names(purchases)
pnamelist = setdiff(pnamelist, c("total_spent", "total_price_paid", "coupon_value", "deal_flag_uc","quantity"))
purchases = purchases[, .(total_price_paid = sum(total_price_paid),
                          coupon_value = sum(coupon_value),
                          quantity = sum(quantity)), by = pnamelist]
setkeyv(purchases, c("upc", "upc_ver_uc"))

# Merge in the type information to the purchase data
purchases=purchases[products[, .(upc, upc_ver_uc, ptype, pthird, brand_descr, size1_amount, multi, series)], nomatch=0L]
purchases[, month:=as.factor(format(purchase_date, '%Y-%m'))]

# Only Purchases Beyond 2008 matters 
purchases = purchases[panel_year>=2008, ]

#---------------------------------------------------------------------------------------------------#
# Generate Summary Statistics about HH information
# Income Summary
hh[,`:=`(inc25 = as.integer(household_income<=13),
         inc40 = as.integer(household_income>=14 & household_income<=17),
         inc50 = as.integer(household_income>=18 & household_income<=19),
         inc60 = as.integer(household_income>=21 & household_income<=23),
         inc70 = as.integer(household_income>=26))]

# Household size
hh[,`:=`(hhsize1 = as.integer(household_size==1),
         hhsize2 = as.integer(household_size==2),
         hhsize3 = as.integer(household_size>=3 & household_size<=4),
         hhsize5 = as.integer(household_size>=5))]

# Type of Residence
hh[,`:=`(onefamily = as.integer(type_of_residence == 1 | type_of_residence == 2),
         twofamily = as.integer(type_of_residence == 3 | type_of_residence == 4),
         threefamily = as.integer(type_of_residence==5|type_of_residence==6|type_of_residence==7))]

# Employement
hh[,`:=`(fulltime = as.integer(male_head_employment == 2 | male_head_employment == 3 |
                                 female_head_employment == 2 | female_head_employment == 3),
         notemployed = as.integer((male_head_employment == 9 & female_head_employment == 9) | 
                                    (male_head_employment == 9 & female_head_employment == 0) |
                                    (male_head_employment == 0 & female_head_employment == 9)))]
hh[,`:=`(partime = as.integer(fulltime!=1 & notemployed!=1))]

# Average of Male & Female Head
hh[male_head_age==0, male_head_age:=NA]
hh[female_head_age==0, female_head_age:=NA]
hh[,`:=`(male_head_age_numeric = ifelse(male_head_age<=7, (male_head_age - 1) * 5 + 22.5,
                                        (male_head_age-1) * 10 - 10), 
         female_head_age_numeric = ifelse(female_head_age<=7, (female_head_age - 1) * 5 + 22.5,
                                          (female_head_age-1) * 10 - 10))]
hh[, `:=`(nheads = as.integer(!is.na(male_head_age)) + as.integer(!is.na(female_head_age)))]
hh[is.na(male_head_age_numeric), male_head_age_numeric:=0]
hh[is.na(female_head_age_numeric), female_head_age_numeric:=0]
hh[,`:=`(head_age_numeric = (male_head_age_numeric + female_head_age_numeric) / nheads,
         nheads=NULL)]

# Presence of Children
hh[, presence_of_children := as.integer(age_and_presence_of_children<=7)]

# Race
hh[, `:=`(african_american = as.integer(race == 2), 
          hispanic = as.integer(hispanic_origin == 1))]

# TV and Internet
hh[, `:=`(cable = as.integer(tv_items == 2 | tv_items == 3), 
          internet = as.integer(household_internet_connection == 1))]

#---------------------------------------------------------------------------------------------------#
# Obtain the master list of households who regularly consumers coffee 
# Identify the first purchase of Keurig/Other Single Serving Coffee
# Flag consumers appropriately
# Get the potential households
setkey(purchases, household_code, purchase_date, trip_code_uc, upc, upc_ver_uc)
# Obtain the number of purchases made in each model, and the series of Keurig purchased if any. 
hh_list = purchases[, .(nptype=.N, series = series[1], panel_year = panel_year[1]), 
                    by = c("household_code", "ptype", "product_module_code")]

if (ground_only){
  # Only consider ground coffee or machine
  hh_list = hh_list[product_module_code %in% c(1463, maker_modules)] 
}

# Flag households with hardware purchases by type
hh_list[, `:=`(hware = as.numeric(product_module_code %in% maker_modules))]

# Flag households with software purchases
hh_list[product_module_code %in% coffee_modules, sware:=as.numeric(nptype>=1)]
hh_list[product_module_code %in% maker_modules, sware:=0]

# Flag both hardware and software users by platform type
hh_list = hh_list[, .(hware = as.numeric(any(hware==1)),
                      sware = as.numeric(any(sware==1)),
                      hsholders = as.numeric(any(hware==1)&any(sware==1)),
                      hseries = mean(series, na.rm=TRUE),
                      panel_year = min(panel_year)), 
                  by = c("household_code", "ptype")]
hh_list[is.na(hseries), hseries:=0]

# Merge back to purchases to find out the first purchase date of hardware,
# and find the first purchase date of software. 
setkeyv(hh_list, c("household_code", "ptype"))
setkeyv(purchases, c("household_code", "ptype"))
purch_temp = hh_list[purchases, nomatch = 0L]
purch_temp = purch_temp[sware==1 | hsholders==1, ]
maxdate = as.Date(max(purch_temp[, purchase_date]) + 1)
mindate = as.Date(min(purch_temp[, purchase_date]) - 1)
purch_temp[, date_temp1 := as.Date(ifelse(product_module_code %in% maker_modules, 
                                          purchase_date, maxdate), origin = "1970-01-01")]
purch_temp[, date_temp2 := as.Date(ifelse(product_module_code %in% maker_modules, 
                                          purchase_date, mindate), origin = "1970-01-01")]
purch_temp[, date_temp3 := as.Date(ifelse(product_module_code %in% coffee_modules, 
                                          purchase_date, maxdate), origin = "1970-01-01")]
purch_temp[, date_temp4 := as.Date(ifelse(product_module_code %in% coffee_modules, 
                                          purchase_date, mindate), origin = "1970-01-01")]
purch_temp[, ptemp1 := ifelse(product_module_code %in% maker_modules, 1, 0)]
purch_temp[, ptemp2 := ifelse(product_module_code %in% coffee_modules, 1, 0)]

hh_date = purch_temp[, .(hfirst_date = min(date_temp1),
                         hlast_date = max(date_temp2),
                         sfirst_date = min(date_temp3), 
                         slast_date  = max(date_temp4),
                         hn          = sum(ptemp1),
                         sn          = sum(ptemp2)),
                     by = c("household_code", "ptype")]
rm(purch_temp)
hh_date[, `:=`(hfirst_date = as.Date(ifelse(hfirst_date==maxdate, NA, hfirst_date),
                                     origin = "1970-01-01"),
               sfirst_date = as.Date(ifelse(sfirst_date==maxdate, NA, sfirst_date),
                                     origin = "1970-01-01"))]
hh_date[, `:=`(hlast_date = as.Date(ifelse(hlast_date==mindate, NA, hlast_date),
                                    origin = "1970-01-01"),
               slast_date = as.Date(ifelse(slast_date==mindate, NA, slast_date),
                                    origin = "1970-01-01"))]
setkeyv(hh_date, c("household_code", "ptype"))

# Merge date and purchase occasion information back to hh_list
hh_list = hh_list[hh_date, nomatch=0L] # 3378 households with only hardware purchases are dropped

# Break dataset into three parts, and merge into one data set
hh_list_other = hh_list[ptype=="OTHER", ]
hh_list_keurig = hh_list[ptype=="KEURIG", ]
hh_list_prior = hh_list[ptype!="KEURIG" & ptype!="OTHER", ]

# Generate the first date of prior machine holding
hh_list_prior = hh_list_prior[sn>=3 & as.integer(slast_date - sfirst_date)>=30, ]
hh_list_prior[, `:=`(first_date_temp = pmin(sfirst_date, hfirst_date, na.rm=TRUE),
                     last_date_temp = pmax(slast_date, hlast_date, na.rm=TRUE))]
hh_list_prior = hh_list_prior[, .(prior_first_date = min(first_date_temp), 
                                  prior_last_date = max(last_date_temp),
                                  prior_sn = sum(sn),
                                  prior_holder = 1), by = "household_code"]

# Generate the first date of Keurig machine holding status
hh_list_keurig = hh_list_keurig[sn>=3 & as.integer(slast_date - sfirst_date)>=30, ]
hh_list_keurig[, `:=`(kholder = 1, ptype = NULL, sware=NULL, hsholders=NULL)]

# Only keep the variables needed for household consuming ground coffee. 
hh_list_other = hh_list_other[, .(household_code, gfirst_date = sfirst_date, glast_date = slast_date, gsn = sn)]

# Obtain the list of households
hh_list = unique(c(hh_list_other[, household_code], hh_list_keurig[, household_code], 
                   hh_list_prior[, household_code]))
hh_list = data.table(household_code = hh_list)

# Merge the data sets together to create the updated hh_list
setkey(hh_list, household_code)
setkey(hh_list_keurig, household_code)
setkey(hh_list_prior, household_code)
setkey(hh_list_other, household_code)
hh_list = hh_list_prior[hh_list]
hh_list = hh_list_other[hh_list]
hh_list = hh_list_keurig[hh_list]
hh_list[is.na(hware), hware:=0]
hh_list[is.na(hseries), hseries:=0]
hh_list[is.na(hn), hn:=0]
hh_list[is.na(sn), sn:=0]
hh_list[is.na(kholder), kholder:=0]
hh_list[is.na(gsn), gsn:=0]
hh_list[is.na(prior_sn), prior_sn:=0]
hh_list[is.na(prior_holder), prior_holder:=0]

# Flag panel_year as the year the first time the individual is observed to make a software purchase
hh_list_keurig[, days_to_sfirst:=as.numeric(sfirst_date - hfirst_date)]
hh_list_keurig[days_to_sfirst < -45, days_to_sfirst:=NA]
hh_list_keurig[days_to_sfirst < 0, days_to_sfirst:=0]
hh_list_keurig[days_to_sfirst > 180, days_to_sfirst:=NA]
setkeyv(hh_list_keurig, c("household_code", "panel_year")) 
# About 50% Purchase Keurig and Coffee at the same time or software purchase happen earlier than coffee purchase.
# About 70% Purchase within the first 2 weeks.

# Merge in hh information, and conduct regressions
cols = c("household_code", "panel_year", "projection_factor",
         "inc25", "inc40", "inc50", "inc60", "inc70", 
         "hhsize1", "hhsize2", "hhsize3", "hhsize5",
         "onefamily", "twofamily", "threefamily", 
         "fulltime", "notemployed", "partime",
         "head_age_numeric", "presence_of_children",
         "african_american", "hispanic", "cable", "internet",
         "panelist_zip_code", "fips_county_code", "fips_county_descr",
         "scantrack_market_code", "scantrack_market_descr", "dma_code", "dma_descr")

# Encoding and cleaning hh information
hh = hh[, cols, with=FALSE]
evars = c("household_code", "panel_year", "projection_factor")
lvars = setdiff(cols, evars)
for (v in lvars){
  setnames(hh, v, "vars")
  #if missing put 0, missing is extremely rare.
  hh[is.na(vars), vars:=0] 
  # hh[, vars:=factor(vars)]
  setnames(hh, "vars", v)
}
setkeyv(hh, c("household_code", "panel_year"))

# Merge in hh info
hh_list_keurig = hh_list_keurig[hh, nomatch=0L]

# Run regression of hh characteristics
excl_set = c("household_code", "panel_year", "projection_factor",
             "panelist_zip_code", "fips_county_code", "fips_county_descr",
             "scantrack_market_code", "scantrack_market_descr", 
             "dma_code", "dma_descr")
fm = as.formula(paste("days_to_sfirst ~ ", paste(setdiff(cols, excl_set), collapse="+",sep=""), "-1", sep=""))
X = model.matrix(fm, data=hh_list_keurig[!is.na(days_to_sfirst), ], sparse = TRUE)
y = model.extract(model.frame(fm, data=hh_list_keurig[!is.na(days_to_sfirst),]), "response")
lasso_days = glmnet(X, y, alpha = 1)
fm2 = as.formula(paste(" ~ ", paste(setdiff(cols, excl_set), collapse="+",sep=""), "-1", sep=""))
NewX = model.matrix(fm2, data=hh_list_keurig, sparse = TRUE)
Newy = predict(lasso_days, NewX, type="response", exact = FALSE)[,50]
hh_list_keurig[, new_days := Newy]
# I compared the predicted days to the actual days --- just too far off
# As majority of the customers buy the machine buy K-Cups within 14 days,
# I will flag the hardware acquisition date as the first day they buy the portion packs.

# Flag customer presence in each panel year
setkeyv(hh_list, c("household_code"))
hnames = names(hh_list)
ylist = sort(unique(hh[, panel_year]))
for (yr in ylist){
  hh_temp = hh[panel_year==yr, .(household_code)]
  hh_temp[, presence:=1]
  setkey(hh_temp, household_code)
  hh_list = hh_temp[hh_list]
  hh_list[is.na(presence), presence:=0]
  newname = paste("y", yr, sep="")
  setnames(hh_list, "presence", newname)
  hnames=c(hnames, newname)
}
setcolorder(hh_list, hnames)

# Considering burnin period, impute first and last hardware adoption
hh_list[, himputed:=0]
for (yr in ylist){
  first_date = as.Date(paste(yr, "-01-01", sep=""))
  hh_list[panel_year==yr & is.na(hfirst_date) & sfirst_date>=(first_date+burnindays), 
          `:=`(hfirst_date = sfirst_date, hlast_date = sfirst_date, himputed = 1)]
  
  # If present in last year. Then, no need to burnin
  if(yr >= 2005){
    lastyn = paste("y", (yr-1), sep="")
    setnames(hh_list, lastyn, "lastyname")
    hh_list[panel_year==yr & is.na(hfirst_date) & lastyname==1, 
            `:=`(hfirst_date = sfirst_date, hlast_date = sfirst_date,
                 himputed = 1)]
    setnames(hh_list, "lastyname", lastyn)
  }
}

# Drop households who are not present during the years 2008, 2009, 2010, 2011, 2012, 2013. 
hh_list[, existence := (y2008+y2009+y2010+y2011+y2012+y2013)]
hh_list = hh_list[existence>0, ]
setnames(hh_list, "panel_year", "adoption_panel_year")

# Get the smaller of machine and software adoption date
hh_list[, k_first_date := ifelse(is.na(hfirst_date), sfirst_date, 
                                 ifelse(abs(sfirst_date - hfirst_date)>=45, # 45 day benchmark for hardware adoption to count.
                                        sfirst_date, hfirst_date))]
hh_list[, k_first_date := as.Date(k_first_date, origin = "1970-01-01")]

# Merge back into purchases to get consumption rate
setkey(purchases, household_code)
setkey(hh_list, household_code)
purchases = purchases[hh_list[, .(household_code, kholder, k_first_date)], nomatch=0L]
setkey(purchases, household_code, purchase_date, trip_code_uc, upc, upc_ver_uc)

# If never adopted Keurig, let k_first_date be the last date of any purchase
max_date = purchases[, max(purchase_date)]
purchases[is.na(k_first_date), k_first_date := max_date]

# Obtain purchases and purchase occasions
purchases[, adoption:=as.integer(purchase_date>=k_first_date)]
purchases = purchases[product_module_code==1463, ]
hh_purch = purchases[, .(quantity = sum(quantity * size1_amount), npurch = .N), by = c("household_code", "adoption")]

# reshape data
hh_purch_0 = hh_purch[adoption==0, ]
hh_purch_1 = hh_purch[adoption==1, ]
setnames(hh_purch_0, c("quantity", "npurch"), c("bquantity", "bnpurch"))
setnames(hh_purch_1, c("quantity", "npurch"), c("aquantity", "anpurch"))
hh_purch_0[, adoption:=NULL]
hh_purch_1[, adoption:=NULL]
setkey(hh_purch_0, household_code)
setkey(hh_purch_1, household_code)
hh_purch = merge(hh_purch_0, hh_purch_1, by = "household_code", all=TRUE)

# Merge back into household_information
setkey(hh_purch, household_code)
setkey(hh_list, household_code)
hh_list = hh_list[hh_purch, nomatch=0L]

# Get the number of weeks existence in panel and before and after adoption
hh_list[is.na(k_first_date), `:=`(bweeks = (existence * 52.1429),
                                  aweeks = as.numeric(NA))]
hh_list[!is.na(k_first_date) & adoption_panel_year==2008, 
        `:=`(bweeks = as.numeric(week(k_first_date)), 
             aweeks = (52.1429-week(k_first_date)+(y2009+y2010+y2011+y2012+y2013)*52.1429))]
hh_list[!is.na(k_first_date) & adoption_panel_year==2009, 
        `:=`(bweeks = week(k_first_date) + y2008*52.1429, 
             aweeks = (52.1429-week(k_first_date)+(y2010+y2011+y2012+y2013)*52.1429))]
hh_list[!is.na(k_first_date) & adoption_panel_year==2010, 
        `:=`(bweeks = week(k_first_date) + (y2008+y2009)*52.1429, 
             aweeks = (52.1429-week(k_first_date)+(y2011+y2012+y2013)*52.1429))]
hh_list[!is.na(k_first_date) & adoption_panel_year==2011, 
        `:=`(bweeks = week(k_first_date) + (y2008+y2009+y2010)*52.1429, 
             aweeks = (52.1429-week(k_first_date)+(y2012+y2013)*52.1429))]
hh_list[!is.na(k_first_date) & adoption_panel_year==2012, 
        `:=`(bweeks = week(k_first_date) + (y2008+y2009+y2010+y2011)*52.1429, 
             aweeks = (52.1429-week(k_first_date)+(y2013)*52.1429))]
hh_list[!is.na(k_first_date) & adoption_panel_year==2013, 
        `:=`(bweeks = week(k_first_date) + (y2008+y2009+y2010+y2011+y2012)*52.1429, 
             aweeks = (52.1429-week(k_first_date)))]

# Get average consumption per-week
hh_list[, `:=`(brate = bquantity/(existence * 52.1429),
               arate = aquantity/(existence * 52.1429))]

# The household has to make at least 2 purchases annually to be counted, or at least consume 1 kcup on average per week. 
hh_list[, annual_purchases := (sn+gsn+prior_sn)/existence]
hh_list = hh_list[annual_purchases>=2 | arate >= 1, ]

# Obtain the expenditure shares and reshape data as wide format
hh_brand_purch = purchases[, .(expenditure = sum(total_price_paid-coupon_value),
                         quantity = sum(quantity * size1_amount)), 
                     by = c("household_code", "adoption", "brand_descr")]
hh_brand_purch[, `:=`(expend_share = expenditure/sum(expenditure),
                      expenditure = sum(expenditure), 
                      quan_share = quantity/sum(quantity)),
               by = c("household_code", "adoption")]
hh_brand_purch = hh_brand_purch[order(-expend_share), ]
setkeyv(hh_brand_purch, c("household_code", "adoption"))
hh_brand_purch[, brand := 1:length(expenditure), by = c("household_code", "adoption")]

# Only keep the top 3 brands
hh_brand_purch = hh_brand_purch[brand<=3, ]

# Reshape data
hh_brand_purch = dcast(hh_brand_purch, household_code + adoption + expenditure ~ brand, 
                       value.var = c("brand_descr", "expend_share", "quan_share"))

hh_brand_purch = dcast(hh_brand_purch, household_code ~ adoption, 
                       value.var = c("expenditure", "brand_descr_1", "brand_descr_2",
                                     "brand_descr_3", "expend_share_1", "expend_share_2",
                                     "expend_share_3", "quan_share_1", "quan_share_2",
                                     "quan_share_3"))
setnames(hh_brand_purch, setdiff(names(hh_brand_purch), "household_code"),
         c("bexpenditure", "aexpenditure", "bbrand_descr_1", "abrand_descr_1",
           "bbrand_descr_2", "abrand_descr_2", "bbrand_descr_3", "abrand_descr_3",
           "bexpend_share_1", "aexpend_share_1", "bexpend_share_2", "aexpend_share_2",
           "bexpend_share_3", "aexpend_share_3", "bquan_share_1", "aquan_share_1",
           "bquan_share_2", "aquan_share_2", "bquan_share_3", "aquan_share_3"))

# Merge back into household_information
setkey(hh_brand_purch, household_code)
setkey(hh_list, household_code)
hh_list = hh_list[hh_brand_purch, nomatch=0L]

# Merge into household data.
setkey(hh, household_code)
setkey(hh_list, household_code)
hh_info_panel = hh_list[hh, nomatch=0L]

save(hh_list, hh_info_panel, file = paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))

#Clean up workspace
gc()
# End of this script -------------------------------------------------------------------#
