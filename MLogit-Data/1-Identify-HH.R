#---------------------------------------------------------------------------------------------------#
# Load appropriate data

# Load HMS Purchase data
load(paste(HMS_input_dir, "/HMS-Purchases-Coffee.RData", sep=""))
purchases[upc=="009955504002" & quantity == 24, quantity:=2] 
purchases[upc=="009955504002" & quantity == 12, quantity:=1]

# Load Product information
load(paste(meta_dir, "/Products.RData", sep=""))

# Load trip level information to get hh demographics in terms of consumption
load(paste(meta_dir, "/Trips.RData", sep=""))

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
    products[grepl(b, upc_descr) & as.integer(product_module_code) == 7755 & ptype=="KEURIG", series:= i ]
  }
}

# Load HH information
load(paste(meta_dir, "/HH.RData", sep=""))
hh[, projection_factor:=projection_factor/10000]

# Make purchases trip, product unique
pnamelist = names(purchases)
pnamelist = setdiff(pnamelist, c("total_spent", "total_price_paid", "coupon_value", "deal_flag_uc","quantity"))
purchases = purchases[, .(total_price_paid = sum(total_price_paid), coupon_value = sum(coupon_value),
                          quantity = sum(quantity)), by = pnamelist]
setkeyv(purchases, c("upc", "upc_ver_uc"))

# Merge in the type information to the purchase data
purchases=purchases[products[, .(upc, upc_ver_uc, ptype, pthird, brand_descr, 
                                 size1_amount, multi, series)], nomatch=0L]
purchases[, month_var:=as.factor(format(purchase_date, '%Y-%m'))]

# Only Purchases Beyond 2008 matters 
# purchases = purchases[panel_year>=2008, ]

#---------------------------------------------------------------------------------------------------#
# Generate Summary Statistics about HH information
# Trip pattern summary
hh_tr_year = trips[, .(total_spent = sum(total_spent), coff = sum(coff), 
                       caf = sum(caf), drink=sum(drink), grocery = sum(grocery),
                       rest = sum(rest), ntrips = .N, nretailers = length(unique(retailer_code))),
                   by = c("household_code", "panel_year")]
hh_ry = trips[, .(total_spent = sum(total_spent), coff = sum(coff), 
                  caf = sum(caf), drink=sum(drink), grocery = sum(grocery),
                  rest = sum(rest), ntrips = .N),
              by = c("household_code", "retailer_code","panel_year")]
hh_ry[, `:=`(total_spent_sum = sum(total_spent), coff_sum = sum(coff), 
             caf_sum = sum(caf), drink_sum=sum(drink), grocery_sum = sum(grocery),
             rest_sum = sum(rest), ntrips_sum = sum(ntrips)),
      by = c("household_code", "panel_year")]
hh_ry = hh_ry[, .(total_spent_hhi = sum((100*total_spent/total_spent_sum)^2),
                  coff_hhi = sum((100*coff/coff_sum)^2), 
                  caf_hhi = sum((100*caf/caf_sum)^2), 
                  drink_hhi = sum((100*drink/drink_sum)^2), 
                  grocery_hhi = sum((100*grocery/grocery_sum)^2),
                  rest_hhi = sum((100*rest/rest_sum)^2), 
                  ntrips_hhi = sum((100*ntrips/ntrips_sum)^2)),
              by = c("household_code", "panel_year")]
setkeyv(hh_ry, c("household_code", "panel_year"))
setkeyv(hh_tr_year, c("household_code", "panel_year"))
hh_tr_year = hh_tr_year[hh_ry]
rm(hh_ry)

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
hh[as.integer(male_head_age)==0, male_head_age:=NA]
hh[as.integer(male_head_age)==0, female_head_age:=NA]
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

# Include additional variables 
setkeyv(hh, c("household_code", "panel_year"))
setkeyv(hh_tr_year, c("household_code", "panel_year"))
hh = hh[hh_tr_year, nomatch=0L]
htnames = names(hh_tr_year)
save(hh, file = paste(meta_dir, "/HH-Cleaned.RData", sep=""))
rm(hh_tr_year)
gc()
#---------------------------------------------------------------------------------------------------#
# Obtain the master list of households who regularly consumers coffee 
# Identify the first purchase of Keurig/Other Single Serving Coffee
# Flag consumers appropriately
# Get the potential households
setkey(purchases, household_code, purchase_date, trip_code_uc, upc, upc_ver_uc)

if (ground_only){
  # Only consider ground coffee or machine
  purchases = purchases[product_module_code %in% c(1463, maker_modules), ] 
}

# Obtain the number of purchases made in each model, and the series of Keurig purchased if any. 
hh_list = purchases[, .(nptype=.N, series = series[1], panel_year = panel_year[1],
                        purchase_date=purchase_date[1]), 
                    by = c("household_code", "ptype", "product_module_code")]

# Flag households with hardware purchases by type
hh_list[, `:=`(hware = as.numeric(product_module_code %in% maker_modules))]

# Flag households with software purchases
hh_list[product_module_code %in% coffee_modules, sware:=as.numeric(nptype>=1)]
hh_list[product_module_code %in% maker_modules, sware:=0]

# Flag both hardware and software users by platform type
hh_list = hh_list[, .(hware = as.integer(sum(hware)),
                      sware = as.integer(sum(sware)),
                      hsholders = as.integer((sum(hware)>=1)&(sum(sware)>=1)),
                      hseries = as.integer(mean(series, na.rm=TRUE))), 
                  by = c("household_code", "ptype")]
hh_list[is.na(hseries), hseries:=0]

# Merge back to purchases to find out the first purchase date of hardware,
# and find the first purchase date of software. 
setkeyv(hh_list, c("household_code", "ptype"))
setkeyv(purchases, c("household_code", "ptype"))
purch_temp = hh_list[purchases, nomatch = 0L]
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
                         hlast_date  = max(date_temp2),
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
hh_list = hh_list[hh_date, nomatch=0L] 

# Break dataset into three parts, and merge into one data set
hh_list_other = hh_list[ptype=="OTHER", ]
hh_list_keurig = hh_list[ptype=="KEURIG", ]
hh_list_prior = hh_list[ptype!="KEURIG" & ptype!="OTHER", ]

# Generate the first date of prior machine holding
# hh_list_prior = hh_list_prior[sn>=2 & as.integer(slast_date - sfirst_date)>=30, ]
hh_list_prior[, `:=`(first_date_temp = pmin(sfirst_date, hfirst_date, na.rm=TRUE),
                     last_date_temp = pmax(slast_date, hlast_date, na.rm=TRUE))]
hh_list_prior = hh_list_prior[, .(prior_first_date = min(first_date_temp), 
                                  prior_last_date = max(last_date_temp),
                                  prior_sn = sum(sn),
                                  prior_holder = 1), by = "household_code"]

# Generate the first date of Keurig machine holding status
# Two purchases of software (on different days) or hardware purchases.
# hh_list_keurig = hh_list_keurig[sn>=2 & as.integer(slast_date-sfirst_date)>=30, ]
hh_list_keurig[, `:=`(kholder = as.integer((sn>=2&sfirst_date<(slast_date+7))|hn>=1), 
                      ptype = NULL, sware=NULL, hsholders=NULL)]

# Only keep the variables needed for household consuming ground coffee. 
hh_list_other = hh_list_other[, .(household_code, gfirst_date = sfirst_date, glast_date = slast_date, gsn = sn)]

# Obtain the list of households
hh_list = unique(c(hh_list_other[, household_code], hh_list_keurig[, household_code], 
                   hh_list_prior[, household_code]))
hh_list = data.table(household_code = hh_list)

# Merge the data sets together to create the updated hh_list -- so one row per data.table
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
# Remove if not keurig adopter and not observed to purchase ground coffee
hh_list = hh_list[!(hn==0&sn<=1&gsn==0), ]

# Flag panel_year as the year the first time the individual is observed to make a software purchase
hh_list_keurig[, days_to_sfirst:=as.numeric(sfirst_date - hfirst_date)]
# K-Cup Purchase happen 45 days before machine adoption
hh_list_keurig[days_to_sfirst < -45, days_to_sfirst:=NA]
# K-Cup Purchase happen before machine adoption
hh_list_keurig[days_to_sfirst < 0, days_to_sfirst:=0]
hh_list_keurig[days_to_sfirst > 180, days_to_sfirst:=NA]
hh_list_keurig[, k_first_date := ifelse(is.na(days_to_sfirst), sfirst_date, hfirst_date)]
hh_list_keurig[is.na(k_first_date), k_first_date := hfirst_date] # Only hardware adoption guys
hh_list_keurig[, k_first_date := as.Date(k_first_date, origin = "1970-01-01")]
hh_list_keurig[, k_first_week := wkend(k_first_date)]
hh_list_keurig[, panel_year := year(k_first_week)]
setkeyv(hh_list_keurig, c("household_code", "panel_year"))
# About 50% Purchase Keurig and Coffee at the same time or software purchase happen earlier than coffee purchase.
# About 60% Purchase within the first 2 weeks.

# Merge in hh information, and conduct regressions
cols = c("household_code", "panel_year", "projection_factor",
         "inc25", "inc40", "inc50", "inc60", "inc70", 
         "hhsize1", "hhsize2", "hhsize3", "hhsize5",
         "onefamily", "twofamily", "threefamily", 
         "fulltime", "notemployed", "partime",
         "head_age_numeric", "presence_of_children",
         "african_american", "hispanic", "cable", "internet",
         "panelist_zip_code", "fips_county_code", "fips_county_descr",
         "scantrack_market_code", "scantrack_market_descr", "dma_code", "dma_descr",
         setdiff(htnames, c("household_code", "panel_year")))

# Encoding and cleaning hh information
hh = hh[, cols, with=FALSE]
evars = c("household_code", "panel_year", "projection_factor", htnames)
lvars = setdiff(cols, evars)
for (v in lvars){
  setnames(hh, v, "vars")
  #if missing put 0, missing is extremely rare.
  hh[is.na(vars), vars:=0] 
  # hh[, vars:=factor(vars)]
  setnames(hh, "vars", v)
}
setkey(hh, household_code, panel_year)
setkey(hh_list_keurig, household_code, panel_year)

# Merge in hh info
hh_list_keurig = hh[hh_list_keurig]

# Run regression of hh characteristics
excl_set = c("household_code", "panel_year", "projection_factor",
             "panelist_zip_code", "fips_county_code", "fips_county_descr",
             "scantrack_market_code", "scantrack_market_descr", 
             "dma_code", "dma_descr", 
             setdiff(htnames, c("total_spent_hhi", "total_spent", "ntrips", "nretailers")))
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
  hh_temp = hh[as.integer(panel_year)==yr, .(household_code)]
  hh_temp[, presence:=as.integer(1)]
  setkey(hh_temp, household_code)
  hh_list = hh_temp[hh_list]
  hh_list[is.na(presence), presence:=as.integer(0)]
  newname = paste("y", yr, sep="")
  setnames(hh_list, "presence", newname)
  hnames=c(hnames, newname)
}
setcolorder(hh_list, hnames)

# Impute first adoption date based on the following rules
# 1. If software purchase happen 45 days before hardware purchase,
#    then, I regard the link to be too week, and impute adoption date.
# 2. Impute 3 scenarios - only if imputed_dates is missing - note no burnin if present in previous year.
#    2.1 No hardware observation -- use software date if software date>=first_date+burnin
#    2.2 No hardware observation -- use first date of the year if software date<first_date+burnin
#    2.3 Hardware observation happen after software observation (45d+) -- use first date of year
# impute_hdate --- store cases where hardware adoption can be imputed
# k_first_date --- after which date, treat K-Cup in the choice set

# Establish association between hardware and software sales only
# Check whether household is available in the next year after adoption
hh_list[, `:=`(hfirst_week=wkend(hfirst_date), hlast_week=wkend(hlast_date))]
hh_list[, panel_year := year(hfirst_week)+1]
setkey(hh_list, household_code, panel_year)
setkey(hh, household_code, panel_year)
hh_list = hh[,.(household_code, panel_year, projection_factor)][hh_list]
hh_list[, `:=`(imputed_hfirst = hfirst_date, imputed_hlast = hlast_date,
               avail_next_year = !(is.na(projection_factor)),
               panel_year=NULL, projection_factor=NULL)]
hh_list[kholder==0, avail_next_year:=NA]
hh_list[(sfirst_date - hfirst_date) < -45, `:=`(imputed_hfirst=NA, imputed_hlast=NA)]
#Most are not present in next year.
#hh_list[(sfirst_date - hfirst_date) > 180, `:=`(imputed_hfirst=NA, imputed_hlast=NA)] 
hh_list[, k_first_date:=imputed_hfirst]

# create panel year as the first software purchase year
hh_list[, `:=`(sfirst_week=wkend(sfirst_date), slast_week=wkend(slast_date))]
hh_list[, `:=`(panel_year=year(sfirst_week))]

# Considering burnin period, impute first and last hardware adoption
hh_list[, himputed:=0]
for (yr in ylist){
  first_date = purchases[panel_year==yr, min(purchase_date)]
  hh_list[panel_year==yr & is.na(imputed_hfirst) & sfirst_date>=(first_date+burnindays), 
          `:=`(imputed_hfirst = sfirst_date, imputed_hlast = sfirst_date, 
               k_first_date = sfirst_date, himputed = 1)]
  hh_list[panel_year==yr & is.na(k_first_date) & sfirst_date<(first_date+burnindays), 
          `:=`(k_first_date = first_date)] # In case of measurement error, underestimate K-Cup value.
  
  # If present in last year. Then, no need to burnin
  if(yr >= 2005){
    lastyn = paste("y", (yr-1), sep="")
    setnames(hh_list, lastyn, "lastyname")
    hh_list[panel_year==yr & is.na(imputed_hfirst) & as.integer(lastyname)==1, 
            `:=`(imputed_hfirst = sfirst_date, imputed_hlast = sfirst_date, 
                 k_first_date = sfirst_date, himputed = 1)]
    setnames(hh_list, "lastyname", lastyn)
  }
}
hh_list[sfirst_date<imputed_hfirst, k_first_date:=sfirst_date]
# If no adoption, then, shouldn't adopt 
hh_list[kholder==0, `:=`(imputed_hfirst=NA, imputed_hlast=NA, k_first_date=NA)]

# Obtain adoption year
hh_list[, `:=`(imputed_hfirst_week = wkend(imputed_hfirst), imputed_hlast_week=wkend(imputed_hlast),
               k_first_week = wkend(k_first_date))]
hh_list[, `:=`(adoption_panel_year=year(imputed_hfirst_week), panel_year=year(k_first_week))]

# Drop households who are not present during the years 2007, 2008, 2009, 2010, 2011, 2012, 2013. 
hh_list[, existence := (y2007+y2008+y2009+y2010+y2011+y2012+y2013)]
hh_list = hh_list[existence>0, ]
hh_list[, existence := (y2004+y2005+y2006+y2007+y2008+y2009+y2010+y2011+y2012+y2013)]

# Merge back into purchases to get consumption rate
setkey(purchases, household_code)
setkey(hh_list, household_code)
purchases = purchases[hh_list[, .(household_code, kholder, k_first_date, imputed_hfirst)], nomatch=0L]
setkey(purchases, household_code, purchase_date, trip_code_uc, upc, upc_ver_uc)

# If never adopted Keurig, let k_first_date be the last date of any purchase
max_date = purchases[, max(purchase_date)] + 7
purchases[is.na(k_first_date), k_first_date := max_date]

# Obtain purchases and purchase occasions
purchases[, adoption:=as.integer(purchase_date>=k_first_date)]
hh_purch = purchases[product_module_code==1463, .(quantity=sum(quantity*size1_amount), npurch=.N), 
                     by = c("household_code", "adoption")]

# reshape data
hh_purch_0 = hh_purch[as.integer(adoption)==0, ]
hh_purch_1 = hh_purch[as.integer(adoption)==1, ]
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

# Obtain the number of weeks before and after adoption
purchases[, week_end:=wkend(purchase_date)]
wk_dt = unique(purchases[, .(panel_year, week_end)])
setkey(wk_dt, panel_year, week_end)
setkey(wk_dt, panel_year)
setkey(hh, panel_year)
hh_wks = hh[,.(household_code, panel_year)][wk_dt, nomatch=0L, allow.cartesian=T]
setkey(hh_wks, household_code)
setkey(hh_list, household_code)
hh_wks = hh_list[,.(household_code, k_first_date, k_first_week)][hh_wks, nomatch=0L]
hh_wks[, `:=`(badopt=as.integer(week_end<k_first_date), adopt=as.integer(week_end>=k_first_date))]
hh_wks[, `:=`(badopt=ifelse(is.na(badopt), 1, badopt), adopt=ifelse(is.na(adopt), 0, adopt))]
hh_wks = hh_wks[, .(bweeks = sum(badopt), aweeks = sum(adopt)), by=c("household_code")]
setkey(hh_wks, household_code)
setkey(hh_list, household_code)
hh_list = hh_list[hh_wks, nomatch=0L]

# Get average consumption per-week
hh_list[, `:=`(brate = ifelse(bweeks==0 | is.na(bweeks), NA, bquantity/bweeks),
               arate = ifelse(aweeks==0 | is.na(aweeks), NA, aquantity/aweeks),
               overall_rate = rowSums(cbind(bquantity, aquantity), na.rm=TRUE)/(aweeks+bweeks))]

# The household has to make at least 2 purchases annually to be counted, 
# or at least consume 1 kcup on average per week. 
hh_list[, annual_purchases := (sn+gsn+prior_sn)/existence]
hh_list = hh_list[annual_purchases>=1.25 | (arate >= 1.25), ]

# Obtain the expenditure shares and reshape data as wide format
hh_brand_purch = purchases[, .(expenditure = sum(total_price_paid-coupon_value),
                               quantity = sum(quantity * size1_amount)), 
                           by = c("household_code", "adoption", "brand_descr")]
hh_brand_purch[, `:=`(expend_share = expenditure/sum(expenditure),
                      quan_share = quantity/sum(quantity),
                      expenditure = sum(expenditure)),
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
hh_info_panel[, panel_year:=NULL]
setnames(hh_info_panel, "i.panel_year", "panel_year")
save(hh_list, hh_info_panel, file = paste(meta_dir, "/HH-Holder-Flags.RData", sep=""))

#Clean up workspace
rm(trips)
gc()
# End of this script -------------------------------------------------------------------#
