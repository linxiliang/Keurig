#Functions

#Time Trend Plot when first recorded purchase is on site.
#Then plot 4 things, number of units purchased, no.upcs/unit, no.brands/unit, and average pack size.
TimeTrendPlot<-function(dt, min.period=12, days=90, cond=TRUE, platform="ALL"){
  dt = copy(dt)
  if (toupper(platform)!="ALL") dt = dt[ptype==platform]
  dt[, first_date := min(purchase_date), by = "household_code"]
  dt[, period:=ceiling(as.numeric(purchase_date-first_date+1)/days)]
  dt[, n_period:=max(period), by = "household_code"]
  if (cond){
    dt = dt[n_period>=min.period, ]
    fname = paste("/figs/hh_purch_trend_",platform,"_cond_",min.period,".pdf", sep="")
  } else{
    fname = paste("/figs/hh_purch_trend_",platform,"_",min.period,".pdf", sep="")
  }
  setkey(dt, NULL)
  upcs_tried = unique(dt[, .(household_code, period, upc, upc_ver_uc)])
  setkey(upcs_tried, household_code, upc, upc_ver_uc, period)
  upcs_tried[, first:=ifelse((1:length(period))==1, 1, 0), 
               by=c("household_code", "upc", "upc_ver_uc")]   
  upcs_tried = upcs_tried[, .(n_upcs=sum(first)),
                          by = c("household_code", "period")]
  brands_tried = unique(dt[, .(household_code, period, brand_descr)])
  setkey(brands_tried, household_code, brand_descr, period)
  brands_tried[, first:=ifelse((1:length(period))==1, 1, 0), 
               by=c("household_code", "brand_descr")]
  brands_tried = brands_tried[,.(n_brands=sum(first)),
                              by = c("household_code", "period")] 
  purch_level = dt[, .(n_units = sum(quantity), 
                        n_packs = sum(quantity*size1_amount)), 
                        by = c("household_code", "period")]
  setkey(upcs_tried, household_code, period)
  setkey(brands_tried, household_code, period)
  setkey(purch_level, household_code, period)
  time_pattern = as.data.table(expand.grid(household_code=unique(purch_level[,household_code]),
                                           period = unique(purch_level[, period])))
  setkey(time_pattern, household_code, period)
  time_pattern = brands_tried[time_pattern]
  time_pattern = upcs_tried[time_pattern]
  time_pattern = purch_level[time_pattern]
  NAto0<-function(x) ifelse(is.na(x),0,x)
  NOInf<-function(x) ifelse(is.infinite(x),as.numeric(NA),x)
  #time_pattern = time_pattern[, lapply(.SD, NAto0)]
  time_pattern[, `:=`(n_units=NAto0(n_units), n_packs=NAto0(n_packs))]
  time_pattern = time_pattern[, .(n_units=mean(n_units),
                                  n_units.sd=sd(n_units),
                                  n_units.se=sd(n_units)/sqrt(.N),
                                  n_upcs=mean(n_upcs, na.rm=TRUE),
                                  n_upcs.sd=sd(n_upcs, na.rm=TRUE),
                                  n_upcs.se=sd(n_upcs, na.rm=TRUE)/sqrt(length(na.omit(n_upcs))),
                                  n_brands=mean(n_brands, na.rm=TRUE),
                                  n_brands.sd=sd(n_brands, na.rm=TRUE),
                                  n_brands.se=sd(n_brands, na.rm=TRUE)/sqrt(length(na.omit(n_brands))),
                                  n_packs=mean(NOInf(n_packs/n_units), na.rm=TRUE),
                                  n_packs.sd=sd(NOInf(n_packs/n_units), na.rm=TRUE),
                                  n_packs.se=sd(NOInf(n_packs/n_units), na.rm=TRUE)/
                                            sqrt(length(na.omit(NOInf(n_packs/n_units))))),
                             , by = "period"]
  setkey(time_pattern, period)
  time_pattern = time_pattern[period<=min.period]
  #Plotting and export as pdf file
  pdf(file=paste(graph_dir, fname, sep=""), width=14, height=10)
  par(mfrow=c(2,2))
  plot(time_pattern[, period], time_pattern[, n_units], 
       ylim=c(min(time_pattern[, (n_units-n_units.se)]),max(time_pattern[, (n_units+n_units.se)])),
       type="o", xlab = paste("Period (Per ", days, " Days)",sep=""), ylab="Units")
  lines(time_pattern[, period], time_pattern[, (n_units+n_units.se)],  lty=2)
  lines(time_pattern[, period], time_pattern[, (n_units-n_units.se)],  lty=2)
  plot(time_pattern[, period], time_pattern[, n_upcs], 
       ylim=c(min(time_pattern[, (n_upcs-n_upcs.se)]),max(time_pattern[, (n_upcs+n_upcs.se)])),
       type="o", xlab = paste("Period (Per ", days, " Days)",sep=""), ylab="No of New UPCs")
  lines(time_pattern[, period], time_pattern[, (n_upcs+n_upcs.se)],  lty=2)
  lines(time_pattern[, period], time_pattern[, (n_upcs-n_upcs.se)],  lty=2)
  plot(time_pattern[, period], time_pattern[, n_brands], 
       ylim=c(min(time_pattern[, (n_brands-n_brands.se)]),max(time_pattern[, (n_brands+n_brands.se)])),
       type="o", xlab = paste("Period (Per ", days, " Days)",sep=""), ylab="No of New Brands")
  lines(time_pattern[, period], time_pattern[, (n_brands+n_brands.se)],  lty=2)
  lines(time_pattern[, period], time_pattern[, (n_brands-n_brands.se)],  lty=2)
  plot(time_pattern[, period], time_pattern[, n_packs], 
       ylim=c(min(time_pattern[, (n_packs-n_packs.se)]),max(time_pattern[, (n_packs+n_packs.se)])),
       type="o", xlab = paste("Period (Per ", days, " Days)",sep=""), ylab="Mean Pack Size")
  lines(time_pattern[, period], time_pattern[, (n_packs+n_packs.se)],  lty=2)
  lines(time_pattern[, period], time_pattern[, (n_packs-n_packs.se)],  lty=2)
  dev.off()
  par(mfrow=c(1,1))
}

PlatformTrend <- function(dt, days=90, start=as.Date("2004-01-01")){
  dt = copy(dt)
  dt[, period:=start + days*(ceiling(as.numeric(purchase_date-start+1)/days)-1)]
  period_purch=dt[,.(units=.N*sum(quantity*projection_factor)/sum(projection_factor),
                     packs=.N*sum(quantity*size1_amount*projection_factor)/
                       sum(projection_factor),
                     spent=.N*sum(projection_factor*(total_price_paid-coupon_value))/
                       sum(projection_factor),
                     N_prods=length(unique(upc*10+upc_ver_uc)),
                     N_brands=length(unique(brand_descr)),
                     N_hh=length(unique(household_code))),
                  by = c("ptype", "period")]
  period_purch[, `:=`(price = spent/packs)]
  
  #Compute concentration ratio and HHI of UPCs and Brands by platform
  upc_share = dt[,.(spent=.N*sum(projection_factor*(total_price_paid-coupon_value))/
                         sum(projection_factor)),
                    by = c("ptype", "period", "upc", "upc_ver_uc")]
  upc_share[, share:=spent/sum(spent), by = c("ptype", "period")]
  upc_share = upc_share[order(-share), ]
  setkey(upc_share, ptype, period)
  upc_share = upc_share[, .(cprod5=sum(share[1:min(length(share), 5)]),
                            prodHHI=sum(share[1:min(length(share), 10)]^2)), 
                        by = c("ptype", "period")]
  
  brand_share = dt[,.(spent=.N*sum(projection_factor*(total_price_paid-coupon_value))/
                        sum(projection_factor)),
                   by = c("ptype", "period", "brand_descr")]
  brand_share[, share:=spent/sum(spent), by = c("ptype", "period")]
  brand_share = brand_share[order(-share), ]
  setkey(brand_share, ptype, period)
  brand_share = brand_share[, .(cbrand3=sum(share[1:min(length(share), 3)]),
                                brandHHI=sum(share[1:min(length(share), 3)]^2)), 
                            by = c("ptype", "period")]  

  market_share = brand_share[upc_share]
  setkeyv(period_purch, c("ptype", "period"))
  period_purch = period_purch[market_share]
  types = c("KEURIG", "TASSIMO", "SENSEO", "GUSTO")
  #Plotting and export as pdf file 
  vars = c("packs", "price", "N_prods", "N_brands", 
           "N_hh", "cprod5", "prodHHI", "cbrand3", "brandHHI")
  for (v in vars){
    yname = ifelse(v=="price", "Price",
                   ifelse(v=="N_brands", "No. of brands", 
                          ifelse(v=="cprod5", "Concentration ratio - Top 5 UPCs", 
                                 ifelse(v=="prodHHI", "HHI (Limit to top 10 UPCs",
                                        ifelse(v=="cbrand3", "Concentration ratio - Top 3 Brands",
                                               ifelse(v=="brandHHI", "HHI (Limit to 3 Brands)", 
                                                      "OTHER"))))))
    if(yname=="OTHER"){
      period_purch[, c(v):=log10(eval(parse(text=v)))]
    }
    yname = ifelse(v=="packs", "Packs (log 10)", 
                   ifelse(v=="N_prods", "No. of UPCs (log 10)", 
                          ifelse(v=="N_hh", "No. of HH making a purchase (log 10)", yname)))
    pdf(file=paste(graph_dir, "/figs/",v,"_consume_by_platform.pdf", sep=""), width=14, height=10)
    par(mfrow=c(2,2))
    ymin = min(period_purch[, v, with=FALSE])
    ymax = max(period_purch[, v, with=FALSE])
    xmin = min(period_purch[, period])
    xmax = max(period_purch[, period])
    for(type in types){
      print(period_purch[ptype==type, 
                         plot(period, eval(parse(text=v)), main=type, type="o", ylab=yname,
                              xlab = paste("Period (Per ", days, " Days)",sep=""),
                              xlim = c(xmin, xmax), ylim=c(ymin, ymax))])
    }
    dev.off()
  }
  par(mfrow=c(1,1))
}
