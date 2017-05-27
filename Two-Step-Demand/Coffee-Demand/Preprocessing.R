# Make nprod and nbrand to logs
hh_market_prod[, `:=`(nbrand = log(nbrand), nprod = log(nprod))]

# Sample 5% of purchasing trips
t_list = hh_market_prod[, unique(t)]
nsamp = ceiling(sratio*length(t_list))
sample_t = sample(t_list, nsamp)
hh_samp = hh_market_prod[t%in%sample_t, ]

# Reassign hh and t
setkey(hh_samp, household_code, t, brand_descr, keurig, roast, brand_descr_orig)
hh_samp[, hh := .GRP, by = "hh"]
setkey(hh_samp, hh, t)
hh_samp[, t := .GRP, by = "t"]

# Reset Key
setkey(hh_samp, hh, t, brand, roast)

# Create the Design Matrix
BMat = as.matrix(hh_samp[, bnames, with=FALSE])
XMat = as.matrix(hh_samp[, xnames, with=FALSE])
#Create a K matrix to allow different satiation rate for Keurig and Ground 
KMat = hh_samp[, cbind(outside, 1-keurig-outside, keurig)]

# Obtain the M --- Number of items chosen in the trip
hh_samp[, `:=`(M = sum(purchased)), by = c("household_code", "t")]

# Create Expenditure based on size and aggregate price -- not actual price (Robustness check needed)
hh_samp[, expenditure := price * size]
# Or
# hh_samp[, expenditure := total_price_paid - coupon_value] # correlation is high.
# Create log price and log expenditure part
hh_samp[, `:=`(lprice=log(price), lexpend=log(expenditure/price+1))]

save(hh_samp, nsamp, bnames, xnames, XMat, KMat, file = paste(input_dir, "/HH-Aux-Market.RData", sep=""))
