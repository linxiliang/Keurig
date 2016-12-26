# Adjust regular roast and flavor to the correct roast level 
CheckDigitGenerator <- function(x){
  multiplier = c(rep(c(3,1), 6), 3)
  upcdt = data.table(upc=as.character(x))
  upcdt[, `:=`(nupc=nchar(upc), id = 1:.N)]
  upcdt[, upc_str := ifelse(nupc<13, paste(paste(rep("0", (13 - nupc)), collapse=""), upc,  sep=""), upc),
        by = "id"]
  upcdt[, upcsummod := sum(as.numeric(unlist(strsplit(upc_str, split=""))) * multiplier) %% 10, by = "id"]
  upcdt[, check_digit := ifelse(upcsummod == 0, 0, 10-upcsummod)]
  upcdt[, upc_full := paste(upc_str, check_digit, sep="")]
  return(list(check_digit = upcdt$check_digit, upc_full = upcdt$upc_full))
}

products[, upc_full:=CheckDigitGenerator(upc)$upc_full]
reg_adjust = products[flavor_descr=="REGULAR" & style_descr=="CLASSIC MDR M-GR"&product_module_code==1463,
                      ][order(-HMS_share), .(upc, upc_full, upc_descr, brand_descr, 
                                             HMS_cum_share, HMS_share, type_descr)]
write.csv(reg_adjust, file=paste(output_dir, "/regular_adjustment.csv", sep = ""), row.names = F)


# Obtain the list of popular flavor and style combinations. 
flavor_style = products[product_module_code==1463, .(nobs = .N, HMS_share = sum(HMS_share)), 
                        by = c("flavor_descr", "style_descr")]
flavor_style = flavor_style[order(-nobs), ]
flavor_style[, HMS_cum_share := cumsum(HMS_share)]
write.csv(flavor_style, file=paste(output_dir, "/flavor_style.csv", sep = ""), row.names = F)



reg_adjust = products[flavor_descr=="REGULAR" & style_descr=="100% CLN"&product_module_code==1463,
                      ][order(-HMS_share), .(upc, upc_full, upc_descr, brand_descr, 
                                             HMS_cum_share, HMS_share, type_descr)]
