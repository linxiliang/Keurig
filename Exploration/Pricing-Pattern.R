pdf(file="~/PricePlot.pdf")
national[.(88133400046, 1), plot(week_end, imputed_price_hybrid)]
national[.(9955508520, 2), plot(week_end, imputed_price_hybrid)]

national[.(2550000367, 1), plot(week_end, unit, type="l")]
dev.off()

move[.(64964500402),][order(unit), min(week_end)]
move[.(64964500402),][order(unit), ]

move[.(64964500402, 1, 4842607), {plot(week_end, base_price_hybrid, type="l")
     plot(week_end, imputed_price_hybrid, type="l")
     plot(week_end, unit, type="l")}]

move[.(64964500402, 1, 4842607), {plot(week_end, base_price_hybrid, type="l")
                                  plot(week_end, imputed_price_hybrid, type="l")
                                  plot(week_end, unit, type="l")}]
require(data.table)

national = move[, list(imputed_price_hybrid = mean(imputed_price_hybrid, na.rm=TRUE),
                       base_price_hybrid = mean(base_price_hybrid, na.rm=TRUE),
                       unit = sum(unit, na.rm=TRUE)), by = c("upc", "upc_ver_uc", "week_end")]
setkeyv(national, c("upc", "upc_ver_uc", "week_end"))                  
                       

#Keep All Single Cup Related Coffee, and the major brands including
#Folgers, Maxwell House, Dunkin' Donut, Starbucks Coffee

#Kakula K-Cup 18 CT is in OZ, so convert it

move_upc_table[upc==9955501800&upc_ver_uc==1, `:=`(size1_units=CT, size1_amount=18)]
move_upc_table = move_upc_table[grepl("KEURIG", brand_descr)|grepl("KURG", brand_descr)|
                                  grepl("STARBUCKS", brand_descr)|grepl("DUNKIN", brand_descr)|
                                  grepl("FOLGERS", brand_descr)|grepl("MAXWELL", brand_descr)|
                                  size1_units=="CT", ]

selected_upcs = move_upc_table[, .(upc, upc_ver_uc)]
setkeyv(selected_upcs, c("upc", "upc_ver_uc"))
move = selected_upcs[move, nomatch=0L]

save(move, move_upc_table, file="~/Coffee.RData")



#Obtain the aggregate price by product
national = move[, `:=`(imputed_price_hybrid = mean(imputed_price_hybrid, na.rm=TRUE),
                       base_price_hybrid = mean(base_price_hybrid, na.rm=TRUE),
                       unit = sum(unit, na.rm=TRUE)), by = c("upc", "upc_ver_uc", "week_end")]
setkeyv(national, c("upc", "upc_ver_uc", "week_end"))                  

move[.(2550000367, 1), plot(week_end, imputed_price_hybrid)]
move[.(2550000367, 1), plot(week_end, unit, type="l")]


#Keurig Machine Sales Data
move[.(64964500402, 1), plot(week_end, imputed_price_hybrid)]
move[.(64964500402, 1), plot(week_end, unit)]

move[.(82522588213, 1), plot(week_end, imputed_price_hybrid)]

#K-Cups
head(move_upc_table[order(-revenue_RMS), ][size1_units=="CT", .(upc, upc_ver_uc, upc_descr, 
                                                                brand_descr, revenue_RMS, 
                                                                N_weeks_observed_RMS)], 50)

national[.(9955515508, 2), plot(week_end, imputed_price_hybrid, type="l")]

