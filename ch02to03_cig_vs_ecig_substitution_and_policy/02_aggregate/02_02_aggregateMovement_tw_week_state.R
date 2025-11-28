

if(exists("aggregateMovement") == FALSE){
  source("code/02_aggregate/02_func_aggregateMovement.R")
}

selectExpressions <- c(
  "product_module_code",
  "SUM((price/prmult)*units)",
  "SUM(units*size1_amount*multi)",
  "AVG((price/prmult)/(multi*size1_amount))",
  "AVG((price/prmult))",
  "AVG(price)",
  "STDEV((price/prmult)/(multi*size1_amount))",
  "STDEV((price/prmult))",
  "STDEV(price)"
)

selectNames <- c(
  "product_id",
  "total_sales",
  "total_volume",
  "avg_sum_price", #sum \equiv Smallest Unit of Measurement
  "avg_unit_price",
  "avg_price",
  "sd_sum_price",
  "sd_unit_price",
  "sd_price"
)



#-Twoway Aggregation by Week and State of---------------------------------------
byGroup <- c("fips_state_code")
lapply(c(prodID$tcigte, prodID$ecigte),
       function(productID){
         
         lapply(2006:2020, 
                function(year){
                  
                  if(productID == prodID$ecigte & year < 2013){
                    
                  } else {
                    
                    write_tsv(
                      aggregateMovement(year = year, productID = productID,
                                        freq = "week",
                                        bygroup = byGroup,
                                        x = selectExpressions,
                                        xNames = selectNames),
                      path = paste0(direcs$niq.agg,
                                    "/twoway/week/state/tw_wk_st_", 
                                    productID, "_", year, ".tsv")
                    )
                    
                  }
                  
                }
         )
         
       }
       
)

#-Create Master Week-State File-------------------------------------------------
write_tsv(
  (list.files(
    path = paste0(direcs$niq.agg, "twoway/week/state/"), pattern = ".tsv",
    full.names = TRUE
  ) %>%
    lapply(read_tsv, col_names = TRUE) %>%
    do.call(rbind, .)),
  path = paste0(direcs$niq.agg, "twoway/week/tw_wk_st_master.tsv")
)

#-Twoway Aggregation by Week and Brand------------------------------------------
byGroup <- c("brand_code_uc")
lapply(c(prodID$tcigte, prodID$ecigte),
       function(productID){
         
         lapply(2006:2020, 
                function(year){
                  
                  if(productID == prodID$ecigte & year < 2013){
                    
                  } else {
                    
                    write_tsv(
                      aggregateMovement(year = year, productID = productID,
                                        freq = "week",
                                        bygroup = byGroup,
                                        x = selectExpressions,
                                        xNames = selectNames),
                      path = paste0(direcs$niq.agg,
                                    "/twoway/week/brand/tw_wk_br_", 
                                    productID, "_", year, ".tsv")
                    )
                    
                  }
                  
                }
         )
         
       }
       
)

#-Create Master Week-State File-------------------------------------------------
write_tsv(
  (list.files(
    path = paste0(direcs$niq.agg, "twoway/week/brand/"), pattern = ".tsv",
    full.names = TRUE
  ) %>%
    lapply(read_tsv, col_names = TRUE) %>%
    do.call(rbind, .)),
  path = paste0(direcs$niq.agg, "twoway/week/tw_wk_br_master.tsv")
)


