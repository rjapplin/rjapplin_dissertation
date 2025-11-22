

if(exists("aggregateMovement") == FALSE){
  source("code/02_aggregate/02_func_aggregateMovement.R")
}

#-Specify variables for select statement to use with aggregateMovement()--------

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
  "avg_sum_price", #Average Smallest Unit of Measurement Price
  "avg_unit_price",
  "avg_price",
  "sd_sum_price",
  "sd_unit_price",
  "sd_price"
)

#-One-way Aggregation of Cigarette Data-----------------------------------------
lapply(
  2006:2020,
  function(year){
    write_tsv(aggregateMovement(year, productID = prodID$tcigte, freq = "week",
                                bygroup = NULL, x = selectExpressions,
                                xNames = selectNames),
              path = paste0(direcs$niq.agg, "/oneway/weekly/individual/", 
                            prodID$tcigte, "_", year, "_ow_wk.tsv")
    )
  }
)

#-One-way Aggregation of E-Cigarette Data---------------------------------------
lapply(
  2013:2020,
  function(year){
    write_tsv(aggregateMovement(year, productID = prodID$ecigte, freq = "week",
                                bygroup = NULL, x = selectExpressions,
                                xNames = selectNames),
              path = paste0(direcs$niq.agg, "/oneway/weekly/individual/", 
                            prodID$ecigte,  "_", year, "_ow_wk.tsv"))
  }
)


#-Create Master Weekly File-----------------------------------------------------
write_tsv(
  (ow_master_wk <- list.files(
    path = paste0(direcs$niq.agg, "oneway/weekly/"), pattern = ".tsv",
    full.names = TRUE
  ) %>%
    lapply(read_tsv, col_names = TRUE) %>%
    do.call(rbind, .)
  ),
  path = paste0(direcs$niq.agg, "oneway/weekly/ow_wk_")
)







#-Oneway Aggregation by Year: Number of Distinct Products-----------------------
selectExpressions <- c("product_module_code",
                       "panel_year",
                       "COUNT(DISTINCT upc || upc_ver_uc)")
selectNames <-c("product_id",
                "year",
                "distinct_products")
lapply(c(prodID$tcigte, prodID$ecigte),
       function(productID){
         
         lapply(2006:2020, 
                function(year){
                  
                  if(productID == prodID$ecigte & year < 2013){
                    
                  } else {
                    
                    write_tsv(
                      aggregateMovement(year = year, productID = productID,
                                        freq = "year",
                                        bygroup = NULL,
                                        x = selectExpressions,
                                        xNames = selectNames),
                      path = paste0(direcs$niq.agg,
                                    "/oneway/year/distinct_products/ow_yr_dp_", 
                                    productID, "_", year, ".tsv")
                    )
                    
                  }
                  
                }
         )
         
       }
       
)

#-Oneway Master File for Distinct Products per Year-----------------------------
write_tsv(
  (ow_master_wk <- list.files(
    path = paste0(direcs$niq.agg, "oneway/year/distinct_products/"),
    pattern = ".tsv",
    full.names = TRUE
  ) %>%
    lapply(read_tsv, col_names = TRUE) %>%
    do.call(rbind, .)
  ),
  path = paste0(direcs$niq.agg, "oneway/year/ow_yr_dp_master.tsv")
)
