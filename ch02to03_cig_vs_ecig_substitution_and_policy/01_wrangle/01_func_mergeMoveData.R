#This function provides a means for automating the merging
#of movement files with the stores and products files.

mergeMoveData <- function(db, productID, year, overwrite = FALSE){
  
  writeSQL <- function(productID, year){
    
    createStatement <- paste("CREATE TABLE", 
                             paste0("rms_mg_", productID, "_", year), "AS")
    
    topSelectStatement <-  "SELECT panel_year, week_end, R.upc, R.upc_ver_uc, price, 
                              prmult, units, feature, display, fips_state_code,
                              fips_county_code, channel_code, 
                              product_module_code, brand_code_uc, multi, 
                              size1_amount, M.store_code_uc"
    
    #First part writes code for merging movement with rms_versions
    topFromStatement <- paste(
      "FROM", paste0("rms_", productID, "_", year), "AS M",
      sep = " "
    )
    
    joinStatement.1 <- paste("INNER JOIN", paste0("rms_rms_versions_", year),
                             "AS R", sep = " ")
    
    onStatement.1 <- paste("ON M.upc = R.upc")
    
    #Next, writes code for merging with stores
    subSelectStatement.1 <- "(SELECT store_code_uc, fips_state_code, 
                              fips_county_code, channel_code"
    
    subFromStatement.1 <- paste("FROM", paste0("rms_stores_", year),
                                ") AS S")
    
    joinStatement.2 <- paste("INNER JOIN", subSelectStatement.1,
                             subFromStatement.1)
    onStatement.2 <- "ON M.store_code_uc = S.store_code_uc"
    
    #Last part writes code for merging with products
    subSelectStatement.2 <- "(SELECT upc, upc_ver_uc, product_module_code,
                                brand_code_uc, multi, size1_amount"
    subFromStatement.2 <- "FROM rms_products"
    subWhereStatement.1 <- paste("WHERE product_module_code =", productID, ")
                                 AS P",
                                 sep = " ")
    joinStatement.3 <- paste("INNER JOIN", subSelectStatement.2,
                             subFromStatement.2, subWhereStatement.1,
                             sep = " ")
    onStatement.3 <- "ON R.upc = P.upc AND R.upc_ver_uc = P.upc_ver_uc"
    
    #Finally, put everything together
    query <- paste(createStatement, topSelectStatement, topFromStatement, 
                   joinStatement.1, onStatement.1, joinStatement.2, 
                   onStatement.2, joinStatement.3, onStatement.3)
    return(gsub("\n", "", query))
    
    
  }
  query <-  writeSQL(productID, year)
  
  if(overwrite == TRUE){
    lapply(year, function(y) dbRemoveTable(tob, paste0("rms_mg_", productID, "_", y)))
  }
  
  lapply(query, function(q){dbExecute(tob, q)})
  
}

