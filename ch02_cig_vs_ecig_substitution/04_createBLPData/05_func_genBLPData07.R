genBLPDataStructure07_mergeInMktSize <- function(productID, year,
                                                 freq = "quarter",
                                                 type = "weight", db){
  
 
  df <- dbGetQuery(db,
                   paste0(
                     "SELECT brand_code_uc, year, quarter, A.fips_state_code, A.fips_county_code, 
                     price_wvol, price_wsale, brand_mkt_vol, brand_mkt_sales,
                     display_wvol, feature_wvol, display_wsale, feature_wsale, B.market_size AS market_size_vol,
                     C.market_size AS market_size_real FROM blp_df_03_", productID, "_", year, " AS A",
                     " INNER JOIN market_size_", freq, "_", type, "_volume AS B
                      ON A.fips_state_code = B.fips_state_code AND A.fips_county_code = B.fips_county_code
                      INNER JOIN market_size_", freq, "_", type, "_real AS C 
                     ON B.fips_state_code = C.fips_state_code AND B.fips_county_code = C.fips_county_code"
                   )) 

  dbWriteTable(knd.blp.int, paste0("blp_df_04_", productID, "_", year),
               df, overwrite = TRUE)
  
  
  
}  
