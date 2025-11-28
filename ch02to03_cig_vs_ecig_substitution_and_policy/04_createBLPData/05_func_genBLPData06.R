genBLPDataStructure06_aggToBrand <- function(db, method = "weight",
                                                   productID, year){
  
  if(method == "weight"){
    df <- dbGetQuery(db, paste0("SELECT * FROM blp_df_02_", productID, "_", year)) %>%
      group_by(brand_code_uc, year, quarter, fips_state_code,
               fips_county_code) %>%
      dplyr::mutate(price_wvol = sum(price_wvol*(volume/brand_mkt_vol)),
             price_wsale = sum(price_wsale*(sales/brand_mkt_sales)),
             display_wvol = sum(display_wvol*(volume/brand_mkt_vol)),
             feature_wvol = sum(feature_wvol*(volume/brand_mkt_vol)),
             display_wsale = sum(display_wsale*(sales/brand_mkt_sales)),
             feature_wsale = sum(feature_wsale*(sales/brand_mkt_sales))
      ) %>%
      select(brand_code_uc, year, quarter, fips_state_code, fips_county_code,
             price_wvol, price_wsale, brand_mkt_vol, brand_mkt_sales,
             display_wvol, feature_wvol, display_wsale, feature_wsale) %>%
      distinct()
  } else if(method == "arith"){
    df <- dbGetQuery(db, paste0("SELECT * FROM blp_df_02_", productID, "_", year)) %>%
      group_by(brand_code_uc, year, quarter, fips_state_code,
               fips_county_code) %>%
      dplyr::mutate(price_wvol_avg = mean(price_wvol),
             price_wsales_avg = mean(price_wsales),
             display_wvol_avg = mean(display_wvol),
             feature_wvol_avg = mean(feature_wvol),
             display_wsale_avg = mean(display_wsale),
             feature_wsale_avg = mean(feature_wsale)
      ) %>%
      dplyr::select(brand_code_uc, year, quarter, fips_state_code, fips_county_code,
             price_wvol_avg, price_wsale_avg, brand_mkt_vol, brand_mkt_sales,
             display_wvol_avg, feature_wvol_avg, display_wsale_avg,
             feature_wsale_avg) %>%
      distinct()
  }
  
  dbWriteTable(knd.blp.int,
               paste0("blp_df_03_", productID, "_", year),
               df,
               overwrite = TRUE)
  
  print(c(productID, " ", year))
  
  
}



