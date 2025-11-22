genBLPDataStructure02_sumSales <- function(productID, year, s_bar, db){
  
  dbGetQuery(db,
             paste0("SELECT * FROM blp_df_01_", productID, "_", year)
  ) %>%
    group_by(brand_code_uc) %>%
    dplyr::mutate(annual_brand_sales = sum(sales),
                  annual_brand_vol = sum(volume)) %>%
    ungroup() %>%
    group_by(brand_code_uc, quarter) %>%
    dplyr::mutate(brand_quarter_sales = sum(sales),
                  brand_quarter_vol = sum(volume)) %>%
    ungroup() %>%
    group_by(brand_code_uc, quarter, fips_state_code, fips_county_code) %>%
    dplyr::mutate(brand_mkt_sales = sum(sales),
                  brand_mkt_vol = sum(volume)) %>%
    ungroup() %>%
    plyr::mutate(annual_all_sales = sum(sales),
                 annual_all_vol = sum(volume)) %>%
    dplyr::mutate(share_sales = annual_brand_sales/annual_all_sales,
                  share_vol = annual_brand_vol/annual_all_vol) %>%
    group_by(fips_state_code, fips_county_code, quarter) %>%
    dplyr::mutate(total_mkt_sales = sum(sales),
                  total_mkt_vol = sum(volume)) %>%
    filter(share_vol >= s_bar) %>%
    mutate(productID = productID, year = year)
             
  
}
