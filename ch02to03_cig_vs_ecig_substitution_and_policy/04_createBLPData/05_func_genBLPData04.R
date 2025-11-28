genBLPDataStructure04_compSize <- function(type = "inside_market"){
  
  paste0(direcs$der.niq, "blp/master/", type, ".tsv") %>%
    read_tsv() %>%
    group_by(year, quarter, fips_state_code, fips_county_code) %>%
    dplyr::summarise(total_mkt_sales = sum(total_mkt_sales),
                     total_mkt_vol = sum(total_mkt_vol)) %>%
    ungroup() %>%
    dplyr::inner_join(
      read_tsv(
        paste0(direcs$dat.ext, "fred/cpi_qtr.tsv")
      ),
      by = c("year", "quarter")
    ) %>%
    dplyr::mutate(real_total_mkt_sales = total_mkt_sales/(cpi_qtr/100)) %>%
    group_by(fips_state_code, fips_county_code) %>%
    dplyr::mutate(market_size_vol = max(total_mkt_vol),
                  market_size_sales = max(total_mkt_sales),
                  market_size_real_sales = max(real_total_mkt_sales)) %>%
    dplyr::mutate(time_of_max_vol = ifelse(total_mkt_vol == market_size_vol,
                                           TRUE, FALSE),
                  time_of_max_sales = ifelse(total_mkt_sales == market_size_sales,
                                             TRUE, FALSE),
                  time_of_max_real = ifelse(
                    real_total_mkt_sales == market_size_real_sales,
                    TRUE, FALSE)
    ) -> mkt_size_df
  
 
  mkt_size_df %>%
    filter(time_of_max_vol == TRUE) %>%
    dplyr::select(fips_state_code, fips_county_code,
                  market_size_vol) %>%
    distinct() -> market_size_vol
  
  mkt_size_df %>%
    filter(time_of_max_sales == TRUE) %>%
    dplyr::select(fips_state_code, fips_county_code,
                  market_size_sales) %>%
    distinct() -> market_size_sales
  
  mkt_size_df %>%
    filter(time_of_max_real == TRUE) %>%
    dplyr::select(fips_state_code, fips_county_code,
                  market_size_real_sales) %>%
    distinct() -> market_size_real
  
  market_size <- merge(market_size_vol, market_size_sales,
                       c("fips_state_code",
                         "fips_county_code")
  ) %>%
    merge(market_size_real, c("fips_state_code",
                              "fips_county_code")
    )

  
  write_tsv(mkt_size_df, 
            paste0(direcs$der.niq,
                   "blp/master/master_market_size.tsv"))
  
  write_tsv(market_size,
            paste0(direcs$der.niq,
                   "blp/master/master_max_market_size.tsv"))

  

  
}


