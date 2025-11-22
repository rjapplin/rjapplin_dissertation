genBLPDataStructure05_addMktSizetoDB <- function(freq = "quarter",
                                                 type = "weight", db){
  
read_tsv(
  paste0(direcs$der.niq, "blp/master/master_max_market_size.tsv")
  ) %>% 
    select(fips_state_code, fips_county_code, market_size_vol,
           market_size_sales, market_size_real_sales) %>%
    distinct() %>%
    dbWriteTable(db, paste0("market_size_", freq, "_", type), .,
                 overwrite = TRUE)
  
  
}
