genBLPDataStructure03_combAgg <- function(type){
  
  sales <- list.files(
    paste0(direcs$der.niq, "blp/sales/", type, "_sales/"),
    full.names = TRUE
  ) %>%
    lapply(read_tsv) %>%
    do.call(rbind, .)
  
  volume <- list.files(
    paste0(direcs$der.niq, "blp/volume/", type, "_vol/"),
    full.names = TRUE
  ) %>%
    lapply(read_tsv) %>%
    do.call(rbind, .)
  
  if(type == "inside_brand_market"){
    merge_vars <- c("productID", "year", "brand_code_uc",
                    "quarter", "fips_state_code",
                    "fips_county_code")
  } else if(type == "inside_brand_quarter"){
    merge_vars <- c("productID", "year", "brand_code_uc", "quarter")
  } else if(type == "inside_brand"){
    merge_vars <- c("productID", "year", "brand_code_uc")
  } else if(type == "inside_market"){
    merge_vars <- c("productID", "year", "quarter", "fips_state_code",
                    "fips_county_code")
  } else if(type == "total_all"){
    merge_vars <- c("productID", "year")
  }
   
  both <- merge(sales, volume, by = merge_vars)
  
  write_tsv(both,
            paste0(
              direcs$der.niq, "blp/master/", type, ".tsv"
            )
  )
  
  
  
}
