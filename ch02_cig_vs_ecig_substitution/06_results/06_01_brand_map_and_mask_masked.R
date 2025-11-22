if("brand_map_and_mask" %in% dbListTables(results)){
 brandMap <- dbReadTable(results, "brand_map_and_mask") 
} else {

#Brand Map----------------------------------------------------------------------
  brandMap <- data.frame(brand = brandSort) %>%
    mutate(
      broader_brand = 
        case_when(
          "MASKED"
          .default = brand
        )
    ) %>%
    dplyr::mutate(brand_id = seq_along(brand)-1) %>%
    group_by(broader_brand) %>%
    dplyr::mutate(broader_brand_id = seq_along(broader_brand)-1) %>%
    dplyr::mutate(broader_brand_id = brand_id - broader_brand_id)
    
#Identify Top 5 Brands (by category) in 2020------------------------------------
  brands_to_keep_unmasked <- df2 %>%
    inner_join(brandMap, by = "brand") %>%
    select(brand, cdid, brand, broader_brand, brand_id, broader_brand_id, inShare,
           year, category) %>%
    filter(year == 2020) %>%
    group_by(cdid, broader_brand, broader_brand_id, category) %>%
    dplyr::summarize(inShare = sum(inShare)) %>%
    ungroup() %>%
    group_by(category, broader_brand, broader_brand_id) %>%
    dplyr::summarize(inShare = mean(inShare)) %>%
    arrange(category, inShare) %>%
    group_by(category) %>%
    dplyr::mutate(rank = rank(inShare),
                  n = n()) %>%
    dplyr::mutate(rank = n - (rank-1)) %>%
    select(category, broader_brand, broader_brand_id, inShare, rank) %>%
    filter(rank <= 5) %>%
    ungroup() %>%
    select(broader_brand_id) %>%
    mutate(unmask = T)
  
  #Identify Whether to Mask or Not------------------------------------------------
  brandMap <- brandMap %>%
    left_join(brands_to_keep_unmasked,
              by = "broader_brand_id") %>%
    mutate(unmask = ifelse(is.na(unmask), FALSE, unmask))

#Masking------------------------------------------------------------------------
  brandMap <- brandMap %>%
    mutate(unmask = ifelse(brand_id == 0, T, unmask)) %>%
    mutate(inSample = brand %in% allBrands) %>%
    filter(inSample == T) %>%
    select(-inSample) %>%
    mutate(MASKED_NAME = case_when(
      unmask == T ~ broader_brand,
      "MASKED"
    )) %>%
    mutate(
      MASKED_NAME = ifelse(broader_brand == "MASKED", 
                           "Traditional Brand 1",
                           MASKED_NAME)
    )
  
#Order Prefix-------------------------------------------------------------------
  
  brandMap <- brandMap %>%
    mutate(
      brand_prefix = case_when(
        brand_id == 4 ~ 1,
        brand_id == 5 ~ 2,
        brand_id == 8 ~ 3,
        brand_id == 11 ~ 4,
        brand_id == 15 ~ 5,
        brand_id == 16 ~ 6,
        brand_id == 1 ~ 7,
        brand_id == 9 ~ 8,
        brand_id == 10 ~ 9,
        brand_id == 12 ~ 10,
        brand_id == 13 ~ 11,
        brand_id == 14 ~ 12,
        brand_id == 17 ~ 13,
        brand_id == 18 ~ 14,
        brand_id == 19 ~ 15,
        brand_id == 22 ~ 16,
        brand_id == 23 ~ 17,
        brand_id == 25 ~ 18,
        brand_id == 26 ~ 19,
        brand_id == 30 ~ 20,
        brand_id == 31 ~ 21,
        brand_id == 32 ~ 22,
        brand_id == 33 ~ 23,
        brand_id == 39 ~ 24,
        brand_id == 40 ~ 25,
        brand_id == 41 ~ 26,
        brand_id == 42 ~ 27,
        brand_id == 24 ~ 28,
        brand_id == 27 ~ 29,
        brand_id == 28 ~ 30,
        brand_id == 29 ~ 31,
        brand_id == 34 ~ 32,
        brand_id == 35 ~ 33,
        brand_id == 36 ~ 34,
        brand_id == 37 ~ 35,
        brand_id == 38 ~ 36
      )
    ) %>%
    mutate(
      broader_prefix = case_when(
        brand_prefix <= 2 ~ 1,
        brand_prefix == 3 ~ 2,
        brand_prefix == 4 ~ 3,
        brand_prefix == 5 ~ 4,
        brand_prefix == 6 ~ 5,
        brand_prefix == 7 ~ 6,
        brand_prefix == 8 ~ 7,
        brand_prefix == 9 ~ 8,
        brand_prefix == 10 ~ 9,
        brand_prefix == 11 ~ 10,
        brand_prefix == 12 ~ 11,
        brand_prefix == 13 ~ 12,
        brand_prefix == 14 ~ 13,
        brand_prefix == 15 ~ 14,
        brand_prefix == 16 ~ 15,
        brand_prefix == 17 ~ 16,
        brand_prefix == 18 | brand_prefix == 19 ~ 17,
        brand_prefix == 20 ~ 18,
        brand_prefix >= 21 & brand_prefix <= 23 ~ 19,
        brand_prefix == 24 ~ 20,
        brand_prefix >= 25 & brand_prefix <= 27 ~ 21,
        brand_prefix == 28 ~ 22, 
        brand_prefix == 29 ~ 23,
        brand_prefix == 30 ~ 24,
        brand_prefix == 31 ~ 25,
        brand_prefix == 32 ~ 26,
        brand_prefix == 33 ~ 27,
        brand_prefix == 34 ~ 28,
        brand_prefix == 35 ~ 29,
        brand_prefix == 36 ~ 30
      )
    )
  
  dbWriteTable(results, "brand_map_and_mask", brandMap, overwrite = T)
  
}


  
