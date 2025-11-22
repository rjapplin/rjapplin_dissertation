#Correlation Matrix: Numeric, Non Amenity---------------------------------------
if(!("FIGURE.NON_AMEN_COR_MATRIX" %in% dbListTables(iaan))){
  numerics <- dbGetQuery(iaan,
                         "SELECT year, month, price, capacity, bathrooms, bedrooms,
                         min_night, max_night, available, available_030,
                         available_060, available_090, available_365,
                         number_of_reviews, overall_rating, accom_rating,
                         clean_rating, checkin_rating, com_rating, loc_rating,
                         value_rating,
                         amenity_word_count, amenity_count, amenity_char_count,
                         loc_overview_word_count, descrip_word_count, 
                         loc_overview_char_count, descrip_char_count,
                         host_response_rate, host_accept_rate, host_is_superhost,
                         host_listing_count, host_has_pic, host_verified,
                         host_about_word_count, host_about_char_count
                         FROM [DATA.FULL_SAMPLE]")
  
  correlation <- numerics %>%
    left_join(cpi, by = c("year", "month")) %>%
    mutate(real_price = (price/cpi_b2015)*100) %>%
    select(-year, -month, -cpi_b2015, -cpi) %>%
    relocate(real_price, .before = capacity) %>%
    cor(use = "complete.obs")
  rm(numerics)
  
  var_names <- rownames(correlation)
  
  correlation2 <- correlation %>%
    as.data.frame() %>%
    rownames_to_column(var = "Var1") %>%
    melt() %>%
    mutate(Var1 = factor(Var1, levels = var_names),
           variable = factor(variable, levels = var_names)) 
  
  dbWriteTable(iaan, 
               "FIGURE.NON_AMEN_COR_MATRIX",
               correlation2)
  
  rm(correlation, correlation2, var_names)
  non_amenity_cor_mat <- getPlotMaster("NON_AMEN_COR_MATRIX")
  
} else {
  
  non_amenity_cor_mat <- getPlotMaster("NON_AMEN_COR_MATRIX")
  
}


#Amenity Correlation------------------------------------------------------------
if(!("FIGURE.AMEN_COR_MATRIX" %in% dbListTables(iaan))){
  amen <- dbReadTable(iaan, "DATA.FULL_SAMPLE") %>%
    select(year, month, price, ends_with("rating"), starts_with("AC"))
  
  (amen %>%
    select(starts_with("AC")) %>%
    sapply(function(x) sd(x)) %>%
    as.data.frame() %>%
    rownames_to_column(var = "Amenity") %>%
    filter(Amenity != "accom_rating") %>%
    dplyr::rename(SD = ".") %>%
    filter(SD == 0))$Amenity -> amenities_to_exclude
  
  correlation <- amen %>%
    select(-contains(amenities_to_exclude)) %>%
    left_join(cpi, by = c("year", "month")) %>%
    mutate(real_price = (price/cpi_b2015)*100) %>%
    select(-year, -month, -cpi_b2015, -cpi) %>%
    relocate(real_price, .before = overall_rating) %>%
    cor(use = "complete.obs")
  
  correlation2 <- correlation %>%
    as.data.frame() %>%
    rownames_to_column(var = "Var1") %>%
    melt() %>%
    left_join(read.csv("amenity_map.csv") %>%
                select(HUMAN_LABEL, AMENITY_CLEAN_CODE) %>%
                dplyr::rename(Amenity = HUMAN_LABEL, Var1 = AMENITY_CLEAN_CODE),
              by = "Var1") %>%
    mutate(
      Var1 = ifelse(is.na(Amenity), Var1, Amenity)
    ) %>%
    select(-Amenity) %>%
    left_join(read.csv("amenity_map.csv") %>%
                select(HUMAN_LABEL, AMENITY_CLEAN_CODE) %>%
                dplyr::rename(Amenity = HUMAN_LABEL, variable = AMENITY_CLEAN_CODE),
              by = "variable") %>%
    mutate(
      variable = ifelse(is.na(Amenity), variable, Amenity)
    ) %>%
    select(-Amenity)
  
  var_names <- unique(correlation2$Var1)
    
  correlation2 <- correlation2 %>%
    mutate(Var1 = factor(Var1, levels = var_names),
           variable = factor(variable, levels = var_names)) 
  
  dbWriteTable(iaan,
               "FIGURE.AMEN_COR_MATRIX",
               correlation2)
  
  rm(amen, correlation, correlation2, amenities_to_exclude)

  amenity_cor_mat <- getPlotMaster(plot = "AMEN_COR_MATRIX")

  
} else {
  
  amenity_cor_mat <- getPlotMaster(plot = "AMEN_COR_MATRIX")
  
}


