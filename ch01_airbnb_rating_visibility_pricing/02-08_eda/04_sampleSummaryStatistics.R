#Numeric Variable Summary Stats-------------------------------------------------
if(!("TABLE.SUM_STATS" %in% dbListTables(iaan))){
  sumStats <- describeBy(dbReadTable(iaan, "DATA.FULL_SAMPLE") %>%
                           left_join(cpi, by = c("year", "month")) %>%
                           mutate(real_price = (price/cpi_b2015)*100))
  sumStatsTable <- sumStats %>%
    as.data.frame() %>%
    rownames_to_column(var = "Variable") %>%
    mutate(vars = ifelse(Variable == "price", -1, vars)) %>%
    mutate(vars = ifelse(Variable == "real_price", 0, vars)) %>%
    arrange(vars) %>%
    filter(Variable %in% c("price", "real_price", "capacity", "bathrooms", "bedrooms",
                           "min_night", "max_night", "available",
                           "available_030", "available_060", "available_090",
                           "available_365",
                           "number_of_reviews",
                           "overall_rating", "accom_rating", "clean_rating",
                           "checkin_rating", "com_rating", "loc_rating",
                           "value_rating",
                           "amenity_word_count", "amenity_count",
                           "amenity_char_count", "loc_overview_word_count",
                           "descrip_word_count", "loc_overview_char_count",
                           "descrip_char_count", "host_response_rate",
                           "host_accept_rate", "host_is_superhost",
                           "host_listing_count", "host_has_pic", "host_verified",
                           "host_about_word_count", "host_about_char_count") |
             grepl("AC", Variable)) %>%
    select(Variable, n, min, mean, median, max, sd, se) %>%
    mutate(
      min = round(min, 3),
      mean = round(mean, 3),
      median = round(median, 3),
      max = round(max, 3),
      sd = round(sd, 3),
      se = round(se, 3)
    ) %>%
    left_join(
      read.csv("amenity_map.csv") %>%
        select(HUMAN_LABEL, AMENITY_CLEAN_CODE) %>%
        dplyr::rename(Variable = AMENITY_CLEAN_CODE),
      by = "Variable") %>%
    mutate(
      Variable = ifelse(is.na(HUMAN_LABEL), Variable, HUMAN_LABEL)
    ) %>%
    select(-HUMAN_LABEL) %>%
    dplyr::rename(
      N = n,
      Min = min,
      Mean = mean,
      Median = median,
      Max = max,
      SD = sd,
      SE = se
    ) %>%
    select(-SE)
  
  dbWriteTable(iaan,
               "TABLE.SUM_STATS",
               sumStatsTable,
               overwrite = T)
  rm(sumStats, sumStatsTable)
  
} else {
  
  
}

#Factor Variable Summary Stats--------------------------------------------------
if(
  !("TABLE.SUM_STATS_LIST_TYPE" %in% dbListTables(iaan)) |
  !("TABLE.SUM_STATS_ROOM_TYPE" %in% dbListTables(iaan))
) {
  
  facts <- dbGetQuery(iaan,
             "SELECT listing_type, room_type, zip, price, overall_rating FROM [DATA.FULL_SAMPLE]")
  
  facts %>%
    mutate(listing_type = tolower(listing_type)) %>%
    mutate(
      listing_type = gsub("private room in ", "", listing_type, ignore.case = T)
    ) %>%
    mutate(
      listing_type = gsub("room in ", "", listing_type, ignore.case = T)
    ) %>%
    mutate(
      listing_type = gsub("shared room in ", "", listing_type, ignore.case = T)
    ) %>%
    mutate(
      listing_type = gsub("shared ", "", listing_type, ignore.case = T)
    ) %>%
    mutate(
      listing_type = gsub("entire ", "", listing_type, ignore.case = T)
    ) %>%
    mutate(
      listing_type = case_when(
        grepl("bed", listing_type, ignore.case = T) ~ "bnb",
        grepl("casa", listing_type, ignore.case = T) ~ "casa particular",
        grepl("condo", listing_type, ignore.case = T) ~ "condo",
        grepl("cycladic", listing_type, ignore.case = T) ~ "cycladic house",
        grepl("dome", listing_type, ignore.case = T) ~ "dome house",
        grepl("boat", listing_type, ignore.case = T) ~ "boat",
        grepl("minsu", listing_type, ignore.case = T) ~ "minsu",
        grepl("ryokan", listing_type, ignore.case = T) ~"ryokan",
        grepl("tiny", listing_type, ignore.case = T) ~ "tiny home",
        grepl("earth", listing_type, ignore.case = T) ~ "earth house",
        grepl("pension", listing_type, ignore.case = T) ~ "pension",
        .default = listing_type
      ) 
    ) %>%
    mutate(
      listing_type = gsub("private ", "", listing_type, ignore.case = T)
    ) %>%
    mutate(
      listing_type = ifelse(listing_type == "house", "home", listing_type) 
    ) %>%
    mutate(
      listing_type = ifelse(listing_type == "residential home", "home", listing_type)
    ) %>%
    mutate(
      listing_type = ifelse(listing_type == "", "other", listing_type)
    ) %>%
    mutate(
      listing_type = ifelse(listing_type == "parking space", "other", listing_type)
    ) -> facts
  
} else {
  
}

#Listing Type Table-------------------------------------------------------------
if(!("TABLE.SUM_STATS_LIST_TYPE" %in% dbListTables(iaan))){
  listingTypeTable <- (table(facts$listing_type)/length(facts$listing_type)) %>% 
    as.data.frame() %>% 
    arrange(desc(Freq)) %>% 
    mutate(Freq = round(Freq, 3)) %>%
    dplyr::rename(
      Type = Var1,
      Percent = Freq
    ) %>%
    inner_join(
      (table(facts$listing_type)) %>%
        as.data.frame() %>%
        arrange(desc(Freq)) %>%
        dplyr::rename(Type = Var1) %>%
        dplyr::rename(N = Freq),
      by = "Type"
    ) %>%
    inner_join(
      facts %>%
        group_by(listing_type) %>%
        dplyr::summarize(Price = round(mean(price), 3),
                         `Price SD` = round(sd(price), 3),
                         Rating = round(mean(overall_rating, na.rm = T), 3),
                         `Rating SD` = round(sd(overall_rating, na.rm = T), 3)
                         ) %>%
        dplyr::rename(Type = listing_type),
      by = "Type"
    )
  
  dbWriteTable(iaan,
               "TABLE.SUM_STATS_LIST_TYPE",
               listingTypeTable,
               overwrite = T)
  rm(listingTypeTable)
  
} else {
  
}

#Room Type Table----------------------------------------------------------------
if(!("TABLE.SUM_STATS_ROOM_TYPE" %in% dbListTables(iaan))){
  roomTypeTable <- ((table(facts$room_type))/length(facts$room_type)) %>%
    as.data.frame() %>% 
    arrange(desc(Freq)) %>% 
    mutate(Freq = round(Freq, 3)) %>%
    dplyr::rename(
      Type = Var1,
      Percent = Freq
    ) %>%
    inner_join(
      (table(facts$room_type)) %>%
        as.data.frame() %>%
        arrange(desc(Freq)) %>%
        dplyr::rename(Type = Var1) %>%
        dplyr::rename(N = Freq),
      by = "Type"
    ) %>%
    inner_join(
      facts %>%
        group_by(room_type) %>%
        dplyr::summarize(Price = round(mean(price), 3),
                         `Price SD` = round(sd(price), 3),
                         Rating = round(mean(overall_rating, na.rm = T), 3),
                         `Rating SD` = round(sd(overall_rating, na.rm = T), 3)
        ) %>%
        dplyr::rename(Type = room_type),
      by = "Type"
    )
  
  dbWriteTable(iaan,
               "TABLE.SUM_STATS_ROOM_TYPE",
               roomTypeTable,
               overwrite = T)
  rm(roomTypeTable)
  
} else {
  
}

#Cleanup------------------------------------------------------------------------
rm(facts)






