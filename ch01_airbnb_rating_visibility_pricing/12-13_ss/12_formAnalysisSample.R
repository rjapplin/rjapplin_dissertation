#Get Sample---------------------------------------------------------------------
ia_sample <- dbGetQuery(iaan,
           "SELECT KEY_LIST_ID, KEY_HOST_ID, DATE, year, month, first_review_date,
           price,
           CITY_ID, available_030, bathrooms, bedrooms, capacity,
           listing_type, room_type, AC7, AC51, AC72,
           overall_rating, number_of_reviews,
           amenity_word_count, amenity_char_count, descrip_word_count,
           descrip_char_count, loc_overview_char_count, loc_overview_word_count,
           host_response_time, host_response_rate, host_accept_rate,
           host_has_pic, host_verified, host_about_word_count,
           host_about_char_count, host_listing_count, host_is_superhost,
           treatment_group
           FROM [DATA.FULL_SAMPLE]
           ")

#Form Real Price----------------------------------------------------------------
ia_sample <- ia_sample %>%
  left_join(cpi, by = c("year", "month")) %>%
  mutate(real_price = (price/cpi_b2015)*100)

#Simplify Listing Type----------------------------------------------------------
ia_sample <- ia_sample %>%
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
  ) 

#Create Lags--------------------------------------------------------------------
ia_sample <- ia_sample %>%
  arrange(KEY_LIST_ID, KEY_HOST_ID, DATE) %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  mutate(
    price1 = lag(price, n = 1),
    price2 = lag(price, n = 2),
    price3 = lag(price, n = 3),
    real_price1 = lag(real_price, n = 1),
    real_price2 = lag(real_price, n = 2),
    real_price3 = lag(real_price, n = 3)
  ) 

#Create After Treatment Indicator-----------------------------------------------
ia_sample <- ia_sample %>% 
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  mutate(first_review_date = min(as.Date(first_review_date), na.rm = T)) %>%
  mutate(date = as.Date(paste0(year, "-", month, "-", substr(DATE, 7, 8)))) %>%
  mutate(after = ifelse(date >= first_review_date, 1, 0)) %>%
  mutate(flag = ifelse(after == 0 & !(is.na(overall_rating)), 1, 0)) %>%
  mutate(ever_flag = max(flag)) %>%
  filter(ever_flag == 0) %>%
  dplyr::select(-flag, -ever_flag)

#Add in Event Time--------------------------------------------------------------
et_df <- dbReadTable(iaan, "DATA.EVENT_TIME")

ia_sample <- ia_sample %>%
  inner_join(
    et_df,
    by = c("KEY_LIST_ID", "KEY_HOST_ID", "DATE")
  )

#Remove Unncessary Variables----------------------------------------------------
ia_sample <- ia_sample %>%
  dplyr::select(-first_review_date, -cpi, -cpi_b2015, -date, -lobs)

#Create Treatment Variables and Covid Indicator---------------------------------
ia_sample <- ia_sample %>%
  mutate(treated = ifelse(treatment_group == "Treated" | 
                            treatment_group == "Always Treated", 1, 0)) %>%
  mutate(treated_after = treated*after) %>%
  mutate(covid = ifelse(DATE >= 20200130 & DATE <= 20230503, 1, 0))

#Set rating equal to first rating if before treatment---------------------------

ia_sample <- ia_sample %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID, after) %>%
  arrange(KEY_LIST_ID, KEY_HOST_ID, DATE) %>%
  mutate(first_rating = first(overall_rating)) %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  mutate(first_rating = max(first_rating, na.rm = T))

ia_sample <- ia_sample %>%
  mutate(first_rating = ifelse(is.infinite(first_rating), NA, first_rating))

ia_sample <- ia_sample %>%
  mutate(r = ifelse(event_time <= 0, first_rating, overall_rating))

ia_sample <- ia_sample %>%
  mutate(r = r - 4)
  
#Save Analysis Sample-----------------------------------------------------------
dbWriteTable(iaan,
             "DATA.ANALYSIS_SAMPLE",
             ia_sample,
             overwrite = T)

#Cleanup------------------------------------------------------------------------
rm(et_df)
rm(ia_sample)
gc()
