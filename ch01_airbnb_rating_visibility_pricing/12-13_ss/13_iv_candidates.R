ia_sample <- dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE")

iv_cands <- ia_sample %>% 
  ungroup() %>%
  select(price, real_price, host_response_rate, host_accept_rate,
                     host_response_time,
                     host_has_pic, host_verified, host_is_superhost,
                     amenity_word_count, amenity_char_count, descrip_word_count,
                     descrip_char_count, loc_overview_word_count,
                     loc_overview_char_count, host_about_word_count,
                     host_about_char_count, treatment_group) %>% 
  mutate(host_response_rate = cut_interval(host_response_rate, n = 5),
         host_accept_rate = cut_interval(host_accept_rate, n = 5),
         amenity_char_count = cut(amenity_char_count, breaks = 5),
         amenity_word_count = cut(amenity_word_count, breaks = 5),
         descrip_word_count = cut(descrip_word_count, breaks = 5),
         descrip_char_count = cut(descrip_char_count, breaks = 5),
         loc_overview_word_count = cut(loc_overview_word_count, breaks = 5),
         loc_overview_char_count = cut(loc_overview_char_count, breaks = 5),
         host_about_word_count = cut(host_about_word_count, breaks = 5),
         host_about_char_count = cut(host_about_char_count, breaks = 5)
  )
  
#Profile Picture----------------------------------------------------------------
iv_cands %>%
  group_by(host_has_pic, treatment_group) %>%
  filter(!(is.na(host_has_pic))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_HOST_PIC", ., overwrite = T)

#Host Verified------------------------------------------------------------------
iv_cands %>%
  group_by(host_verified, treatment_group) %>%
  filter(!(is.na(host_verified))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_HOST_VER", ., overwrite = T)

#Host Superhost-----------------------------------------------------------------
iv_cands %>%
  group_by(host_is_superhost, treatment_group) %>%
  filter(!(is.na(host_is_superhost))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_HOST_SUPER", ., overwrite = T)

#Host Response Time-------------------------------------------------------------
iv_cands %>%
  group_by(host_response_time, treatment_group) %>%
  filter(!(is.na(host_response_time))) %>%
  filter(host_response_time != "N/A" & host_response_time != "") %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_RESP_TIME", ., overwrite = T)

#Response Rate------------------------------------------------------------------
iv_cands %>%
  group_by(host_response_rate, treatment_group) %>%
  filter(!(is.na(host_response_rate))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_RESP_RATE", ., overwrite = T)

#Accept Rate--------------------------------------------------------------------
iv_cands %>%
  group_by(host_accept_rate, treatment_group) %>%
  filter(!(is.na(host_accept_rate))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_ACPT_RATE", ., overwrite = T)

#Amenity Word Count-------------------------------------------------------------
iv_cands %>%
  group_by(amenity_word_count, treatment_group) %>%
  filter(!(is.na(amenity_word_count))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_AMEN_WORDS", ., overwrite = T)

#Description Word Count---------------------------------------------------------
iv_cands %>%
  group_by(descrip_word_count, treatment_group) %>%
  filter(!(is.na(descrip_word_count))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_DESC_WORDS", ., overwrite = T)

#Loc Overview Word Count--------------------------------------------------------
iv_cands %>%
  group_by(loc_overview_word_count, treatment_group) %>%
  filter(!(is.na(loc_overview_word_count))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_LOC_WORDS", ., overwrite = T)

#Host Word Count----------------------------------------------------------------
iv_cands %>%
  group_by(host_about_word_count, treatment_group) %>%
  filter(!(is.na(host_about_word_count))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_HOST_WORDS", ., overwrite = T)

#Amenity Char Count-------------------------------------------------------------
iv_cands %>%
  group_by(amenity_char_count, treatment_group) %>%
  filter(!(is.na(amenity_char_count))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_AMEN_CHAR", ., overwrite = T)

#Description Word Count---------------------------------------------------------
iv_cands %>%
  group_by(descrip_char_count, treatment_group) %>%
  filter(!(is.na(descrip_char_count))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_DESC_CHAR", ., overwrite = T)

#Loc Overview Word Count--------------------------------------------------------
iv_cands %>%
  group_by(loc_overview_char_count, treatment_group) %>%
  filter(!(is.na(loc_overview_char_count))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_LOC_CHAR", ., overwrite = T)

#Host Word Count----------------------------------------------------------------
iv_cands %>%
  group_by(host_about_char_count, treatment_group) %>%
  filter(!(is.na(host_about_char_count))) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_BY_HOST_CHAR", ., overwrite = T)

#First Stage Regression Price Treated-------------------------------------------
m <- lm(treated_after ~ price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
  number_of_reviews +
  as.factor(listing_type) + as.factor(room_type) + 
  AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
  as.factor(CITY_ID) + as.factor(host_response_time)
  + host_response_rate + 
    host_accept_rate +
    host_has_pic + host_verified + host_about_word_count +
    amenity_word_count + descrip_word_count + loc_overview_word_count,
  dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE")) 

sm <- summary(m)
sm$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var= "Feature") %>%
  filter(grepl("host_response_time", Feature) | grepl("host", Feature) |
           grepl("count", Feature)) %>%
  filter(!(grepl("hostel", Feature))) %>%
  mutate(Feature = gsub("as.factor", "", Feature)) %>%
  mutate(Feature = gsub("\\(", "", Feature)) %>%
  mutate(Feature = gsub("\\)", "", Feature)) %>%
  mutate(Feature = gsub("host_response_time", "response time: ", Feature)) %>%
  mutate(Feature = gsub("N\\/A", "unknown", Feature)) %>%
  dplyr::rename(p = `Pr(>|t|)`) %>%
  select(Feature, Estimate, p) %>%
  mutate(
    Estimate = round(Estimate, 3),
    p = round(p, 3)
  ) %>%
  dbWriteTable(iaan, "MODEl.PRICE_FIRST_STAGE", ., overwrite = T)
rm(m, sm)
gc()

#First Stage Regression Real Treated-------------------------------------------- 
m <- lm(treated_after ~ real_price1 + real_price2 + real_price3 + 
          bathrooms + bedrooms + capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
          as.factor(CITY_ID) + as.factor(host_response_time)
        + host_response_rate + 
          host_accept_rate +
          host_has_pic + host_verified + host_about_word_count +
          amenity_word_count + descrip_word_count + loc_overview_word_count,
        dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE")) 

sm <- summary(m)
sm$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var= "Feature") %>%
  filter(grepl("host_response_time", Feature) | grepl("host", Feature) |
           grepl("count", Feature)) %>%
  filter(!(grepl("hostel", Feature))) %>%
  mutate(Feature = gsub("as.factor", "", Feature)) %>%
  mutate(Feature = gsub("\\(", "", Feature)) %>%
  mutate(Feature = gsub("\\)", "", Feature)) %>%
  mutate(Feature = gsub("host_response_time", "response time: ", Feature)) %>%
  mutate(Feature = gsub("N\\/A", "unknown", Feature)) %>%
  dplyr::rename(p = `Pr(>|t|)`) %>%
  select(Feature, Estimate, p) %>%
  mutate(
    Estimate = round(Estimate, 3),
    p = round(p, 3)
  ) %>%
  dbWriteTable(iaan, "MODEl.REAL_PRICE_FIRST_STAGE", ., overwrite = T)
rm(m, sm)
gc()
  
#First Stage Regression Price Quantity-----------------------------------------
m <- lm(I(30 - available_030) ~ price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
          as.factor(CITY_ID) + as.factor(host_response_time)
        + host_response_rate + 
          host_accept_rate +
          host_has_pic + host_verified + host_about_word_count +
          amenity_word_count + descrip_word_count + loc_overview_word_count,
        dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE")) 

sm <- summary(m)
sm$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var= "Feature") %>%
  filter(grepl("host_response_time", Feature) | grepl("host", Feature) |
           grepl("count", Feature)) %>%
  filter(!(grepl("hostel", Feature))) %>%
  mutate(Feature = gsub("as.factor", "", Feature)) %>%
  mutate(Feature = gsub("\\(", "", Feature)) %>%
  mutate(Feature = gsub("\\)", "", Feature)) %>%
  mutate(Feature = gsub("host_response_time", "response time: ", Feature)) %>%
  mutate(Feature = gsub("N\\/A", "unknown", Feature)) %>%
  dplyr::rename(p = `Pr(>|t|)`) %>%
  select(Feature, Estimate, p) %>%
  mutate(
    Estimate = round(Estimate, 3),
    p = round(p, 3)
  ) %>%
  dbWriteTable(iaan, "MODEl.PRICE_FIRST_STAGE_QUANTITY", ., overwrite = T)
rm(m, sm)
gc()

#First Stage Regression Real Quantity-------------------------------------------
m <- lm(I(30 - available_030) ~ real_price1 + real_price2 + real_price3 + 
          bathrooms + bedrooms + capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
          as.factor(CITY_ID) + as.factor(host_response_time)
        + host_response_rate + 
          host_accept_rate +
          host_has_pic + host_verified + host_about_word_count +
          amenity_word_count + descrip_word_count + loc_overview_word_count,
        dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE")) 

sm <- summary(m)
sm$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var= "Feature") %>%
  filter(grepl("host_response_time", Feature) | grepl("host", Feature) |
           grepl("count", Feature)) %>%
  filter(!(grepl("hostel", Feature))) %>%
  mutate(Feature = gsub("as.factor", "", Feature)) %>%
  mutate(Feature = gsub("\\(", "", Feature)) %>%
  mutate(Feature = gsub("\\)", "", Feature)) %>%
  mutate(Feature = gsub("host_response_time", "response time: ", Feature)) %>%
  mutate(Feature = gsub("N\\/A", "unknown", Feature)) %>%
  dplyr::rename(p = `Pr(>|t|)`) %>%
  select(Feature, Estimate, p) %>%
  mutate(
    Estimate = round(Estimate, 3),
    p = round(p, 3)
  ) %>%
  dbWriteTable(iaan, "MODEl.REAL_PRICE_FIRST_STAGE_QUANTITY", ., overwrite = T)
rm(m, sm)
gc()

#Cleanup------------------------------------------------------------------------
rm(iv_cands)

