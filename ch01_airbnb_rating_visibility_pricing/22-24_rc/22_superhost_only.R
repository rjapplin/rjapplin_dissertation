#Pricing Persistence------------------------------------------------------------
ia_prices <- dbGetQuery(iaan,
                        "SELECT KEY_LIST_ID, KEY_HOST_ID,
                        DATE, year, month, price
                        FROM [DATA.FULL_SAMPLE]
                        WHERE host_is_superhost = 1") %>%
  left_join(cpi, by = c("year", "month")) %>%
  mutate(price = (price/cpi_b2015)*100) %>%
  select(-cpi, -cpi_b2015, -year, -month)

ia_prices <- ia_prices %>%
  arrange(KEY_LIST_ID, KEY_HOST_ID, DATE) %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  mutate(
    price1 = lag(price, n = 1),
    price2 = lag(price, n = 2),
    price3 = lag(price, n = 3),
    price4 = lag(price, n = 4),
    price5 = lag(price, n = 5),
    price6 = lag(price, n = 6),
    price7 = lag(price, n = 7),
    price8 = lag(price, n = 8),
    price9 = lag(price, n = 9),
    price10 = lag(price, n = 10),
    price11 = lag(price, n = 11),
    price12 = lag(price, n = 12),
    price13 = lag(price, n = 13),
    price14 = lag(price, n = 14),
    price15 = lag(price, n = 15),
    price16 = lag(price, n = 16),
    price17 = lag(price, n = 17),
    price18 = lag(price, n = 18),
    price19 = lag(price, n = 19),
    price20 = lag(price, n = 20)
  ) %>%
  ungroup() %>%
  select(starts_with("price"))

AIC_nom <- vector()
BIC_nom <- vector()
for(l in 1:20){
  
  rform <- paste0("price ~ ", paste0("price", 1:l, collapse = " + "))
  
  reg <- ia_prices %>% 
    filter(!(is.na(price20))) %>%
    lm(rform, .)
  AIC_nom[l] <- AIC(reg)
  BIC_nom[l] <- BIC(reg)
  rm(reg)
  
}

data.frame(plag = 1:20, aic = AIC_nom, bic = BIC_nom) %>%
  pivot_longer(cols = c(aic, bic)) %>%
  dbWriteTable(iaan,
               "FIGURE.REAL_PRICE_LAG_IC_SHONLY",
               .,
               overwrite = T)

rform <- paste0("price ~ ", paste0("price", 1:20, collapse = " + "))
reg <- lm(rform, ia_prices)
regsum <- summary(reg)
regsum$coefficients %>%
  as.data.frame() %>%
  mutate(AdjRSq = regsum$adj.r.squared,
         fstat = regsum$fstatistic[1]) %>%
  dbWriteTable(iaan,
               "MODEL.REAL_PRICE_LAG_20_SHONLY",
               .,
               overwrite = T)

rform <- paste0("price ~ ", paste0("price", 1:3, collapse = " + "))
reg <- lm(rform, ia_prices)
regsum <- summary(reg)
regsum$coefficients %>%
  as.data.frame() %>%
  mutate(AdjRSq = regsum$adj.r.squared,
         fstat = regsum$fstatistic[1]) %>%
  dbWriteTable(iaan,
               "MODEL.REAL_PRICE_LAG_3_SHONLY",
               .,
               overwrite = T)

#Real Price FEIV With all IVs---------------------------------------------------
m <- felm(
  real_price ~ real_price1 + real_price2 + real_price3 + 
    bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID |
    (treated_after | I(30 - available_030) ~  + as.factor(host_response_time)
     + host_response_rate + 
       host_accept_rate +
       host_has_pic + host_verified + host_about_word_count +
       amenity_word_count + descrip_word_count + loc_overview_word_count),
  data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
    select(real_price, real_price1, real_price2, real_price3,
           bathrooms, bedrooms, capacity,
           covid, number_of_reviews, host_is_superhost, listing_type,
           room_type, AC7, AC51, AC72, year, month, KEY_LIST_ID,
           treated_after, available_030, host_response_time, host_response_rate,
           host_accept_rate, host_has_pic, host_verified, host_about_word_count,
           amenity_word_count, descrip_word_count, loc_overview_word_count) %>%
    na.omit() %>%
    filter(host_is_superhost == 1)
)
rm(m1)
gc()

#Real Price FEIV With Count IVs Only--------------------------------------------
m2 <- felm(
  real_price ~ real_price1 + real_price2 + real_price3 + 
    bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID |
    (treated_after | I(30 - available_030) ~ host_about_word_count +
       amenity_word_count + descrip_word_count + loc_overview_word_count),
  data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
    select(real_price, real_price1, real_price2, real_price3,
           bathrooms, bedrooms, capacity,
           covid, number_of_reviews, host_is_superhost, listing_type,
           room_type, AC7, AC51, AC72, year, month, KEY_LIST_ID,
           treated_after, available_030, host_response_time, host_response_rate,
           host_accept_rate, host_has_pic, host_verified, host_about_word_count,
           amenity_word_count, descrip_word_count, loc_overview_word_count) %>%
    na.omit() %>%
    filter(host_is_superhost == 1)
)
rm(m2)
gc()

#Real Price IV All IVs----------------------------------------------------------
m3 <- felm(
  real_price ~ real_price1 + real_price2 + real_price3 + 
    bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | CITY_ID |
    (treated_after | I(30 - available_030) ~  + as.factor(host_response_time)
     + host_response_rate + 
       host_accept_rate +
       host_has_pic + host_verified + host_about_word_count +
       amenity_word_count + descrip_word_count + loc_overview_word_count),
  data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
    select(real_price, real_price1, real_price2, real_price3,
           bathrooms, bedrooms, capacity, CITY_ID,
           covid, number_of_reviews, host_is_superhost, listing_type,
           room_type, AC7, AC51, AC72, year, month, KEY_LIST_ID,
           treated_after, available_030, host_response_time, host_response_rate,
           host_accept_rate, host_has_pic, host_verified, host_about_word_count,
           amenity_word_count, descrip_word_count, loc_overview_word_count) %>%
    na.omit() %>%
    filter(host_is_superhost == 1)
)

#Real Price IV With Only Count IVs----------------------------------------------
m4 <- felm(
  real_price ~ real_price1 + real_price2 + real_price3 + 
    bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | CITY_ID |
    (treated_after | I(30 - available_030) ~ host_has_pic + host_verified + host_about_word_count +
       amenity_word_count + descrip_word_count + loc_overview_word_count),
  data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
    select(real_price, real_price1, real_price2, real_price3,
           bathrooms, bedrooms, capacity, CITY_ID,
           covid, number_of_reviews, host_is_superhost, listing_type,
           room_type, AC7, AC51, AC72, year, month, KEY_LIST_ID,
           treated_after, available_030, host_response_time, host_response_rate,
           host_accept_rate, host_has_pic, host_verified, host_about_word_count,
           amenity_word_count, descrip_word_count, loc_overview_word_count) %>%
    na.omit() %>%
    filter(host_is_superhost == 1)
)

#Data Wrangling for Event Studies - Real Event Time-----------------------------
et_sample <- dbGetQuery(iaan, "SELECT * FROM [DATA.ANALYSIS_SAMPLE] 
                                WHERE ((event_time2 >= -151 AND
                                event_time2 <= 121) OR
                                (treatment_group = 'Never Treated')) AND
                                host_is_superhost = 1")
et_sample <- et_sample %>%
  mutate(event_time = round(event_time2/30)) %>%
  ungroup() %>%
  mutate(event_time = 
           case_when(
             treatment_group == 'Treated' ~ event_time,
             treatment_group == 'Never Treated' ~ 0,
             treatment_group == 'Always Treated' ~ 999
           )
  )

et_sample <- et_sample %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  mutate( n = n()) %>%
  mutate(balaced =
           (treatment_group == 'Always Treated' | treatment_group == 'Never Treated') |
           (treatment_group == 'Treated' & n == 10)
  )

et_sample <- et_sample %>%
  mutate(event_time = as.factor(event_time)) %>%
  mutate(event_time = relevel(event_time, ref = "-1"))

#Real Price Real ES OLS, All Bal----------------------------------------------------
m <- lm(real_price ~ + as.factor(event_time) +
          I(30 - available_030) +
          real_price1 + real_price2 + real_price3 + bathrooms + bedrooms + 
          capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
          as.factor(CITY_ID), data = et_sample %>% filter(balaced == T))
sm <- summary(m)
es_df <-
  rbind(
    sm$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("event_time", Feature, ignore.case = T)) %>%
      mutate(Feature = gsub("as.factor(event_time)", "et_", Feature,
                            fixed = T)) %>%
      #filter(Feature != "et_999" & Feature != "et_-999") %>%
      mutate(event_time = extract_number(Feature)) %>%
      arrange(event_time) %>%
      select(-Feature) %>%
      mutate(id = "rr_ols_all") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(balaced == T))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Real Price Real ES OLS, Treated + Never Treated Bal--------------------------------
m <- lm(real_price ~ + as.factor(event_time) +
          I(30 - available_030) +
          real_price1 + real_price2 + real_price3 + 
          bathrooms + bedrooms + capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
          as.factor(CITY_ID), data = et_sample %>% filter(balaced == T &
                                                            event_time != "999")
)
sm <- summary(m)
es_df <- es_df %>%
  rbind(
    sm$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("event_time", Feature, ignore.case = T)) %>%
      mutate(Feature = gsub("as.factor(event_time)", "et_", Feature,
                            fixed = T)) %>%
      #filter(Feature != "et_999" & Feature != "et_-999") %>%
      mutate(event_time = extract_number(Feature)) %>%
      arrange(event_time) %>%
      select(-Feature) %>%
      mutate(id = "rr_ols_tnt") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(balaced == T &
                                       event_time != "999"))$KEY_LIST_ID)))
  )

rm(m, sm)
gc()

#Real Price Real ES OLS, Treated Bal------------------------------------------------
m <- lm(real_price ~ + as.factor(event_time) +
          I(30 - available_030) +
          real_price1 + real_price2 + real_price3 + 
          bathrooms + bedrooms + capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
          as.factor(CITY_ID), data = et_sample %>% 
          filter(balaced == T & event_time != "999" & 
                   treatment_group != 'Never Treated')
)
sm <- summary(m)
es_df <- es_df %>%
  rbind(
    sm$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("event_time", Feature, ignore.case = T)) %>%
      mutate(Feature = gsub("as.factor(event_time)", "et_", Feature,
                            fixed = T)) %>%
      #filter(Feature != "et_999" & Feature != "et_-999") %>%
      mutate(event_time = extract_number(Feature)) %>%
      arrange(event_time) %>%
      select(-Feature) %>%
      mutate(id = "rr_ols_t") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(balaced == T &
                                       event_time != "999" &
                                       treatment_group != 'Never Treated'))$KEY_LIST_ID)))
  )


rm(m, sm)
gc()



#Real Price Real ES FE, All Bal-----------------------------------------------------
m <- felm(real_price ~ + as.factor(event_time) +
            I(30 - available_030) +
            real_price1 + real_price2 + real_price3 + 
            bathrooms + bedrooms + capacity + covid +
            number_of_reviews +
            as.factor(listing_type) + as.factor(room_type) + 
            AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) |
            KEY_LIST_ID, data = et_sample %>% filter(balaced == T))
sm <- summary(m)
es_df <- es_df %>%
  rbind(
    sm$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("event_time", Feature, ignore.case = T)) %>%
      mutate(Feature = gsub("as.factor(event_time)", "et_", Feature,
                            fixed = T)) %>%
      filter(Feature != "et_999" & Feature != "et_-999") %>%
      mutate(event_time = extract_number(Feature)) %>%
      arrange(event_time) %>%
      select(-Feature) %>%
      mutate(id = "rr_fe_all") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(balaced == T))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Real Price Real ES FE, Treated + Never Treated Bal---------------------------
m <- felm(real_price ~ + as.factor(event_time) +
            I(30 - available_030) +
            real_price1 + real_price2 + real_price3 + 
            bathrooms + bedrooms + capacity + covid +
            number_of_reviews +
            as.factor(listing_type) + as.factor(room_type) + 
            AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) |
            KEY_LIST_ID, data = et_sample %>% filter(balaced == T &
                                                       event_time != "999"))
sm <- summary(m)
es_df <- es_df %>%
  rbind(
    sm$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("event_time", Feature, ignore.case = T)) %>%
      mutate(Feature = gsub("as.factor(event_time)", "et_", Feature,
                            fixed = T)) %>%
      filter(Feature != "et_999" & Feature != "et_-999") %>%
      mutate(event_time = extract_number(Feature)) %>%
      arrange(event_time) %>%
      select(-Feature) %>%
      mutate(id = "rr_fe_tnt") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(balaced == T &
                                       event_time != "999"))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Real Price Real ES FE, Treated Bal-------------------------------------------
m <- felm(real_price ~ + as.factor(event_time) +
            I(30 - available_030) +
            real_price1 + real_price2 + real_price3 + 
            bathrooms + bedrooms + capacity + covid +
            number_of_reviews +
            as.factor(listing_type) + as.factor(room_type) + 
            AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) |
            KEY_LIST_ID, data = et_sample %>% 
            filter(balaced == T & event_time != "999" & 
                     treatment_group != "Never Treated"))
sm <- summary(m)
es_df <- es_df %>%
  rbind(
    sm$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("event_time", Feature, ignore.case = T)) %>%
      mutate(Feature = gsub("as.factor(event_time)", "et_", Feature,
                            fixed = T)) %>%
      filter(Feature != "et_999" & Feature != "et_-999") %>%
      mutate(event_time = extract_number(Feature)) %>%
      arrange(event_time) %>%
      select(-Feature) %>%
      mutate(id = "rr_fe_t") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(balaced == T &
                                       event_time != "999" &
                                       treatment_group != 'Never Treated'))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()





#Real Price Real ES OLS, All Unbal----------------------------------------------
m <- lm(real_price ~ + as.factor(event_time) +
          I(30 - available_030) +
          real_price1 + real_price2 + real_price3 + bathrooms + bedrooms + 
          capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
          as.factor(CITY_ID), data = et_sample)
sm <- summary(m)
es_df <- es_df %>%
  rbind(
    sm$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("event_time", Feature, ignore.case = T)) %>%
      mutate(Feature = gsub("as.factor(event_time)", "et_", Feature,
                            fixed = T)) %>%
      #filter(Feature != "et_999" & Feature != "et_-999") %>%
      mutate(event_time = extract_number(Feature)) %>%
      arrange(event_time) %>%
      select(-Feature) %>%
      mutate(id = "rr_ols_all_unbal") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample)$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Real Price Real ES OLS, Treated + Never Treated UnBal--------------------------------
m <- lm(real_price ~ + as.factor(event_time) +
          I(30 - available_030) +
          real_price1 + real_price2 + real_price3 + 
          bathrooms + bedrooms + capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
          as.factor(CITY_ID), data = et_sample %>% filter(event_time != "999")
)
sm <- summary(m)
es_df <- es_df %>%
  rbind(
    sm$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("event_time", Feature, ignore.case = T)) %>%
      mutate(Feature = gsub("as.factor(event_time)", "et_", Feature,
                            fixed = T)) %>%
      #filter(Feature != "et_999" & Feature != "et_-999") %>%
      mutate(event_time = extract_number(Feature)) %>%
      arrange(event_time) %>%
      select(-Feature) %>%
      mutate(id = "rr_ols_tnt_unbal") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999"))$KEY_LIST_ID)))
  )

rm(m, sm)
gc()

#Real Price Real ES OLS, Treated unBal------------------------------------------------
m <- lm(real_price ~ + as.factor(event_time) +
          I(30 - available_030) +
          real_price1 + real_price2 + real_price3 + 
          bathrooms + bedrooms + capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
          as.factor(CITY_ID), data = et_sample %>% 
          filter(event_time != "999" & 
                   treatment_group != 'Never Treated')
)
sm <- summary(m)
es_df <- es_df %>%
  rbind(
    sm$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("event_time", Feature, ignore.case = T)) %>%
      mutate(Feature = gsub("as.factor(event_time)", "et_", Feature,
                            fixed = T)) %>%
      #filter(Feature != "et_999" & Feature != "et_-999") %>%
      mutate(event_time = extract_number(Feature)) %>%
      arrange(event_time) %>%
      select(-Feature) %>%
      mutate(id = "rr_ols_t_unbal") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999" &
                                       treatment_group != 'Never Treated'))$KEY_LIST_ID)))
  )


rm(m, sm)
gc()



#Real Price Real ES FE, All UnBal-----------------------------------------------------
m <- felm(real_price ~ + as.factor(event_time) +
            I(30 - available_030) +
            real_price1 + real_price2 + real_price3 + 
            bathrooms + bedrooms + capacity + covid +
            number_of_reviews +
            as.factor(listing_type) + as.factor(room_type) + 
            AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) |
            KEY_LIST_ID, data = et_sample)
sm <- summary(m)
es_df <- es_df %>%
  rbind(
    sm$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("event_time", Feature, ignore.case = T)) %>%
      mutate(Feature = gsub("as.factor(event_time)", "et_", Feature,
                            fixed = T)) %>%
      filter(Feature != "et_999" & Feature != "et_-999") %>%
      mutate(event_time = extract_number(Feature)) %>%
      arrange(event_time) %>%
      select(-Feature) %>%
      mutate(id = "rr_fe_all_unbal") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample)$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Real Price Real ES FE, Treated + Never Treated unBal---------------------------
m <- felm(real_price ~ + as.factor(event_time) +
            I(30 - available_030) +
            real_price1 + real_price2 + real_price3 + 
            bathrooms + bedrooms + capacity + covid +
            number_of_reviews +
            as.factor(listing_type) + as.factor(room_type) + 
            AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) |
            KEY_LIST_ID, data = et_sample %>% filter(event_time != "999"))
sm <- summary(m)
es_df <- es_df %>%
  rbind(
    sm$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("event_time", Feature, ignore.case = T)) %>%
      mutate(Feature = gsub("as.factor(event_time)", "et_", Feature,
                            fixed = T)) %>%
      filter(Feature != "et_999" & Feature != "et_-999") %>%
      mutate(event_time = extract_number(Feature)) %>%
      arrange(event_time) %>%
      select(-Feature) %>%
      mutate(id = "rr_fe_tnt_unbal") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999"))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Real Price Real ES FE, Treated UnBal-------------------------------------------
m <- felm(real_price ~ + as.factor(event_time) +
            I(30 - available_030) +
            real_price1 + real_price2 + real_price3 + 
            bathrooms + bedrooms + capacity + covid +
            number_of_reviews +
            as.factor(listing_type) + as.factor(room_type) + 
            AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) |
            KEY_LIST_ID, data = et_sample %>% 
            filter(event_time != "999" & 
                     treatment_group != "Never Treated"))
sm <- summary(m)
es_df <- es_df %>%
  rbind(
    sm$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("event_time", Feature, ignore.case = T)) %>%
      mutate(Feature = gsub("as.factor(event_time)", "et_", Feature,
                            fixed = T)) %>%
      filter(Feature != "et_999" & Feature != "et_-999") %>%
      mutate(event_time = extract_number(Feature)) %>%
      arrange(event_time) %>%
      select(-Feature) %>%
      mutate(id = "rr_fe_t_unbal") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999" &
                                       treatment_group != 'Never Treated'))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Save sh es to database---------------------------------------------------------
dbWriteTable(iaan, "MODEL.SH_ONLY_EVENTSTUDIES", es_df, overwrite = T)
rm(es_df, et_sample)
gc()

#Data Wrangling For CS Event Studies - Real Event Time--------------------------
if(!("DATA.CS_SH_ONLY_SAMPLE" %in% dbListTables(iaan))){
  
  cs_sample <- dbGetQuery(iaan, "SELECT * FROM [DATA.ANALYSIS_SAMPLE] 
                                  WHERE ((event_time2 >= -151 AND
                                  event_time2 <= 121) OR
                                  (treatment_group = 'Never Treated'))
                                  AND host_is_superhost = 1")
  cs_sample <- cs_sample %>%
    mutate(event_time2 = round(event_time2/30)) %>%
    ungroup() %>%
    mutate(event_time = 
             case_when(
               treatment_group == 'Treated' ~ event_time,
               treatment_group == 'Never Treated' ~ 0,
               treatment_group == 'Always Treated' ~ 999
             )
    ) %>%
    filter(treatment_group != "Always Treated")
  
  cs_sample <- cs_sample %>%
    dplyr::group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
    dplyr::mutate(id = cur_group_id()) %>%
    dplyr::group_by(id) %>%
    select(-KEY_LIST_ID, -KEY_HOST_ID)
  
  cs_sample <- cs_sample %>%
    arrange(year, month) %>%
    group_by(year, month) %>%
    mutate(period = cur_group_id()) %>%
    group_by(id) %>%
    dplyr::mutate(G = ifelse(event_time2 == 0, period, NA)) %>%
    dplyr::mutate(G = min(G, na.rm = T)) %>%
    dplyr::mutate(G = ifelse(treatment_group == "Never Treated", 0, G)) %>%
    dplyr::mutate(G = ifelse(is.infinite(G), 0, G))
  
  cs_sample <- cs_sample %>%
    select(id, period, G, treatment_group, treated_after, price, real_price,
           price1, price2, price3, real_price1, real_price2, real_price3,
           available_030, bathrooms, bedrooms, bedrooms, capacity, covid,
           listing_type, room_type, number_of_reviews, AC7, AC51, AC72, year,
           month, CITY_ID)
  
  cs_sample <- cs_sample %>%
    mutate(d = 30 - available_030)
  
  cs_sample %>%
    dbWriteTable(iaan, "DATA.CS_SH_ONLY_SAMPLE", .)
  
} else {
  
  cs_sample <- dbReadTable(iaan, "DATA.CS_SAMPLE")
  
}

cs_sample <- na.omit(cs_sample)

for(i in 1:200){
  
  if(i <= 100){
    cg <- "notyettreated"
  } else {
    cg <- "nevertreated"
  }
  
  seed <- sample(1:999999, 1)
  set.seed(seed)
  Gl <- sample(unique(cs_sample$G), 1, T)
  cs_subsample <- cs_sample %>% filter(G == 0 | G %in% Gl:(Gl+9))
  nid <- length(unique(cs_subsample$id))
  fid <- (0.2*nid) %>% round()
  sid <- sample(unique(cs_subsample$id), fid, T)
  cs_subsample1 <- cs_subsample %>% filter(id %in% sid)
  cs_subsample1 <- cs_subsample1 %>%
    select(id, period, G, real_price, real_price1, bedrooms,
           bathrooms, capacity, AC7, AC51, AC72, month,
           treatment_group) %>% 
    na.omit() %>%
    group_by(id) %>%
    mutate(n = n_distinct(period)) %>%
    filter(n >= 6)
  
  
  attgt <- try(
    att_gt(yname = "real_price",
           tname = "period",
           idname = "id",
           gname = "G",
           xformla = ~0 + real_price1,
           data = cs_subsample1,
           allow_unbalanced_panel = T,
           control_group = cg,
           biters = 100,
           pl = T,
           cores = 4,
           anticipation = 1)
  )
  
  if("try-error" %in% class(attgt)){
    print(paste0("Error on iteration ", i))
    if(exists("res")){
      res <- res %>%
        rbind(
          data.frame(
            et = NA,
            est = NA,
            est_se = NA,
            overall = NA,
            overall_se = NA,
            iteration = i,
            seed = seed,
            control = cg
          )
        )
    } else {
      res <-data.frame(
        et = NA,
        est = NA,
        est_se = NA,
        overall = NA,
        overall_se = NA,
        iteration = i,
        seed = seed,
        control = cg
      )
    }
  } else {
    
    attgt <- aggte(attgt, type = "dynamic", na.rm = T)
    if(exists("res")){
      res <- res %>% rbind(
        data.frame(
          et = attgt$egt,
          est = attgt$att.egt,
          est_se = attgt$se.egt,
          overall = attgt$overall.att,
          overall_se = attgt$overall.se,
          iteration = i,
          seed = seed,
          control = cg
        )
      )
    } else {
      res <-data.frame(
        et = attgt$egt,
        est = attgt$att.egt,
        est_se = attgt$se.egt,
        overall = attgt$overall.att,
        overall_se = attgt$overall.se,
        iteration = i,
        seed = seed,
        control = cg
      )
    }
    
  }
  
  gc()
  print(i)
  
  
}

dbWriteTable(iaan, "MODEL.CS_SH_ONLY_EVENT_STUDIES", res, overwrite = T)
rm(attgt, cs_sample, cs_subsample, cs_subsample1, cg, fid, Gl,
   i, nid, seed, sid, res)
