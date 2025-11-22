#Data Wrangling For Event Studies - Nominal Event Time--------------------------
et_sample <- dbGetQuery(iaan, "SELECT * FROM [DATA.ANALYSIS_SAMPLE] 
                                WHERE (event_time >= -5 AND
                                event_time <= 4) OR
                                (treatment_group = 'Never Treated')")
et_sample <- et_sample %>%
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

#Nominal Price Nominal ES OLS, All----------------------------------------------
m <- lm(price ~ + as.factor(event_time) +
          I(30 - available_030) +
          price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
          as.factor(CITY_ID), data = et_sample)
sm <- summary(m)
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
  mutate(id = "nn_ols_all") %>%
  mutate(n = length(m$residuals)) %>%
  mutate(nl = length(
    unique((et_sample)$KEY_LIST_ID))) -> es_df
rm(m, sm)
gc()

#Nominal Price Nominal ES OLS, Treated + Never Treated--------------------------
m <- lm(price ~ + as.factor(event_time) +
          I(30 - available_030) +
          price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
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
      mutate(id = "nn_ols_tnt") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999"))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Nominal Price Nominal ES OLS, Treated------------------------------------------
m <- lm(price ~ + as.factor(event_time) +
          I(30 - available_030) +
          price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
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
      mutate(id = "nn_ols_t") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999" &
                                       treatment_group != 'Never Treated'))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()





#Nominal Price Nominal ES FE, All-----------------------------------------------
m <- felm(price ~ + as.factor(event_time) +
            I(30 - available_030) +
            price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
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
      mutate(id = "nn_fe_all") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample)$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Nominal Price Nominal ES FE, Treated + Never Treated---------------------------
m <- felm(price ~ + as.factor(event_time) +
            I(30 - available_030) +
            price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
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
      mutate(id = "nn_fe_tnt") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999"))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Nominal Price Nominal ES FE, Treated-------------------------------------------
m <- felm(price ~ + as.factor(event_time) +
            I(30 - available_030) +
            price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
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
      mutate(id = "nn_fe_t") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999" &
                                       treatment_group != 'Never Treated'))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Real Price Nominal ES OLS, All-------------------------------------------------
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
      mutate(id = "rn_ols_all") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample)$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Real Price Nominal ES OLS, Treated + Never Treated--------------------------
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
      mutate(id = "rn_ols_tnt") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999"))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Real Price Nominal ES OLS, Treated------------------------------------------
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
      mutate(id = "rn_ols_t") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999" &
                                       treatment_group != 'Never Treated'))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Real Price Nominal ES FE, All-----------------------------------------------
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
      mutate(id = "rn_fe_all") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample)$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Real Price Nominal ES FE, Treated + Never Treated---------------------------
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
      mutate(id = "rn_fe_tnt") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999"))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Real Price Nominal ES FE, Treated-------------------------------------------
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
      mutate(id = "rn_fe_t") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999" &
                                       treatment_group != 'Never Treated'))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Data Wrangling for Event Studies - Real Event Time-----------------------------
et_sample <- dbGetQuery(iaan, "SELECT * FROM [DATA.ANALYSIS_SAMPLE] 
                                WHERE (event_time2 >= -151 AND
                                event_time2 <= 121) OR
                                (treatment_group = 'Never Treated')")
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


#Nominal Price Real ES OLS, All-------------------------------------------------
m <- lm(price ~ + as.factor(event_time) +
          I(30 - available_030) +
          price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
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
      mutate(id = "nr_ols_all") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample)$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Nominal Price Real ES OLS, Treated + Never Treated-----------------------------
m <- lm(price ~ + as.factor(event_time) +
          I(30 - available_030) +
          price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
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
      mutate(id = "nr_ols_tnt") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999"))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Nominal Price Real ES OLS, Treated---------------------------------------------
m <- lm(price ~ + as.factor(event_time) +
          I(30 - available_030) +
          price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
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
      mutate(id = "nr_ols_t") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999" &
                                       treatment_group != 'Never Treated'))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Nominal Price Real ES FE, All--------------------------------------------------
m <- felm(price ~ + as.factor(event_time) +
            I(30 - available_030) +
            price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
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
      mutate(id = "nr_fe_all") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample)$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Nominal Price Real ES FE, Treated + Never Treated------------------------------
m <- felm(price ~ + as.factor(event_time) +
            I(30 - available_030) +
            price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
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
      mutate(id = "nr_fe_tnt") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999"))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Nominal Price Real ES FE, Treated----------------------------------------------
m <- felm(price ~ + as.factor(event_time) +
            I(30 - available_030) +
            price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
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
      mutate(id = "nr_fe_t") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999" &
                                       treatment_group != 'Never Treated'))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()



#Real Price Real ES OLS, All----------------------------------------------------
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
      mutate(id = "rr_ols_all") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample)$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Real Price Real ES OLS, Treated + Never Treated--------------------------------
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
      mutate(id = "rr_ols_tnt") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999"))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Real Price Real ES OLS, Treated------------------------------------------------
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
      mutate(id = "rr_ols_t") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999" &
                                       treatment_group != 'Never Treated'))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Real Price Real ES FE, All-----------------------------------------------------
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
      mutate(id = "rr_fe_all") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample)$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Real Price Real ES FE, Treated + Never Treated---------------------------
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
      mutate(id = "rr_fe_tnt") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999"))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()


#Real Price Real ES FE, Treated-------------------------------------------
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
      mutate(id = "rr_fe_t") %>%
      mutate(n = length(m$residuals)) %>%
      mutate(nl = length(
        unique((et_sample %>% filter(event_time != "999" &
                                       treatment_group != 'Never Treated'))$KEY_LIST_ID)))
  )
rm(m, sm)
gc()

#Write To Database--------------------------------------------------------------
dbWriteTable(iaan, "MODEL.CLASSIC_EVENT_STUDIES_UNBAL", es_df,
             overwrite = T)
rm(es_df, et_sample)
gc()
