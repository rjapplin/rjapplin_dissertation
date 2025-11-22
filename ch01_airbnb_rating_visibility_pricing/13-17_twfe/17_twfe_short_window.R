twfe_short <- dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
  filter((event_time2 <= 150 & event_time2 >= -150) | 
           treatment_group == "Never Treated")

#OLS: Target - Price------------------------------------------------------------
m <- lm(
  price ~ treated_after + I(30 - available_030) +
    price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
    as.factor(CITY_ID),
  data = twfe_short
)
sm <- summary(m)

#Create Formatted Coefficient Table
res <- sm$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  filter(Feature == "treated_after" | grepl("available_030", Feature)) %>%
  dplyr::rename(p = `Pr(>|t|)`) %>%
  select(Feature, Estimate, p) %>%
  mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
  mutate(Estimate = round(Estimate, 3)) %>%
  mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
  select(Feature, Estimate, p) %>%
  mutate(Method = "OLS") %>%
  mutate(Target = "Nominal Price") %>%
  mutate(Feature = 
           case_when(
             grepl("treated_after", Feature) ~ "treated",
             grepl("available_030", Feature) ~ "booked_nights"
           )
  ) %>%
  mutate(`Includes Rating` = 'No') %>%
  select(Feature, Method, Target, `Includes Rating`, Estimate, p)
  
#Remove Model Objects
rm(m, sm)
gc()

#OLS: Target - Real Price-------------------------------------------------------
m <- lm(
  real_price ~ treated_after + I(30 - available_030) +
    real_price1 + real_price2 + real_price3 + bathrooms + bedrooms + capacity + 
    covid + number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
    as.factor(CITY_ID),
  data = twfe_short
)

sm <- summary(m)

#Create Formatted Coefficient Table
res <- res %>%
  rbind(
    sm$coefficients %>% 
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      filter(Feature == "treated_after" | grepl("available_030", Feature)) %>%
      dplyr::rename(p = `Pr(>|t|)`) %>%
      select(Feature, Estimate, p) %>%
      mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
      mutate(Estimate = round(Estimate, 3)) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(Feature, Estimate, p) %>%
      mutate(Method = "OLS") %>%
      mutate(Target = "Real Price") %>%
      mutate(Feature = 
               case_when(
                 grepl("treated_after", Feature) ~ "treated",
                 grepl("available_030", Feature) ~ "booked_nights"
               )
      ) %>%
      mutate(`Includes Rating` = 'No') %>%
      select(Feature, Method, Target, `Includes Rating`, Estimate, p)
  )

#Remove Model Objects
rm(m, sm)
gc()

#IV: Target - Price-------------------------------------------------------------
  m <- felm(
    price ~ price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
      number_of_reviews + host_is_superhost +
      as.factor(room_type) + 
      AC7 + AC51 + AC72 + as.factor(year) + as.factor(month)  + 
      as.factor(listing_type) | CITY_ID |
      (treated_after | I(30 - available_030) ~  + as.factor(host_response_time)
       + host_response_rate + 
         host_accept_rate +
         host_has_pic + host_verified + host_about_word_count +
         amenity_word_count + descrip_word_count + loc_overview_word_count),
    data = twfe_short)
  sm <- summary(m)
  
  res <- res %>%
    rbind(
      sm$coefficients %>% 
        as.data.frame() %>%
        rownames_to_column(var = "Feature") %>%
        filter(grepl("treated_after", Feature) |
                 grepl("available_030", Feature)) %>%
        dplyr::rename(p = `Pr(>|t|)`) %>%
        select(Feature, Estimate, p) %>%
        mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
        mutate(Estimate = round(Estimate, 3)) %>%
        mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
        select(Feature, Estimate, p) %>%
        mutate(Method = "IV") %>%
        mutate(Target = "Nominal Price") %>%
        mutate(Feature = 
                 case_when(
                   grepl("treated_after", Feature) ~ "treated",
                   grepl("available_030", Feature) ~ "booked_nights"
                 )
        ) %>%
        mutate(`Includes Rating` = 'No') %>%
        select(Feature, Method, Target, `Includes Rating`, Estimate, p)
    )
  
  rm(m, sm)
  gc()


#IV: Target Real Price----------------------------------------------------------
  m <- felm(
    real_price ~ real_price1 + real_price2 + real_price3 + 
      bathrooms + bedrooms + capacity + covid +
      number_of_reviews + host_is_superhost +
      as.factor(room_type) + 
      AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) + 
      as.factor(listing_type) | CITY_ID |
      (treated_after | I(30 - available_030) ~  + as.factor(host_response_time)
       + host_response_rate + 
         host_accept_rate +
         host_has_pic + host_verified + host_about_word_count +
         amenity_word_count + descrip_word_count + loc_overview_word_count),
    data = twfe_short) 
  sm <- summary(m)
  
  res <- res %>%
    rbind(
      sm$coefficients %>% 
        as.data.frame() %>%
        rownames_to_column(var = "Feature") %>%
        filter(grepl("treated_after", Feature) |
                 grepl("available_030", Feature)) %>%
        dplyr::rename(p = `Pr(>|t|)`) %>%
        select(Feature, Estimate, p) %>%
        mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
        mutate(Estimate = round(Estimate, 3)) %>%
        mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
        select(Feature, Estimate, p) %>%
        mutate(Method = "IV") %>%
        mutate(Target = "Real Price") %>%
        mutate(Feature = 
                 case_when(
                   grepl("treated_after", Feature) ~ "treated",
                   grepl("available_030", Feature) ~ "booked_nights"
                 )
        ) %>%
        mutate(`Includes Rating` = 'No') %>%
        select(Feature, Method, Target, `Includes Rating`, Estimate, p)
    )
  
  rm(m, sm)
  gc()

  

#FE Target - Price-----------------------------------------------------------------------
m <- felm(
  price ~ treated_after + I(30 - available_030) +
    price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID,
  data = twfe_short
)
sm <- summary(m)

#Create Formatted Coefficient Table
res <- res %>%
  rbind(
    sm$coefficients %>% 
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      filter(grepl("treated_after", Feature) |
               grepl("available_030", Feature)) %>%
      dplyr::rename(p = `Pr(>|t|)`) %>%
      select(Feature, Estimate, p) %>%
      mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
      mutate(Estimate = round(Estimate, 3)) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(Feature, Estimate, p) %>%
      mutate(Method = "FE") %>%
      mutate(Target = "Nominal Price") %>%
      mutate(Feature = 
               case_when(
                 grepl("treated_after", Feature) ~ "treated",
                 grepl("available_030", Feature) ~ "booked_nights"
               )
      ) %>%
      mutate(`Includes Rating` = 'No') %>%
      select(Feature, Method, Target, `Includes Rating`, Estimate, p)
  )

rm(m, sm)
gc()

#FE Target - Real Price---------------------------------------------------------
m <- felm(
  real_price ~ treated_after + I(30 - available_030) +
    real_price1 + real_price2 + real_price3 + bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID,
  data = twfe_short
)
sm <- summary(m)

res <- res %>%
  rbind(
    sm$coefficients %>% 
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      filter(grepl("treated_after", Feature) |
               grepl("available_030", Feature)) %>%
      dplyr::rename(p = `Pr(>|t|)`) %>%
      select(Feature, Estimate, p) %>%
      mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
      mutate(Estimate = round(Estimate, 3)) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(Feature, Estimate, p) %>%
      mutate(Method = "FE") %>%
      mutate(Target = "Real Price") %>%
      mutate(Feature = 
               case_when(
                 grepl("treated_after", Feature) ~ "treated",
                 grepl("available_030", Feature) ~ "booked_nights"
               )
      ) %>%
      mutate(`Includes Rating` = 'No') %>%
      select(Feature, Method, Target, `Includes Rating`, Estimate, p)
  )

rm(m, sm)
gc()


#FE IV Price--------------------------------------------------------------------
  m <- felm(
    price ~ price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
      number_of_reviews + host_is_superhost +
      as.factor(listing_type) + as.factor(room_type) + 
      AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID |
      (treated_after | I(30 - available_030) ~  + as.factor(host_response_time)
       + host_response_rate + 
         host_accept_rate +
         host_has_pic + host_verified + host_about_word_count +
         amenity_word_count + descrip_word_count + loc_overview_word_count),
    data = twfe_short
  )
  sm <- summary(m)
  
  res <- res %>%
    rbind(
      sm$coefficients %>% 
        as.data.frame() %>%
        rownames_to_column(var = "Feature") %>%
        filter(grepl("treated_after", Feature) |
                 grepl("available_030", Feature)) %>%
        dplyr::rename(p = `Pr(>|t|)`) %>%
        select(Feature, Estimate, p) %>%
        mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
        mutate(Estimate = round(Estimate, 3)) %>%
        mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
        select(Feature, Estimate, p) %>%
        mutate(Method = "FEIV") %>%
        mutate(Target = "Nominal Price") %>%
        mutate(Feature = 
                 case_when(
                   grepl("treated_after", Feature) ~ "treated",
                   grepl("available_030", Feature) ~ "booked_nights"
                 )
        ) %>%
        mutate(`Includes Rating` = 'No') %>%
        select(Feature, Method, Target, `Includes Rating`, Estimate, p)
    )
  
  rm(m, sm)
  gc()


#FE IV: Target Real Price--------------------------------------------------------
  m <- felm(
    real_price ~ real_price1 + real_price2 + real_price3 + 
      bathrooms + bedrooms + capacity + covid +
      number_of_reviews + host_is_superhost +
      as.factor(listing_type) + as.factor(room_type) + 
      AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID |
      (treated_after | I(30 - available_030) ~  + as.factor(host_response_time)
       + host_response_rate + 
         host_accept_rate +
         host_has_pic + host_verified + host_about_word_count +
         amenity_word_count + descrip_word_count + loc_overview_word_count),
    data = twfe_short
  )
  sm <- summary(m)
  
  res <- res %>%
    rbind(
      sm$coefficients %>% 
        as.data.frame() %>%
        rownames_to_column(var = "Feature") %>%
        filter(grepl("treated_after", Feature) |
                 grepl("available_030", Feature)) %>%
        dplyr::rename(p = `Pr(>|t|)`) %>%
        select(Feature, Estimate, p) %>%
        mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
        mutate(Estimate = round(Estimate, 3)) %>%
        mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
        select(Feature, Estimate, p) %>%
        mutate(Method = "FEIV") %>%
        mutate(Target = "Real Price") %>%
        mutate(Feature = 
                 case_when(
                   grepl("treated_after", Feature) ~ "treated",
                   grepl("available_030", Feature) ~ "booked_nights"
                 )
        ) %>%
        mutate(`Includes Rating` = 'No') %>%
        select(Feature, Method, Target, `Includes Rating`, Estimate, p)
    )
  
  rm(m, sm)
  gc()
  
#OLS: Target - Price, With Rating Int-------------------------------------------
m <- lm(
  price ~ treated_after + I(30 - available_030) + r + I(treated_after*r) +
    price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
    as.factor(CITY_ID),
  data = twfe_short
)
sm <- summary(m)
  
#Create Formatted Coefficient Table
res <- res %>%
  rbind(
    sm$coefficients %>% 
    as.data.frame() %>%
    rownames_to_column(var = "Feature") %>%
    filter(grepl("treated_after", Feature) | 
             grepl("available_030", Feature) |
             Feature == "r") %>%
    dplyr::rename(p = `Pr(>|t|)`) %>%
    select(Feature, Estimate, p) %>%
    mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
    mutate(Estimate = round(Estimate, 3)) %>%
    mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
    select(Feature, Estimate, p) %>%
    mutate(Method = "OLS") %>%
    mutate(Target = "Nominal Price") %>%
    mutate(Feature = 
             case_when(
               grepl("\\(treated_after", Feature) ~ "treated*rating",
               Feature == "treated_after" ~ "treated",
               grepl("available_030", Feature) ~ "booked_nights",
               Feature == "r" ~ "rating"
             )
    ) %>%
    mutate(`Includes Rating` = 'Yes') %>%
    select(Feature, Method, Target, `Includes Rating`, Estimate, p)
  )

rm(m, sm)
gc()

#OLS: Target - Real Price, With Rating Int--------------------------------------
m <- lm(
  real_price ~ treated_after + I(30 - available_030) + r + I(treated_after*r) +
    real_price1 + real_price2 + real_price3 + bathrooms + bedrooms + capacity + 
    covid + number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
    as.factor(CITY_ID),
  data = twfe_short
)
sm <- summary(m)

res <- res %>%
  rbind(
    sm$coefficients %>% 
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      filter(grepl("treated_after", Feature) | 
               grepl("available_030", Feature) |
               Feature == "r") %>%
      dplyr::rename(p = `Pr(>|t|)`) %>%
      select(Feature, Estimate, p) %>%
      mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
      mutate(Estimate = round(Estimate, 3)) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(Feature, Estimate, p) %>%
      mutate(Method = "OLS") %>%
      mutate(Target = "Real Price") %>%
      mutate(Feature = 
               case_when(
                 grepl("\\(treated_after", Feature) ~ "treated*rating",
                 Feature == "treated_after" ~ "treated",
                 grepl("available_030", Feature) ~ "booked_nights",
                 Feature == "r" ~ "rating"
               )
      ) %>%
      mutate(`Includes Rating` = 'Yes') %>%
      select(Feature, Method, Target, `Includes Rating`, Estimate, p)
  )

rm(m, sm)
gc()

#IV: Target - Price, With Rating Int--------------------------------------------
m <- felm(
  price ~ price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
    number_of_reviews + host_is_superhost +
    as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month)  + 
    as.factor(listing_type) | CITY_ID |
    (treated_after | I(30 - available_030) | r | I(treated_after*r) ~  + as.factor(host_response_time)
     + host_response_rate + 
       host_accept_rate +
       host_has_pic + host_verified + host_about_word_count +
       amenity_word_count + descrip_word_count + loc_overview_word_count +
       I(host_response_rate*r) + I(host_accept_rate*r) + I(host_has_pic*r) +
       I(host_verified*r) + I(host_about_word_count*r) + I(amenity_word_count*r) +
       I(descrip_word_count*r) + I(loc_overview_word_count*r)),
  data = twfe_short)
sm <- summary(m)


res <- res %>%
  rbind(
    sm$coefficients %>% 
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      filter(grepl("treated_after", Feature) | 
               grepl("available_030", Feature) |
               Feature == "`r(fit)`") %>%
      dplyr::rename(p = `Pr(>|t|)`) %>%
      select(Feature, Estimate, p) %>%
      mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
      mutate(Estimate = round(Estimate, 3)) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(Feature, Estimate, p) %>%
      mutate(Method = "IV") %>%
      mutate(Target = "Nominal Price") %>%
      mutate(Feature = 
               case_when(
                 grepl("\\(treated_after", Feature) ~ "treated*rating",
                 grepl("treated_after", Feature) ~ "treated",
                 grepl("available_030", Feature) ~ "booked_nights",
                 Feature == "`r(fit)`" ~ "rating"
               )
      ) %>%
      mutate(`Includes Rating` = 'Yes') %>%
      select(Feature, Method, Target, `Includes Rating`, Estimate, p)
  )

rm(m, sm)
gc()

#IV: Target - Real Price, With Rating Int---------------------------------------
m <- felm(
  real_price ~ real_price1 + real_price2 + real_price3 
  + bathrooms + bedrooms + capacity + covid +
    number_of_reviews + host_is_superhost +
    as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month)  + 
    as.factor(listing_type) | CITY_ID |
    (treated_after | I(30 - available_030) | r | I(treated_after*r) ~  + as.factor(host_response_time)
     + host_response_rate + 
       host_accept_rate +
       host_has_pic + host_verified + host_about_word_count +
       amenity_word_count + descrip_word_count + loc_overview_word_count +
       I(host_response_rate*r) + I(host_accept_rate*r) + I(host_has_pic*r) +
       I(host_verified*r) + I(host_about_word_count*r) + I(amenity_word_count*r) +
       I(descrip_word_count*r) + I(loc_overview_word_count*r)),
  data = twfe_short)
sm <- summary(m)


res <- res %>%
  rbind(
    sm$coefficients %>% 
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      filter(grepl("treated_after", Feature) | 
               grepl("available_030", Feature) |
               Feature == "`r(fit)`") %>%
      dplyr::rename(p = `Pr(>|t|)`) %>%
      select(Feature, Estimate, p) %>%
      mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
      mutate(Estimate = round(Estimate, 3)) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(Feature, Estimate, p) %>%
      mutate(Method = "IV") %>%
      mutate(Target = "Real Price") %>%
      mutate(Feature = 
               case_when(
                 grepl("\\(treated_after", Feature) ~ "treated*rating",
                 grepl("treated_after", Feature) ~ "treated",
                 grepl("available_030", Feature) ~ "booked_nights",
                 Feature == "`r(fit)`" ~ "rating"
               )
      ) %>%
      mutate(`Includes Rating` = 'Yes') %>%
      select(Feature, Method, Target, `Includes Rating`, Estimate, p)
  )

rm(m, sm)
gc()

#FE Target - Price, With Rating Int---------------------------------------------
m <- felm(
  price ~ treated_after + I(30 - available_030) + r + I(treated_after*r) +
    price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID,
  data = twfe_short
)
sm <- summary(m)

#Create Formatted Coefficient Table
res <- res %>%
  rbind(
    sm$coefficients %>% 
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      filter(grepl("treated_after", Feature) |
               grepl("available_030", Feature) |
               Feature == "r") %>%
      dplyr::rename(p = `Pr(>|t|)`) %>%
      select(Feature, Estimate, p) %>%
      mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
      mutate(Estimate = round(Estimate, 3)) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(Feature, Estimate, p) %>%
      mutate(Method = "FE") %>%
      mutate(Target = "Nominal Price") %>%
      mutate(Feature = 
               case_when(
                 grepl("\\(treated_after", Feature) ~ "treated*rating",
                 grepl("treated_after", Feature) ~ "treated",
                 grepl("available_030", Feature) ~ "booked_nights",
                 Feature == "r" ~ "rating"
               )
      ) %>%
      mutate(`Includes Rating` = 'Yes') %>%
      select(Feature, Method, Target, `Includes Rating`, Estimate, p)
  )

rm(m, sm)
gc()

#FE Target - Real Price, With Rating Int----------------------------------------
m <- felm(
  real_price ~ treated_after + I(30 - available_030) + r + I(treated_after*r) +
    real_price1 + real_price2 + real_price3 + 
    bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID,
  data = twfe_short
)
sm <- summary(m)

#Create Formatted Coefficient Table
res <- res %>%
  rbind(
    sm$coefficients %>% 
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      filter(grepl("treated_after", Feature) |
               grepl("available_030", Feature) | 
               Feature == "r") %>%
      dplyr::rename(p = `Pr(>|t|)`) %>%
      select(Feature, Estimate, p) %>%
      mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
      mutate(Estimate = round(Estimate, 3)) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(Feature, Estimate, p) %>%
      mutate(Method = "FE") %>%
      mutate(Target = "Real Price") %>%
      mutate(Feature = 
               case_when(
                 grepl("\\(treated_after", Feature) ~ "treated*rating",
                 grepl("treated_after", Feature) ~ "treated",
                 grepl("available_030", Feature) ~ "booked_nights",
                 Feature == "r" ~ "rating"
               )
      ) %>%
      mutate(`Includes Rating` = 'Yes') %>%
      select(Feature, Method, Target, `Includes Rating`, Estimate, p)
  )

rm(m, sm)
gc()

#IV: Target - Price, With Rating Int--------------------------------------------
m <- felm(
  price ~ price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
    number_of_reviews + host_is_superhost +
    as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month)  + 
    as.factor(listing_type) | KEY_LIST_ID |
    (treated_after | I(30 - available_030) | r | I(treated_after*r) ~  + as.factor(host_response_time)
     + host_response_rate + 
       host_accept_rate +
       host_has_pic + host_verified + host_about_word_count +
       amenity_word_count + descrip_word_count + loc_overview_word_count +
       I(host_response_rate*r) + I(host_accept_rate*r) + I(host_has_pic*r) +
       I(host_verified*r) + I(host_about_word_count*r) + I(amenity_word_count*r) +
       I(descrip_word_count*r) + I(loc_overview_word_count*r)),
  data = twfe_short)
sm <- summary(m)


res <- res %>%
  rbind(
    sm$coefficients %>% 
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      filter(grepl("treated_after", Feature) | 
               grepl("available_030", Feature) |
               Feature == "`r(fit)`") %>%
      dplyr::rename(p = `Pr(>|t|)`) %>%
      select(Feature, Estimate, p) %>%
      mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
      mutate(Estimate = round(Estimate, 3)) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(Feature, Estimate, p) %>%
      mutate(Method = "FEIV") %>%
      mutate(Target = "Nominal Price") %>%
      mutate(Feature = 
               case_when(
                 grepl("\\(treated_after", Feature) ~ "treated*rating",
                 grepl("treated_after", Feature) ~ "treated",
                 grepl("available_030", Feature) ~ "booked_nights",
                 Feature == "`r(fit)`" ~ "rating"
               )
      ) %>%
      mutate(`Includes Rating` = 'Yes') %>%
      select(Feature, Method, Target, `Includes Rating`, Estimate, p)
  )

rm(m, sm)
gc()

#IV: Target - Real Price, With Rating Int---------------------------------------
m <- felm(
  real_price ~ real_price1 + real_price2 + real_price3 + 
    bathrooms + bedrooms + capacity + covid +
    number_of_reviews + host_is_superhost +
    as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month)  + 
    as.factor(listing_type) | KEY_LIST_ID |
    (treated_after | I(30 - available_030) | r | I(treated_after*r) ~  + as.factor(host_response_time)
     + host_response_rate + 
       host_accept_rate +
       host_has_pic + host_verified + host_about_word_count +
       amenity_word_count + descrip_word_count + loc_overview_word_count +
       I(host_response_rate*r) + I(host_accept_rate*r) + I(host_has_pic*r) +
       I(host_verified*r) + I(host_about_word_count*r) + I(amenity_word_count*r) +
       I(descrip_word_count*r) + I(loc_overview_word_count*r)),
  data = twfe_short)
sm <- summary(m)


res <- res %>%
  rbind(
    sm$coefficients %>% 
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      filter(grepl("treated_after", Feature) | 
               grepl("available_030", Feature) |
               Feature == "`r(fit)`") %>%
      dplyr::rename(p = `Pr(>|t|)`) %>%
      select(Feature, Estimate, p) %>%
      mutate(sig = ifelse(p <= 0.01, "^*", "")) %>%
      mutate(Estimate = round(Estimate, 3)) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(Feature, Estimate, p) %>%
      mutate(Method = "FEIV") %>%
      mutate(Target = "Real Price") %>%
      mutate(Feature = 
               case_when(
                 grepl("\\(treated_after", Feature) ~ "treated*rating",
                 grepl("treated_after", Feature) ~ "treated",
                 grepl("available_030", Feature) ~ "booked_nights",
                 Feature == "`r(fit)`" ~ "rating"
               )
      ) %>%
      mutate(`Includes Rating` = 'Yes') %>%
      select(Feature, Method, Target, `Includes Rating`, Estimate, p)
  )

rm(m, sm)
gc()

#Write To Database--------------------------------------------------------------
dbWriteTable(iaan, "MODEL.TWFE_SHORTWIN", res, overwrite = T)
rm(twfe_short, res)
