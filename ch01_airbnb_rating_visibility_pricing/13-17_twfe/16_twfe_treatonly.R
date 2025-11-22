if("MODEL.TWFE_TREATONLY_COI" %in% dbListTables(iaan)){
  
} else {
  #TWFE Treated Only--------------------------------------------------------------
  ia_sample <- dbGetQuery(iaan, "SELECT * 
                          FROM [DATA.ANALYSIS_SAMPLE] 
                          WHERE treatment_group = 'Treated'")
  
  #OLS
  res <- summary(lm(price ~ treated_after + I(30 - available_030) +
       price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
       number_of_reviews +
       as.factor(listing_type) + as.factor(room_type) + 
       AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
       as.factor(CITY_ID), ia_sample))$coefficients %>%
    as.data.frame() %>%
    rownames_to_column(var = "Feature") %>%
    select(Feature, Estimate, `Std. Error`) %>%
    filter(Feature == "treated_after" | Feature == "I(30 - available_030)") %>%
    mutate(Feature = ifelse(Feature == "I(30 - available_030)", "booked_nights",
                            Feature)) %>%
    dplyr::rename(SE = `Std. Error`) %>%
    mutate(Estimate = round(Estimate, 3),
           SE = round(SE, 3)) %>%
    mutate(target = "Nominal Price",
           method = "OLS")
  
  res <- res %>%
    rbind(
      summary(lm(real_price ~ treated_after + I(30 - available_030) +
               real_price1 + real_price2 + real_price3 + 
               bathrooms + bedrooms + capacity + covid +
               number_of_reviews +
               as.factor(listing_type) + as.factor(room_type) + 
               AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
               as.factor(CITY_ID), ia_sample))$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(Feature == "treated_after" | Feature == "I(30 - available_030)") %>%
      mutate(Feature = ifelse(Feature == "I(30 - available_030)", "booked_nights",
                              Feature)) %>%
      dplyr::rename(SE = `Std. Error`) %>%
      mutate(Estimate = round(Estimate, 3),
             SE = round(SE, 3)) %>%
      mutate(target = "Real Price",
             method = "OLS")
    )
  
  #Fixed Effects
  res <- res %>%
    rbind(
      summary(felm(
        price ~ treated_after + I(30 - available_030) +
          price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID,
        data = ia_sample
      ))$coefficients %>%
        as.data.frame() %>%
        rownames_to_column(var = "Feature") %>%
        select(Feature, Estimate, `Std. Error`) %>%
        filter(Feature == "treated_after" | Feature == "I(30 - available_030)") %>%
        mutate(Feature = ifelse(Feature == "I(30 - available_030)", "booked_nights",
                                Feature)) %>%
        dplyr::rename(SE = `Std. Error`) %>%
        mutate(Estimate = round(Estimate, 3),
               SE = round(SE, 3)) %>%
        mutate(target = "Nominal Price",
               method = "FE")
  )
  
  res <- res %>%
    rbind(
      summary(felm(
        real_price ~ treated_after + I(30 - available_030) +
          real_price1 + real_price2 + real_price3 + bathrooms + bedrooms + capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID,
        data = ia_sample
      ))$coefficients %>%
        as.data.frame() %>%
        rownames_to_column(var = "Feature") %>%
        select(Feature, Estimate, `Std. Error`) %>%
        filter(Feature == "treated_after" | Feature == "I(30 - available_030)") %>%
        mutate(Feature = ifelse(Feature == "I(30 - available_030)", "booked_nights",
                                Feature)) %>%
        dplyr::rename(SE = `Std. Error`) %>%
        mutate(Estimate = round(Estimate, 3),
               SE = round(SE, 3)) %>%
        mutate(target = "Real Price",
               method = "FE")
    )
  
  #IV
  res <- res %>% rbind(
    summary(felm(
      price ~ price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
        number_of_reviews + host_is_superhost +
        as.factor(room_type) + 
        AC7 + AC51 + AC72 + as.factor(year) + as.factor(month)  + 
        as.factor(listing_type) | CITY_ID |
        (treated_after | I(30 - available_030) ~  + as.factor(host_response_time)
         + host_response_rate + 
           host_accept_rate +
           host_has_pic + host_verified + host_about_word_count +
           amenity_word_count + descrip_word_count + loc_overview_word_count),data = ia_sample)
      )$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("treated_after", Feature) | grepl("available_030", Feature)) %>%
      mutate(Feature = 
               case_when(
                 grepl("treated_after", Feature) ~ "treated_after",
                 grepl("available_030", Feature) ~ "booked_nights"
               )
      ) %>%
      dplyr::rename(SE = `Std. Error`) %>%
      mutate(Estimate = round(Estimate, 3),
             SE = round(SE, 3)) %>%
      mutate(target = "Nominal Price",
             method = "IV")
  )
  
  res <- res %>% rbind(
    summary(felm(
      real_price ~ real_price1 + real_price2 + real_price3 + 
        bathrooms + bedrooms + capacity + covid +
        number_of_reviews + host_is_superhost +
        as.factor(room_type) + 
        AC7 + AC51 + AC72 + as.factor(year) + as.factor(month)  + 
        as.factor(listing_type) | CITY_ID |
        (treated_after | I(30 - available_030) ~  + as.factor(host_response_time)
         + host_response_rate + 
           host_accept_rate +
           host_has_pic + host_verified + host_about_word_count +
           amenity_word_count + descrip_word_count + loc_overview_word_count),data = ia_sample)
    )$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("treated_after", Feature) | grepl("available_030", Feature)) %>%
      mutate(Feature = 
               case_when(
                 grepl("treated_after", Feature) ~ "treated_after",
                 grepl("available_030", Feature) ~ "booked_nights"
               )
      ) %>%
      dplyr::rename(SE = `Std. Error`) %>%
      mutate(Estimate = round(Estimate, 3),
             SE = round(SE, 3)) %>%
      mutate(target = "Real Price",
             method = "IV")
  )
  
  #FEIV
  res <- res %>% rbind(
    summary(felm(
      price ~ price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
        number_of_reviews + host_is_superhost +
        as.factor(room_type) + 
        AC7 + AC51 + AC72 + as.factor(year) + as.factor(month)  + 
        as.factor(listing_type) | KEY_LIST_ID |
        (treated_after | I(30 - available_030) ~  + as.factor(host_response_time)
         + host_response_rate + 
           host_accept_rate +
           host_has_pic + host_verified + host_about_word_count +
           amenity_word_count + descrip_word_count + loc_overview_word_count),data = ia_sample)
    )$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("treated_after", Feature) | grepl("available_030", Feature)) %>%
      mutate(Feature = 
               case_when(
                 grepl("treated_after", Feature) ~ "treated_after",
                 grepl("available_030", Feature) ~ "booked_nights"
               )
      ) %>%
      dplyr::rename(SE = `Std. Error`) %>%
      mutate(Estimate = round(Estimate, 3),
             SE = round(SE, 3)) %>%
      mutate(target = "Nominal Price",
             method = "FEIV")
  )
  
  res <- res %>% rbind(
    summary(felm(
      real_price ~ real_price1 + real_price2 + real_price3 + 
        bathrooms + bedrooms + capacity + covid +
        number_of_reviews + host_is_superhost +
        as.factor(room_type) + 
        AC7 + AC51 + AC72 + as.factor(year) + as.factor(month)  + 
        as.factor(listing_type) | KEY_LIST_ID |
        (treated_after | I(30 - available_030) ~  + as.factor(host_response_time)
         + host_response_rate + 
           host_accept_rate +
           host_has_pic + host_verified + host_about_word_count +
           amenity_word_count + descrip_word_count + loc_overview_word_count),data = ia_sample)
    )$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("treated_after", Feature) | grepl("available_030", Feature)) %>%
      mutate(Feature = 
               case_when(
                 grepl("treated_after", Feature) ~ "treated_after",
                 grepl("available_030", Feature) ~ "booked_nights"
               )
      ) %>%
      dplyr::rename(SE = `Std. Error`) %>%
      mutate(Estimate = round(Estimate, 3),
             SE = round(SE, 3)) %>%
      mutate(target = "Real Price",
             method = "FEIV")
  )
  
  res <- res %>%
    mutate(rating_interaction = "No")
  res_twfe <- res
  
  #TWFE By Rating-----------------------------------------------------------------
  #OLS
  res_twfebr <- summary(lm(price ~ treated_after + r + I(treated_after*r) +
                      I(30 - available_030) +
                      price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
                      number_of_reviews +
                      as.factor(listing_type) + as.factor(room_type) + 
                      AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
                      as.factor(CITY_ID), ia_sample))$coefficients %>%
    as.data.frame() %>%
    rownames_to_column(var = "Feature") %>%
    select(Feature, Estimate, `Std. Error`) %>%
    filter(Feature == "treated_after" | Feature == "I(30 - available_030)" |
             Feature == "r" | Feature == "I(treated_after * r)") %>%
    mutate(Feature = case_when(Feature == "I(30 - available_030)" ~ "booked_nights",
                               Feature == "r" ~ "rating",
                               Feature == "I(treated_after * r)" ~ "treated*rating",
                            .default = Feature)) %>%
    dplyr::rename(SE = `Std. Error`) %>%
    mutate(Estimate = round(Estimate, 3),
           SE = round(SE, 3)) %>%
    mutate(target = "Nominal Price",
           method = "OLS")
  
  res_twfebr <- res_twfebr %>%
    rbind(
      summary(lm(real_price ~ treated_after + r + I(treated_after*r) +
                   I(30 - available_030) +
                   real_price1 + real_price2 + real_price3 + 
                   bathrooms + bedrooms + capacity + covid +
                   number_of_reviews +
                   as.factor(listing_type) + as.factor(room_type) + 
                   AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
                   as.factor(CITY_ID), ia_sample))$coefficients %>%
        as.data.frame() %>%
        rownames_to_column(var = "Feature") %>%
        select(Feature, Estimate, `Std. Error`) %>%
        filter(Feature == "treated_after" | Feature == "I(30 - available_030)" |
                 Feature == "r" | Feature == "I(treated_after * r)") %>%
        mutate(Feature = case_when(Feature == "I(30 - available_030)" ~ "booked_nights",
                                   Feature == "r" ~ "rating",
                                   Feature == "I(treated_after * r)" ~ "treated*rating",
                                   .default = Feature)) %>%
        dplyr::rename(SE = `Std. Error`) %>%
        mutate(Estimate = round(Estimate, 3),
               SE = round(SE, 3)) %>%
        mutate(target = "Real Price",
               method = "OLS")
    )
  
  #Fixed Effects
  res_twfebr <- res_twfebr %>%
    rbind(
      summary(felm(
        price ~ treated_after + r + I(treated_after*r) +
          I(30 - available_030) +
          price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID,
        data = ia_sample
      ))$coefficients %>%
        as.data.frame() %>%
        rownames_to_column(var = "Feature") %>%
        select(Feature, Estimate, `Std. Error`) %>%
        filter(Feature == "treated_after" | Feature == "I(30 - available_030)" |
                 Feature == "r" | Feature == "I(treated_after * r)") %>%
        mutate(Feature = case_when(Feature == "I(30 - available_030)" ~ "booked_nights",
                                   Feature == "r" ~ "rating",
                                   Feature == "I(treated_after * r)" ~ "treated*rating",
                                   .default = Feature)) %>%
        dplyr::rename(SE = `Std. Error`) %>%
        mutate(Estimate = round(Estimate, 3),
               SE = round(SE, 3)) %>%
        mutate(target = "Nominal Price",
               method = "FE")
    )
  
  res_twfebr <- res_twfebr %>%
    rbind(
      summary(felm(
        real_price ~ treated_after + r + I(treated_after*r) +
          I(30 - available_030) +
          real_price1 + real_price2 + real_price3 + bathrooms + bedrooms + capacity + covid +
          number_of_reviews +
          as.factor(listing_type) + as.factor(room_type) + 
          AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID,
        data = ia_sample
      ))$coefficients %>%
        as.data.frame() %>%
        rownames_to_column(var = "Feature") %>%
        select(Feature, Estimate, `Std. Error`) %>%
        filter(Feature == "treated_after" | Feature == "I(30 - available_030)" |
                 Feature == "r" | Feature == "I(treated_after * r)") %>%
        mutate(Feature = case_when(Feature == "I(30 - available_030)" ~ "booked_nights",
                                   Feature == "r" ~ "rating",
                                   Feature == "I(treated_after * r)" ~ "treated*rating",
                                   .default = Feature)) %>%
        dplyr::rename(SE = `Std. Error`) %>%
        mutate(Estimate = round(Estimate, 3),
               SE = round(SE, 3)) %>%
        mutate(target = "Real Price",
               method = "FE")
    )
  
  #IV
  res_twfebr <- res_twfebr %>% rbind(
    m <- summary(felm(
      price ~ price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
        number_of_reviews + host_is_superhost +
        as.factor(listing_type) + as.factor(room_type) + 
        AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | CITY_ID |
        (treated_after | I(30 - available_030) | r | I(treated_after*r) ~  + as.factor(host_response_time)
         + host_response_rate + 
           host_accept_rate +
           host_has_pic + host_verified + host_about_word_count +
           amenity_word_count + descrip_word_count + loc_overview_word_count +
           I(host_response_rate*r) + I(host_accept_rate*r) + I(host_has_pic*r) +
           I(host_verified*r) + I(host_about_word_count*r) + I(amenity_word_count*r) +
           I(descrip_word_count*r) + I(loc_overview_word_count*r)),
      data = ia_sample)
    )$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("treated_after", Feature) | grepl("available_030", Feature) |
               Feature == "`r(fit)`" | grepl("\\*", Feature)) %>%
      mutate(Feature = 
               case_when(
                 Feature == "`treated_after(fit)`" ~ "treated_after",
                 grepl("available_030", Feature) ~ "booked_nights",
                 Feature == "`r(fit)`" ~ "rating",
                 grepl("\\*", Feature) ~ "treated_after*rating"
               )
      ) %>%
      dplyr::rename(SE = `Std. Error`) %>%
      mutate(Estimate = round(Estimate, 3),
             SE = round(SE, 3)) %>%
      mutate(target = "Nominal Price",
             method = "IV")
  )
  
  res_twfebr <- res_twfebr %>% rbind(
    m <- summary(felm(
      real_price ~ real_price1 + real_price2 + real_price3 
      + bathrooms + bedrooms + capacity + covid +
        number_of_reviews + host_is_superhost +
        as.factor(listing_type) + as.factor(room_type) + 
        AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | CITY_ID |
        (treated_after | I(30 - available_030) | r | I(treated_after*r) ~  + as.factor(host_response_time)
         + host_response_rate + 
           host_accept_rate +
           host_has_pic + host_verified + host_about_word_count +
           amenity_word_count + descrip_word_count + loc_overview_word_count +
           I(host_response_rate*r) + I(host_accept_rate*r) + I(host_has_pic*r) +
           I(host_verified*r) + I(host_about_word_count*r) + I(amenity_word_count*r) +
           I(descrip_word_count*r) + I(loc_overview_word_count*r)),
      data = ia_sample)
    )$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("treated_after", Feature) | grepl("available_030", Feature) |
               Feature == "`r(fit)`" | grepl("\\*", Feature)) %>%
      mutate(Feature = 
               case_when(
                 Feature == "`treated_after(fit)`" ~ "treated_after",
                 grepl("available_030", Feature) ~ "booked_nights",
                 Feature == "`r(fit)`" ~ "rating",
                 grepl("\\*", Feature) ~ "treated_after*rating"
               )
      ) %>%
      dplyr::rename(SE = `Std. Error`) %>%
      mutate(Estimate = round(Estimate, 3),
             SE = round(SE, 3)) %>%
      mutate(target = "Real Price",
             method = "IV")
  )
  
  #FEIV
  res_twfebr <- res_twfebr %>% rbind(
    m <- summary(felm(
      price ~ price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
        number_of_reviews + host_is_superhost +
        as.factor(listing_type) + as.factor(room_type) + 
        AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID |
        (treated_after | I(30 - available_030) | r | I(treated_after*r) ~  + as.factor(host_response_time)
         + host_response_rate + 
           host_accept_rate +
           host_has_pic + host_verified + host_about_word_count +
           amenity_word_count + descrip_word_count + loc_overview_word_count +
           I(host_response_rate*r) + I(host_accept_rate*r) + I(host_has_pic*r) +
           I(host_verified*r) + I(host_about_word_count*r) + I(amenity_word_count*r) +
           I(descrip_word_count*r) + I(loc_overview_word_count*r)),
      data = ia_sample)
    )$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("treated_after", Feature) | grepl("available_030", Feature) |
               Feature == "`r(fit)`" | grepl("\\*", Feature)) %>%
      mutate(Feature = 
               case_when(
                 Feature == "`treated_after(fit)`" ~ "treated_after",
                 grepl("available_030", Feature) ~ "booked_nights",
                 Feature == "`r(fit)`" ~ "rating",
                 grepl("\\*", Feature) ~ "treated_after*rating"
               )
      ) %>%
      dplyr::rename(SE = `Std. Error`) %>%
      mutate(Estimate = round(Estimate, 3),
             SE = round(SE, 3)) %>%
      mutate(target = "Nominal Price",
             method = "FEIV")
  )
  
  res_twfebr <- res_twfebr %>% rbind(
    m <- summary(felm(
      real_price ~ real_price1 + real_price2 + real_price3 
      + bathrooms + bedrooms + capacity + covid +
        number_of_reviews + host_is_superhost +
        as.factor(listing_type) + as.factor(room_type) + 
        AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID |
        (treated_after | I(30 - available_030) | r | I(treated_after*r) ~  + as.factor(host_response_time)
         + host_response_rate + 
           host_accept_rate +
           host_has_pic + host_verified + host_about_word_count +
           amenity_word_count + descrip_word_count + loc_overview_word_count +
           I(host_response_rate*r) + I(host_accept_rate*r) + I(host_has_pic*r) +
           I(host_verified*r) + I(host_about_word_count*r) + I(amenity_word_count*r) +
           I(descrip_word_count*r) + I(loc_overview_word_count*r)),
      data = ia_sample)
    )$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      select(Feature, Estimate, `Std. Error`) %>%
      filter(grepl("treated_after", Feature) | grepl("available_030", Feature) |
               Feature == "`r(fit)`" | grepl("\\*", Feature)) %>%
      mutate(Feature = 
               case_when(
                 Feature == "`treated_after(fit)`" ~ "treated_after",
                 grepl("available_030", Feature) ~ "booked_nights",
                 Feature == "`r(fit)`" ~ "rating",
                 grepl("\\*", Feature) ~ "treated_after*rating"
               )
      ) %>%
      dplyr::rename(SE = `Std. Error`) %>%
      mutate(Estimate = round(Estimate, 3),
             SE = round(SE, 3)) %>%
      mutate(target = "Real Price",
             method = "FEIV")
  )
  
  #Combine Results-----------------------------------------------------------------
  res_twfebr <- res_twfebr %>%
    mutate(rating_interaction = "yes")
  res <- rbind(res_twfe, res_twfebr)
  dbWriteTable(iaan, "MODEL.TWFE_TREATONLY_COI", res, overwrite = T)

}
  