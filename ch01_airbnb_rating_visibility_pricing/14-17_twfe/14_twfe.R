#OLS: Target - Price------------------------------------------------------------
m <- lm(
  price ~ treated_after + I(30 - available_030) +
    price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
    as.factor(CITY_ID),
  data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE")
)
sm <- summary(m)

#Create Formatted Coefficient Table
sm$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  filter(Feature %in% c("treated_after", "number_of_reviews", 
                        "I(30 - available_030)",
                        "price1", "price2", "price3", "bathrooms", "bedrooms",
                        "capacity", "covid", "AC7", "AC51", "AC72")) %>%
  dplyr::rename(SE = `Std. Error`,
                t = `t value`,
                p = `Pr(>|t|)`) %>%
  mutate(Estimate = round(Estimate, 3),
         SE = round(SE, 3),
         t = round(t, 3),
         p = round(p, 3)) %>%
  mutate(sig = ifelse(p < 0.01, "*", "")) %>%
  dplyr::select(Feature, Estimate, SE, sig) %>%
  pivot_longer(cols = c(Estimate, SE)) %>%
  mutate(sig = ifelse(name == "SE", "", sig)) %>%
  mutate(Feature = ifelse(name == "SE", "", Feature)) %>%
  mutate(value = ifelse(name == "SE", 
                        paste0("$(", value, ")$"), 
                        paste0("$", value, "^", sig, "$"))) %>%
  mutate(value = ifelse(name == "Estimate" & sig != "*",
                        gsub("^", "", value),
                        value)
  ) %>%
  dplyr::select(-name, -sig) %>%
  as.data.frame() %>%
  rbind(
    data.frame(Feature = "R Squared", value = round(sm$r.squared, 3)),
    data.frame(Feature = "Adj. R Squared", value = round(sm$adj.r.squared, 3)),
    data.frame(Feature = "F Stat", value = round(sm$fstatistic[[1]], 3))
  ) %>%
  mutate(Feature = case_when(
    Feature == "treated_after" ~ "treated",
    Feature == "I(30 - available_030)" ~ "booked_nights",
    .default = Feature
  )) %>%
  dplyr::rename(OLS = value) %>%
  dbWriteTable(iaan, "MODEL.PRICE_OLS_FORMATTED", ., overwrite = T)

#Save Coefficients
sm$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  select(Feature, Estimate) %>%
  dbWriteTable(iaan, "MODEL.PRICE_OLS_COEFS", ., overwrite = T)

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
  data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE")
)
  
sm <- summary(m)

#Create Formatted Coefficient Table
sm$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  filter(Feature %in% c("treated_after", "number_of_reviews", 
                        "I(30 - available_030)",
                        "real_price1", "real_price2", "real_price3", 
                        "bathrooms", "bedrooms",
                        "capacity", "covid", "AC7", "AC51", "AC72")) %>%
  dplyr::rename(SE = `Std. Error`,
                t = `t value`,
                p = `Pr(>|t|)`) %>%
  mutate(Estimate = round(Estimate, 3),
         SE = round(SE, 3),
         t = round(t, 3),
         p = round(p, 3)) %>%
  mutate(sig = ifelse(p < 0.01, "*", "")) %>%
  dplyr::select(Feature, Estimate, SE, sig) %>%
  pivot_longer(cols = c(Estimate, SE)) %>%
  mutate(sig = ifelse(name == "SE", "", sig)) %>%
  mutate(Feature = ifelse(name == "SE", "", Feature)) %>%
  mutate(value = ifelse(name == "SE", 
                        paste0("$(", value, ")$"), 
                        paste0("$", value, "^", sig, "$"))) %>%
  mutate(value = ifelse(name == "Estimate" & sig != "*",
                        gsub("^", "", value),
                        value)
  ) %>%
  dplyr::select(-name, -sig) %>%
  as.data.frame() %>%
  rbind(
    data.frame(Feature = "R Squared", value = round(sm$r.squared, 3)),
    data.frame(Feature = "Adj. R Squared", value = round(sm$adj.r.squared, 3)),
    data.frame(Feature = "F Stat", value = round(sm$fstatistic[[1]], 3))
  ) %>%
  mutate(Feature = case_when(
    Feature == "treated_after" ~ "treated",
    Feature == "I(30 - available_030)" ~ "booked_nights",
    .default = Feature
  )) %>%
  dplyr::rename(OLS = value) %>%
  dbWriteTable(iaan, "MODEL.REAL_PRICE_OLS_FORMATTED", ., overwrite = T)

#Save Coefficients
sm$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  select(Feature, Estimate) %>%
  dbWriteTable(iaan, "MODEL.REAL_PRICE_OLS_COEFS", ., overwrite = T)

#Remove Model Objects
rm(m, sm)
gc()

#IV: Target - Price-------------------------------------------------------------
for(i in 1:100){
  
  if(!(exists("all_listings"))){
    all_listings <- dbGetQuery(iaan, "SELECT DISTINCT KEY_LIST_ID
                                FROM [DATA.ANALYSIS_SAMPLE]")$KEY_LIST_ID
  }
  seed <- sample(1:999999999, 1)
  set.seed(seed)
  samp_listings <- sample(all_listings, round(0.5*length(all_listings)), T)
  
  dbExecute(iaan,
            "CREATE TABLE IF NOT EXISTS [MODEL.PRICE_IV_LOOP_RES] (
            Feature TEXT,
            Estimate REAL,
            SE REAL,
            t REAL,
            p REAL,
            iteration INT,
            seed REAL
            )")
  
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
    data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
      select(price, price1, price2, price3, bathrooms, bedrooms, capacity,
             covid, number_of_reviews, host_is_superhost, listing_type,
             room_type, AC7, AC51, AC72, year, month, KEY_LIST_ID, CITY_ID,
             treated_after, available_030, host_response_time, host_response_rate,
             host_accept_rate, host_has_pic, host_verified, host_about_word_count,
             amenity_word_count, descrip_word_count, loc_overview_word_count) %>%
      na.omit() %>%
      filter(KEY_LIST_ID %in% samp_listings)
  )
  sm <- summary(m)
  condf <- condfstat(m)
  
  sm$coefficients %>% 
    as.data.frame() %>%
    rownames_to_column(var = "Feature") %>%
    dplyr::filter(Feature %in% c("number_of_reviews",
                                 "price1", "price2", "price3", "bathrooms", "bedrooms",
                                 "capacity", "covid", "AC7", "AC51", "AC72") |
                    grepl("fit", Feature)) %>%
    dplyr::mutate(n = case_when(
      Feature == "`treated_after(fit)`" ~ 1,
      Feature == "`I(30 - available_030)(fit)`" ~ 2,
      .default = 999
    )) %>%
    dplyr::arrange(n) %>%
    dplyr::select(-n) %>%
    dplyr::rename(SE = `Std. Error`,
                  t = `t value`,
                  p = `Pr(>|t|)`) %>%
    dplyr::mutate(Estimate = round(Estimate, 3),
                  SE = round(SE, 3),
                  t = round(t, 3),
                  p = round(p, 3)) %>%
    as.data.frame() %>%
    plyr::rbind.fill(
      data.frame(Feature = "R Squared", Estimate = round(sm$r.squared, 3)),
      data.frame(Feature = "Adj. R Squared", Estimate = round(sm$adj.r.squared, 3)),
      data.frame(Feature = "Proj. R Squared", Estimate = round(sm$P.r.squared[[1]], 3)),
      data.frame(Feature = "Proj. Adj. R Squared", Estimate = round(sm$P.adj.r.squared[[1]], 3)),
      data.frame(Feature = "F Stat", Estimate = round(sm$fstat[[1]], 3)),
      data.frame(Feature = "Proj. F Stat", Estimate = round(sm$P.fstat[[5]], 3)),
      data.frame(Feature = "Endog. F Stat", Estimate = round(sm$E.fstat[[5]], 3)),
      data.frame(Feature = "treated Cond. F Stat", 
                 Estimate = round(condf[1], 3)),
      data.frame(Feature = "booked_nights Cond. F Stat",
                 Estimate = round(condf[2], 3)),
      data.frame(Feature = "n", Estimate = length(m$residuals))
    ) %>%
    dplyr::mutate(Feature = case_when(
      Feature == "`treated_after(fit)`" ~ "treated",
      Feature == "`I(30 - available_030)(fit)`" ~ "booked_nights",
      .default = Feature
    )) %>%
    dplyr::mutate(iteration = i,
                  seed = seed) %>%
    dbWriteTable(iaan, "MODEL.PRICE_IV_LOOP_RES", ., append = T)
  print(i)
  rm(m, sm, condf)
  gc()
  
}

#IV: Target Real Price----------------------------------------------------------
for(i in 1:100){
  
  if(!(exists("all_listings"))){
    all_listings <- dbGetQuery(iaan, "SELECT DISTINCT KEY_LIST_ID
                                FROM [DATA.ANALYSIS_SAMPLE]")$KEY_LIST_ID
  }
  seed <- sample(1:999999999, 1)
  set.seed(seed)
  samp_listings <- sample(all_listings, round(0.5*length(all_listings)), T)
  
  dbExecute(iaan,
            "CREATE TABLE IF NOT EXISTS [MODEL.REAL_PRICE_IV_LOOP_RES] (
            Feature TEXT,
            Estimate REAL,
            SE REAL,
            t REAL,
            p REAL,
            iteration INT,
            seed REAL
            )")
  
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
    data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
      select(real_price, real_price1, real_price2, real_price3, 
             bathrooms, bedrooms, capacity,
             covid, number_of_reviews, host_is_superhost, listing_type,
             room_type, AC7, AC51, AC72, year, month, KEY_LIST_ID, CITY_ID,
             treated_after, available_030, host_response_time, host_response_rate,
             host_accept_rate, host_has_pic, host_verified, host_about_word_count,
             amenity_word_count, descrip_word_count, loc_overview_word_count) %>%
      na.omit() %>%
      filter(KEY_LIST_ID %in% samp_listings)
  )
  sm <- summary(m)
  condf <- condfstat(m)
  
  sm$coefficients %>% 
    as.data.frame() %>%
    rownames_to_column(var = "Feature") %>%
    dplyr::filter(Feature %in% c("number_of_reviews",
                                 "real_price1", "real_price2", "real_price3", 
                                 "bathrooms", "bedrooms",
                                 "capacity", "covid", "AC7", "AC51", "AC72") |
                    grepl("fit", Feature)) %>%
    dplyr::mutate(n = case_when(
      Feature == "`treated_after(fit)`" ~ 1,
      Feature == "`I(30 - available_030)(fit)`" ~ 2,
      .default = 999
    )) %>%
    dplyr::arrange(n) %>%
    dplyr::select(-n) %>%
    dplyr::rename(SE = `Std. Error`,
                  t = `t value`,
                  p = `Pr(>|t|)`) %>%
    dplyr::mutate(Estimate = round(Estimate, 3),
                  SE = round(SE, 3),
                  t = round(t, 3),
                  p = round(p, 3)) %>%
    as.data.frame() %>%
    plyr::rbind.fill(
      data.frame(Feature = "R Squared", Estimate = round(sm$r.squared, 3)),
      data.frame(Feature = "Adj. R Squared", Estimate = round(sm$adj.r.squared, 3)),
      data.frame(Feature = "Proj. R Squared", Estimate = round(sm$P.r.squared[[1]], 3)),
      data.frame(Feature = "Proj. Adj. R Squared", Estimate = round(sm$P.adj.r.squared[[1]], 3)),
      data.frame(Feature = "F Stat", Estimate = round(sm$fstat[[1]], 3)),
      data.frame(Feature = "Proj. F Stat", Estimate = round(sm$P.fstat[[5]], 3)),
      data.frame(Feature = "Endog. F Stat", Estimate = round(sm$E.fstat[[5]], 3)),
      data.frame(Feature = "treated Cond. F Stat", 
                 Estimate = round(condf[1], 3)),
      data.frame(Feature = "booked_nights Cond. F Stat",
                 Estimate = round(condf[2], 3)),
      data.frame(Feature = "n", Estimate = length(m$residuals))
    ) %>%
    dplyr::mutate(Feature = case_when(
      Feature == "`treated_after(fit)`" ~ "treated",
      Feature == "`I(30 - available_030)(fit)`" ~ "booked_nights",
      .default = Feature
    )) %>%
    dplyr::mutate(iteration = i,
                  seed = seed) %>%
    dbWriteTable(iaan, "MODEL.REAL_PRICE_IV_LOOP_RES", ., append = T)
  print(i)
  rm(m, sm, condf)
  gc()
  
}

#FE Target - Price-----------------------------------------------------------------------
m <- felm(
  price ~ treated_after + I(30 - available_030) +
    price1 + price2 + price3 + bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID,
  data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE")
)
sm <- summary(m)

#Create Formatted Coefficient Table
sm$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  filter(Feature %in% c("treated_after", "number_of_reviews", 
                        "I(30 - available_030)",
                        "price1", "price2", "price3", "bathrooms", "bedrooms",
                        "capacity", "covid", "AC7", "AC51", "AC72")) %>%
  dplyr::rename(SE = `Std. Error`,
                t = `t value`,
                p = `Pr(>|t|)`) %>%
  mutate(Estimate = round(Estimate, 3),
         SE = round(SE, 3),
         t = round(t, 3),
         p = round(p, 3)) %>%
  mutate(sig = ifelse(p < 0.01, "*", "")) %>%
  dplyr::select(Feature, Estimate, SE, sig) %>%
  pivot_longer(cols = c(Estimate, SE)) %>%
  mutate(sig = ifelse(name == "SE", "", sig)) %>%
  mutate(Feature = ifelse(name == "SE", "", Feature)) %>%
  mutate(value = ifelse(name == "SE", 
                        paste0("$(", value, ")$"), 
                        paste0("$", value, "^", sig, "$"))) %>%
  mutate(value = ifelse(name == "Estimate" & sig != "*",
                        gsub("^", "", value),
                        value)
  ) %>%
  dplyr::select(-name, -sig) %>%
  as.data.frame() %>%
  rbind(
    data.frame(Feature = "R Squared", value = round(sm$r.squared, 3)),
    data.frame(Feature = "Adj. R Squared", value = round(sm$adj.r.squared, 3)),
    data.frame(Feature = "Proj. R Squared", value = round(sm$P.r.squared, 3)),
    data.frame(Feature = "Proj. Adj. R Squared", value = round(sm$P.adj.r.squared, 3)),
    data.frame(Feature = "F Stat", value = round(sm$fstat, 3)),
    data.frame(Feature = "Proj. F Stat", value = round(sm$P.fstat[5], 3))
  ) %>%
  mutate(Feature = case_when(
    Feature == "treated_after" ~ "treated",
    Feature == "I(30 - available_030)" ~ "booked_nights",
    .default = Feature
  )) %>%
  dplyr::rename(OLS = value) %>%
  dbWriteTable(iaan, "MODEL.PRICE_FE_FORMATTED", ., overwrite = T)

#Save Coefficients
sm$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  select(Feature, Estimate) %>%
  dbWriteTable(iaan, "MODEL.PRICE_IVFE_COEFS", ., overwrite = T)

#Remove Model Objects
rm(m, sm)
gc()

#FE Target - Real Price------------------------------------------------------------------
m <- felm(
  real_price ~ treated_after + I(30 - available_030) +
    real_price1 + real_price2 + real_price3 + bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID,
  data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE")
)
sm <- summary(m)

#Create Formatted Coefficient Table
sm$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  filter(Feature %in% c("treated_after", "number_of_reviews", 
                        "I(30 - available_030)",
                        "real_price1", "real_price2", "real_price3", 
                        "bathrooms", "bedrooms",
                        "capacity", "covid", "AC7", "AC51", "AC72")) %>%
  dplyr::rename(SE = `Std. Error`,
                t = `t value`,
                p = `Pr(>|t|)`) %>%
  mutate(Estimate = round(Estimate, 3),
         SE = round(SE, 3),
         t = round(t, 3),
         p = round(p, 3)) %>%
  mutate(sig = ifelse(p < 0.01, "*", "")) %>%
  dplyr::select(Feature, Estimate, SE, sig) %>%
  pivot_longer(cols = c(Estimate, SE)) %>%
  mutate(sig = ifelse(name == "SE", "", sig)) %>%
  mutate(Feature = ifelse(name == "SE", "", Feature)) %>%
  mutate(value = ifelse(name == "SE", 
                        paste0("$(", value, ")$"), 
                        paste0("$", value, "^", sig, "$"))) %>%
  mutate(value = ifelse(name == "Estimate" & sig != "*",
                        gsub("^", "", value),
                        value)
  ) %>%
  dplyr::select(-name, -sig) %>%
  as.data.frame() %>%
  rbind(
    data.frame(Feature = "R Squared", value = round(sm$r.squared, 3)),
    data.frame(Feature = "Adj. R Squared", value = round(sm$adj.r.squared, 3)),
    data.frame(Feature = "Proj. R Squared", value = round(sm$P.r.squared, 3)),
    data.frame(Feature = "Proj. Adj. R Squared", value = round(sm$P.adj.r.squared, 3)),
    data.frame(Feature = "F Stat", value = round(sm$fstat, 3)),
    data.frame(Feature = "Proj. F Stat", value = round(sm$P.fstat[5], 3))
  ) %>%
  mutate(Feature = case_when(
    Feature == "treated_after" ~ "treated",
    Feature == "I(30 - available_030)" ~ "booked_nights",
    .default = Feature
  )) %>%
  dplyr::rename(FE = value) %>%
  dbWriteTable(iaan, "MODEL.REAL_PRICE_FE_FORMATTED", ., overwrite = T)

#Save Coefficients
sm$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  select(Feature, Estimate) %>%
  dbWriteTable(iaan, "MODEL.REAL_PRICE_FE_COEFS", ., overwrite = T)

#Remove Model Objects
rm(m, sm)
gc()

#FE IV Price--------------------------------------------------------------------
all_listings <- dbGetQuery(iaan,
                           "SELECT DISTINCT KEY_LIST_ID FROM 
                           [DATA.ANALYSIS_SAMPLE]")$KEY_LIST_ID

for(i in 1:100){
  
  seed <- sample(1:999999999, 1)
  set.seed(seed)
  samp_listings <- sample(all_listings, round(0.5*length(all_listings)), T)
  
  dbExecute(iaan,
            "CREATE TABLE IF NOT EXISTS [MODEL.PRICE_IV_LOOP_RES] (
            Feature TEXT,
            Estimate REAL,
            SE REAL,
            t REAL,
            p REAL,
            iteration INT,
            seed REAL
            )")
  
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
    data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
      select(price, price1, price2, price3, bathrooms, bedrooms, capacity,
             covid, number_of_reviews, host_is_superhost, listing_type,
             room_type, AC7, AC51, AC72, year, month, KEY_LIST_ID,
             treated_after, available_030, host_response_time, host_response_rate,
             host_accept_rate, host_has_pic, host_verified, host_about_word_count,
             amenity_word_count, descrip_word_count, loc_overview_word_count) %>%
      na.omit() %>%
      filter(KEY_LIST_ID %in% samp_listings)
  )
  sm <- summary(m)
  condf <- condfstat(m)
  
  sm$coefficients %>% 
    as.data.frame() %>%
    rownames_to_column(var = "Feature") %>%
    dplyr::filter(Feature %in% c("number_of_reviews",
                          "price1", "price2", "price3", "bathrooms", "bedrooms",
                          "capacity", "covid", "AC7", "AC51", "AC72") |
             grepl("fit", Feature)) %>%
    dplyr::mutate(n = case_when(
      Feature == "`treated_after(fit)`" ~ 1,
      Feature == "`I(30 - available_030)(fit)`" ~ 2,
      .default = 999
    )) %>%
    dplyr::arrange(n) %>%
    dplyr::select(-n) %>%
    dplyr::rename(SE = `Std. Error`,
                  t = `t value`,
                  p = `Pr(>|t|)`) %>%
    dplyr::mutate(Estimate = round(Estimate, 3),
           SE = round(SE, 3),
           t = round(t, 3),
           p = round(p, 3)) %>%
    as.data.frame() %>%
    rbind.fill(
      data.frame(Feature = "R Squared", Estimate = round(sm$r.squared, 3)),
      data.frame(Feature = "Adj. R Squared", Estimate = round(sm$adj.r.squared, 3)),
      data.frame(Feature = "Proj. R Squared", Estimate = round(sm$P.r.squared[[1]], 3)),
      data.frame(Feature = "Proj. Adj. R Squared", Estimate = round(sm$P.adj.r.squared[[1]], 3)),
      data.frame(Feature = "F Stat", Estimate = round(sm$fstat[[1]], 3)),
      data.frame(Feature = "Proj. F Stat", Estimate = round(sm$P.fstat[[5]], 3)),
      data.frame(Feature = "Endog. F Stat", Estimate = round(sm$E.fstat[[5]], 3)),
      data.frame(Feature = "treated Cond. F Stat", 
                 Estimate = round(condf[1], 3)),
      data.frame(Feature = "booked_nights Cond. F Stat",
                 Estimate = round(condf[2], 3)),
      data.frame(Feature = "n", Estimate = length(m$residuals))
    ) %>%
    dplyr::mutate(Feature = case_when(
      Feature == "`treated_after(fit)`" ~ "treated",
      Feature == "`I(30 - available_030)(fit)`" ~ "booked_nights",
      .default = Feature
    )) %>%
    dplyr::mutate(iteration = i,
                  seed = seed) %>%
    dbWriteTable(iaan, "MODEL.PRICE_FEIV_LOOP_RES", ., append = T)
  print(i)
  rm(m, sm, condf)
  gc()

}


#FE IV: Target Real Price--------------------------------------------------------
for(i in 1:100){
  
  seed <- sample(1:999999999, 1)
  set.seed(seed)
  samp_listings <- sample(all_listings, round(0.5*length(all_listings)), T)
  
  dbExecute(iaan,
            "CREATE TABLE IF NOT EXISTS [MODEL.REAL_PRICE_IV_LOOP_RES] (
            Feature TEXT,
            Estimate REAL,
            SE REAL,
            t REAL,
            p REAL,
            iteration INT,
            seed REAL
            )")
  
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
    data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
      select(real_price, real_price1, real_price2, real_price3,
             bathrooms, bedrooms, capacity,
             covid, number_of_reviews, host_is_superhost, listing_type,
             room_type, AC7, AC51, AC72, year, month, KEY_LIST_ID,
             treated_after, available_030, host_response_time, host_response_rate,
             host_accept_rate, host_has_pic, host_verified, host_about_word_count,
             amenity_word_count, descrip_word_count, loc_overview_word_count) %>%
      na.omit() %>%
      filter(KEY_LIST_ID %in% samp_listings)
  )
  sm <- summary(m)
  condf <- condfstat(m)
  
  sm$coefficients %>% 
    as.data.frame() %>%
    rownames_to_column(var = "Feature") %>%
    dplyr::filter(Feature %in% c("number_of_reviews",
                                 "real_price1", "real_price2", "real_price3", 
                                 "bathrooms", "bedrooms",
                                 "capacity", "covid", "AC7", "AC51", "AC72") |
                    grepl("fit", Feature)) %>%
    dplyr::mutate(n = case_when(
      Feature == "`treated_after(fit)`" ~ 1,
      Feature == "`I(30 - available_030)(fit)`" ~ 2,
      .default = 999
    )) %>%
    dplyr::arrange(n) %>%
    dplyr::select(-n) %>%
    dplyr::rename(SE = `Std. Error`,
                  t = `t value`,
                  p = `Pr(>|t|)`) %>%
    dplyr::mutate(Estimate = round(Estimate, 3),
                  SE = round(SE, 3),
                  t = round(t, 3),
                  p = round(p, 3)) %>%
    as.data.frame() %>%
    plyr::rbind.fill(
      data.frame(Feature = "R Squared", Estimate = round(sm$r.squared, 3)),
      data.frame(Feature = "Adj. R Squared", Estimate = round(sm$adj.r.squared, 3)),
      data.frame(Feature = "Proj. R Squared", Estimate = round(sm$P.r.squared[[1]], 3)),
      data.frame(Feature = "Proj. Adj. R Squared", Estimate = round(sm$P.adj.r.squared[[1]], 3)),
      data.frame(Feature = "F Stat", Estimate = round(sm$fstat[[1]], 3)),
      data.frame(Feature = "Proj. F Stat", Estimate = round(sm$P.fstat[[5]], 3)),
      data.frame(Feature = "Endog. F Stat", Estimate = round(sm$E.fstat[[5]], 3)),
      data.frame(Feature = "treated Cond. F Stat", 
                 Estimate = round(condf[1], 3)),
      data.frame(Feature = "booked_nights Cond. F Stat",
                 Estimate = round(condf[2], 3)),
      data.frame(Feature = "n", Estimate = length(m$residuals))
    ) %>%
    dplyr::mutate(Feature = case_when(
      Feature == "`treated_after(fit)`" ~ "treated",
      Feature == "`I(30 - available_030)(fit)`" ~ "booked_nights",
      .default = Feature
    )) %>%
    dplyr::mutate(iteration = i,
                  seed = seed) %>%
    dbWriteTable(iaan, "MODEL.REAL_PRICE_FEIV_LOOP_RES", ., append = T)
  print(i)
  rm(m, sm, condf)
  gc()
  
}


