# Unit Root Test----------------------------------------------------------------
df <- dbGetQuery(iaan, "SELECT 
                          KEY_LIST_ID, 
                          KEY_HOST_ID, 
                          year, 
                          month, 
                          price, 
                          real_price,
                          price1, real_price1
                        FROM [DATA.ANALYSIS_SAMPLE]
                       "
) %>%
  mutate(year_month = as.yearmon(paste(year, month), "%Y %m")) %>%
  distinct()

df <- df %>%
  group_by(KEY_LIST_ID, year_month) %>%
  dplyr::mutate(n = n()) %>%
  group_by(KEY_LIST_ID) %>%
  dplyr::mutate(nmax = max(n)) %>%
  filter(nmax==1)

df <- df %>%
  group_by(KEY_LIST_ID) %>%
  dplyr::mutate(n = n())

df <- df %>%
  filter(n >= 24)

pdf <- pdata.frame(df, c("KEY_LIST_ID", "year_month"))
ur_test = purtest(real_price~1, 
        pdf, 
        pmax=3, test="ips", 
        index=c("KEY_LIST_ID", "year_month"))

data.frame(
  test = "ips",
  stat = ur_test$statistic$statistic[[1]],
  pval = ur_test$statistic$p.value[[1]]
) %>%
  dbWriteTable(iaan, "MODEL.UNIT_ROOT_TEST", .)

rm(df, pdf, ur_test)
gc()

# Remove Lags Real--------------------------------------------------------------

ids <- dbGetQuery(iaan, 
                  "SELECT DISTINCT KEY_LIST_ID FROM [DATA.ANALYSIS_SAMPLE]"
) %>%
  pull(KEY_LIST_ID)


id_sample <-  sample(x = ids, size = 0.5*length(ids), replace = F)

# OLS
m <- lm(
  real_price ~ treated_after + I(30 - available_030) +  bathrooms + bedrooms + 
    capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
    as.factor(CITY_ID),
  data = dbGetQuery(iaan,
                    "SELECT KEY_LIST_ID, real_price, treated_after, available_030,
                    bathrooms, bedrooms, capacity, covid, number_of_reviews,
                    listing_type, room_type, AC7, AC51, AC72, year, month,
                    CITY_ID
                    FROM [DATA.ANALYSIS_SAMPLE]") %>%
    filter(KEY_LIST_ID %in% id_sample)
)

m <- summary(m)
m <- m$coefficients
m <- m %>% as.data.frame()
m <- m %>% rownames_to_column("feature")
res <- data.frame(
  treat_coef = (m %>% filter(grepl("treated", feature)))$Estimate,
  treat_pval = (m %>% filter(grepl("treated", feature)))$`Std. Error`,
  q_coef = (m %>% filter(grepl("available", feature)))$Estimate,
  q_std = (m %>% filter(grepl("available", feature)))$`Std. Error`,
  method = "OLS"
)

res %>%
  dbWriteTable(iaan, "MODEL.NO_LAG_RESULTS", .)

rm(m, res)
gc()

# IV
m <- felm(
  real_price ~bathrooms + bedrooms + capacity + covid +
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
    filter(KEY_LIST_ID %in% id_sample)
)

m <- summary(m)
m <- m$coefficients
m <- m %>% as.data.frame()
m <- m %>% rownames_to_column("feature")
res <- data.frame(
  treat_coef = (m %>% filter(grepl("treated", feature)))$Estimate,
  treat_pval = (m %>% filter(grepl("treated", feature)))$`Std. Error`,
  q_coef = (m %>% filter(grepl("available", feature)))$Estimate,
  q_std = (m %>% filter(grepl("available", feature)))$`Std. Error`,
  method = "IV"
)

res %>%
  dbWriteTable(iaan, "MODEL.NO_LAG_RESULTS", ., append = T)
rm(res, m)
gc()

# FE
m <- felm(
  real_price ~ treated_after + I(30 - available_030) + bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID,
  data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
    filter(KEY_LIST_ID %in% id_sample)
)
m <- summary(m)
m <- m$coefficients
m <- m %>% as.data.frame()
m <- m %>% rownames_to_column("feature")

res <- data.frame(
  treat_coef = (m %>% filter(grepl("treated", feature)))$Estimate,
  treat_pval = (m %>% filter(grepl("treated", feature)))$`Std. Error`,
  q_coef = (m %>% filter(grepl("available", feature)))$Estimate,
  q_std = (m %>% filter(grepl("available", feature)))$`Std. Error`,
  method = "FE"
)

res %>%
  dbWriteTable(iaan, "MODEL.NO_LAG_RESULTS", ., append = T)
rm(res, m)
gc()

# FEIV
m <- felm(
 real_price ~ bathrooms + bedrooms + capacity + covid +
    number_of_reviews + host_is_superhost +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID |
    (treated_after | I(30 - available_030) ~  + as.factor(host_response_time)
     + host_response_rate + 
       host_accept_rate +
       host_has_pic + host_verified + host_about_word_count +
       amenity_word_count + descrip_word_count + loc_overview_word_count),
  data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
    select(real_price, bathrooms, bedrooms, capacity,
           covid, number_of_reviews, host_is_superhost, listing_type,
           room_type, AC7, AC51, AC72, year, month, KEY_LIST_ID,
           treated_after, available_030, host_response_time, host_response_rate,
           host_accept_rate, host_has_pic, host_verified, host_about_word_count,
           amenity_word_count, descrip_word_count, loc_overview_word_count) %>%
    na.omit() %>%
    filter(KEY_LIST_ID %in% id_sample)
)

m <- summary(m)
m <- m$coefficients
m <- m %>% as.data.frame()
m <- m %>% rownames_to_column("feature")

res <- data.frame(
  treat_coef = (m %>% filter(grepl("treated", feature)))$Estimate,
  treat_pval = (m %>% filter(grepl("treated", feature)))$`Std. Error`,
  q_coef = (m %>% filter(grepl("available", feature)))$Estimate,
  q_std = (m %>% filter(grepl("available", feature)))$`Std. Error`,
  method = "FEIV"
)

res %>%
  dbWriteTable(iaan, "MODEL.NO_LAG_RESULTS", ., append = T)
rm(res, m)
gc()

# Remove Lags Nom---------------------------------------------------------------

# OLS
m <- lm(
  price ~ treated_after + I(30 - available_030) +  bathrooms + bedrooms + 
    capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) +
    as.factor(CITY_ID),
  data = dbGetQuery(iaan,
                    "SELECT KEY_LIST_ID, price, treated_after, available_030,
                    bathrooms, bedrooms, capacity, covid, number_of_reviews,
                    listing_type, room_type, AC7, AC51, AC72, year, month,
                    CITY_ID
                    FROM [DATA.ANALYSIS_SAMPLE]") %>%
    filter(KEY_LIST_ID %in% id_sample)
)

m <- summary(m)
m <- m$coefficients
m <- m %>% as.data.frame()
m <- m %>% rownames_to_column("feature")
res <- data.frame(
  treat_coef = (m %>% filter(grepl("treated", feature)))$Estimate,
  treat_pval = (m %>% filter(grepl("treated", feature)))$`Std. Error`,
  q_coef = (m %>% filter(grepl("available", feature)))$Estimate,
  q_std = (m %>% filter(grepl("available", feature)))$`Std. Error`,
  method = "OLS"
)

res %>%
  dbWriteTable(iaan, "MODEL.NO_LAG_RESULTS_NOMINAL", .)

rm(m, res)
gc()

# IV
m <- felm(
  price ~bathrooms + bedrooms + capacity + covid +
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
    select(price, 
           bathrooms, bedrooms, capacity,
           covid, number_of_reviews, host_is_superhost, listing_type,
           room_type, AC7, AC51, AC72, year, month, KEY_LIST_ID, CITY_ID,
           treated_after, available_030, host_response_time, host_response_rate,
           host_accept_rate, host_has_pic, host_verified, host_about_word_count,
           amenity_word_count, descrip_word_count, loc_overview_word_count) %>%
    na.omit() %>%
    filter(KEY_LIST_ID %in% id_sample)
)

m <- summary(m)
m <- m$coefficients
m <- m %>% as.data.frame()
m <- m %>% rownames_to_column("feature")
res <- data.frame(
  treat_coef = (m %>% filter(grepl("treated", feature)))$Estimate,
  treat_pval = (m %>% filter(grepl("treated", feature)))$`Std. Error`,
  q_coef = (m %>% filter(grepl("available", feature)))$Estimate,
  q_std = (m %>% filter(grepl("available", feature)))$`Std. Error`,
  method = "IV"
)

res %>%
  dbWriteTable(iaan, "MODEL.NO_LAG_RESULTS_NOMINAL", ., append = T)
rm(res, m)
gc()

# FE
m <- felm(
  price ~ treated_after + I(30 - available_030) + bathrooms + bedrooms + capacity + covid +
    number_of_reviews +
    as.factor(listing_type) + as.factor(room_type) + 
    AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID,
  data = dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
    filter(KEY_LIST_ID %in% id_sample)
)
m <- summary(m)
m <- m$coefficients
m <- m %>% as.data.frame()
m <- m %>% rownames_to_column("feature")

res <- data.frame(
  treat_coef = (m %>% filter(grepl("treated", feature)))$Estimate,
  treat_pval = (m %>% filter(grepl("treated", feature)))$`Std. Error`,
  q_coef = (m %>% filter(grepl("available", feature)))$Estimate,
  q_std = (m %>% filter(grepl("available", feature)))$`Std. Error`,
  method = "FE"
)

res %>%
  dbWriteTable(iaan, "MODEL.NO_LAG_RESULTS_NOMINAL", ., append = T)
rm(res, m)
gc()

# FEIV
m <- felm(
  price ~ bathrooms + bedrooms + capacity + covid +
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
    filter(KEY_LIST_ID %in% id_sample)
)

m <- summary(m)
m <- m$coefficients
m <- m %>% as.data.frame()
m <- m %>% rownames_to_column("feature")

res <- data.frame(
  treat_coef = (m %>% filter(grepl("treated", feature)))$Estimate,
  treat_pval = (m %>% filter(grepl("treated", feature)))$`Std. Error`,
  q_coef = (m %>% filter(grepl("available", feature)))$Estimate,
  q_std = (m %>% filter(grepl("available", feature)))$`Std. Error`,
  method = "FEIV"
)

res %>%
  dbWriteTable(iaan, "MODEL.NO_LAG_RESULTS", ., append = T)
rm(res, m)
gc()

# Fix colnames
real <- dbReadTable(iaan, "MODEL.NO_LAG_RESULTS")
colnames(real) <- c("treat_coef", "treat_std", "q_coef", "q_std", "method")
real %>%
  dbWriteTable(iaan, "MODEL.NO_LAG_RESULTS", ., overwrite = T)

nom <- dbReadTable(iaan, "MODEL.NO_LAG_RESULTS_NOMINAL")
colnames(nom) <- c("treat_coef", "treat_std", "q_coef", "q_std", "method")
nom %>%
  dbWriteTable(iaan, "MODEL.NO_LAG_RESULTS", ., overwrite = T)

rm(real, nom)

# Get Sargen--------------------------------------------------------------------
df <- dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
  select(KEY_LIST_ID, real_price, real_price1, real_price2, real_price3, 
         bathrooms, bedrooms, capacity,
         covid, number_of_reviews, host_is_superhost, listing_type,
         room_type, AC7, AC51, AC72, year, month, KEY_LIST_ID, CITY_ID,
         treated_after, available_030, host_response_time, host_response_rate,
         host_accept_rate, host_has_pic, host_verified, host_about_word_count,
         amenity_word_count, descrip_word_count, loc_overview_word_count) %>%
  na.omit() %>%
  filter(KEY_LIST_ID %in% id_sample) %>%
  na.omit()

id_sample_2 <- sample(id_sample, floor(.25*length(id_sample)))
m <- AER::ivreg(real_price ~ treated_after + I(30 - available_030) + 
             capacity + covid + number_of_reviews + host_is_superhost +
             as.factor(room_type) + AC7 + AC51 + AC72 +
             as.factor(year) + as.factor(month) + as.factor(listing_type)
             | capacity + covid + number_of_reviews + host_is_superhost +
             as.factor(room_type) + AC7 + AC51 + AC72 +
             as.factor(year) + as.factor(month) + as.factor(listing_type) +
             host_response_rate + 
             host_accept_rate +
             host_has_pic + host_verified + host_about_word_count +
             amenity_word_count + descrip_word_count + loc_overview_word_count,
             data = df %>% filter(KEY_LIST_ID %in% id_sample_2))
m <- summary(m, diagnostics = TRUE)


