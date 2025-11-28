#Loop Parameter Setup-----------------------------------------------------------
if(!(exists("all_listings"))){
  all_listings <- dbGetQuery(iaan, "SELECT DISTINCT KEY_LIST_ID
                                FROM [DATA.ANALYSIS_SAMPLE]")$KEY_LIST_ID
}

main_eq <- "real_price ~ real_price1 + real_price2 + real_price3 + 
  bathrooms + bedrooms + capacity + covid +
  number_of_reviews + host_is_superhost +
  as.factor(listing_type) + as.factor(room_type) + 
  AC7 + AC51 + AC72 + as.factor(year) + as.factor(month) | KEY_LIST_ID | 
 (treated_after | I(30 - available_030) ~ "

iv_cands <- c("as.factor(host_response_time)", "host_response_rate",
              "host_accept_rate", "host_has_pic", "host_verified",
              "host_about_word_count", "amenity_word_count",
              "descrip_word_count", "loc_overview_word_count")
n_cand <- length(iv_cands)

#All Hosts----------------------------------------------------------------------
for(i in 1:100){

seed <- i
set.seed(i)
n_ivs <- sample(1:n_cand, 1)
ivs <- paste0(sample(iv_cands, n_ivs, F), collapse = " + ")
model_i <- as.formula(paste0(main_eq, ivs, ")"))

samp_listings <- sample(all_listings, round(0.3*length(all_listings)), T)
m <- felm(
  model_i, data = dbGetQuery(iaan,
                             paste0("SELECT * FROM [DATA.ANALYSIS_SAMPLE]
                             WHERE KEY_LIST_ID IN ( ",
                                    paste0(samp_listings, collapse = ", "),
                                    ")")
                             ) %>%
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

res <- tail(summary(m)$coefficients, 2) %>%
  as.data.frame() %>%
  rownames_to_column("coef") %>%
  mutate(coef = ifelse(coef == "`treated_after(fit)`", "T", "Q")) %>%
  dplyr::rename(
    se = `Std. Error`
  ) %>%
  select(coef, Estimate, se) %>%
  mutate(iter = i) %>%
  mutate(resp_time = grepl("host_response_time", ivs) %>% as.numeric(),
         resp_rate = grepl("host_response_rate", ivs) %>% as.numeric(),
         accept_rate = grepl("host_accept_rate", ivs) %>% as.numeric(),
         has_pic = grepl("host_has_pic", ivs) %>% as.numeric(),
         verified = grepl("host_verified", ivs) %>% as.numeric(),
         host_about = grepl("host_about_word_count", ivs) %>% as.numeric(),
         amenity = grepl("amenity_word_count", ivs) %>% as.numeric(),
         descrip = grepl("descrip_word_count", ivs) %>% as.numeric(),
         loc = grepl("loc_overview_word_count", ivs) %>% as.numeric()
  )

if(i == 1){
  dbWriteTable(iaan, "MODEL.IV_ROBUSTNESS_LOOP_RESULTS", res)
} else {
  dbWriteTable(iaan, "MODEL.IV_ROBUSTNESS_LOOP_RESULTS", res, append = T)
}
rm(m, res, samp_listings, n_ivs, ivs, model_i)
gc()
print(i)

}

#Superhost Only-----------------------------------------------------------------
for(i in 1:100){

seed <- i
set.seed(i)
n_ivs <- sample(1:n_cand, 1)
ivs <- paste0(sample(iv_cands, n_ivs, F), collapse = " + ")
model_i <- as.formula(paste0(main_eq, ivs, ")"))

samp_listings <- sample(all_listings, round(0.3*length(all_listings)), T)
m <- felm(
  model_i, data = dbGetQuery(iaan,
                             paste0("SELECT * FROM [DATA.ANALYSIS_SAMPLE]
                             WHERE KEY_LIST_ID IN ( ",
                                    paste0(samp_listings, collapse = ", "),
                                    ") & host_is_superhost = 1")
                             ) %>%
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

res <- tail(summary(m)$coefficients, 2) %>%
  as.data.frame() %>%
  rownames_to_column("coef") %>%
  mutate(coef = ifelse(coef == "`treated_after(fit)`", "T", "Q")) %>%
  dplyr::rename(
    se = `Std. Error`
  ) %>%
  select(coef, Estimate, se) %>%
  mutate(iter = i) %>%
  mutate(resp_time = grepl("host_response_time", ivs) %>% as.numeric(),
         resp_rate = grepl("host_response_rate", ivs) %>% as.numeric(),
         accept_rate = grepl("host_accept_rate", ivs) %>% as.numeric(),
         has_pic = grepl("host_has_pic", ivs) %>% as.numeric(),
         verified = grepl("host_verified", ivs) %>% as.numeric(),
         host_about = grepl("host_about_word_count", ivs) %>% as.numeric(),
         amenity = grepl("amenity_word_count", ivs) %>% as.numeric(),
         descrip = grepl("descrip_word_count", ivs) %>% as.numeric(),
         loc = grepl("loc_overview_word_count", ivs) %>% as.numeric()
  )

if(i == 1){
  dbWriteTable(iaan, "MODEL.IV_ROBUSTNESS_LOOP_RESULTS_SHONLY", res)
} else {
  dbWriteTable(iaan, "MODEL.IV_ROBUSTNESS_LOOP_RESULTS_SHONLY", res, append = T)
}
rm(m, res, samp_listings, n_ivs, ivs, model_i)
gc()
print(i)

}

rm(all_listings, i, iv_cands, main_eq, n_cand, seed)








