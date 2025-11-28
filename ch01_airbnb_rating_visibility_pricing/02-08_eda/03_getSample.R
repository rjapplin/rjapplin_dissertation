if(!(exists("iadb")){
  stop("Need to connect to iadb to run this script.")
} else {

#Characteristics for Sample Selection-------------------------------------------

#Get certain minimums and maximum to use to determine whether a listing stays
#in sample
ia <- dbGetQuery(iadb,
           "SELECT
              KEY_LIST_ID, KEY_HOST_ID,
              MIN(CAST(DATE AS NUMERIC)) AS list_first_obs,
              MAX(CAST(DATE AS NUMERIC)) AS list_last_obs,
              MIN(CAST(price AS REAL)) AS min_price,
              MAX(CAST(price AS REAL)) AS max_price,
              MIN(overall_rating) AS min_rating,
              MIN(first_review) AS min_first_review
            FROM 
              (SELECT KEY_LIST_ID, KEY_HOST_ID, DATE, CAST(price AS REAL) AS price, overall_rating, 
                CASE
                  WHEN first_review_date IS NULL THEN '9999-12-31'
                  WHEN first_review_date = '' THEN '9999-12-31'
                  ELSE first_review_date
                END AS first_review FROM [CURATED.TAB_LISTING_ANALYTICS])
            GROUP BY KEY_LIST_ID, KEY_HOST_ID")

#Determine Listings to Select for inclusion-------------------------------------
ia <- ia %>%
  filter(min_price >= 47) %>%
  filter(max_price <= 315) %>%
  filter(min_rating >= 4 | is.na(min_rating)) %>%
  mutate(list_first_obs = as.Date(
    paste0(substr(list_first_obs, 1, 4), "-",
           substr(list_first_obs, 5, 6), "-",
           substr(list_first_obs, 7, 8))
  )) %>% 
  mutate(min_first_review = as.Date(min_first_review)) %>%
  mutate(see_treat_or_never = (min_first_review >= list_first_obs | 
                                 min_first_review == "9999-12-31")) %>%
  filter(!(min_first_review == "9999-12-31" & !(is.na(min_rating)))) %>%
  group_by(KEY_LIST_ID) %>%
  mutate(n = n()) %>%
  mutate(max_n = max(n)) %>%
  filter(max_n == 1) %>%
  select(KEY_LIST_ID, KEY_HOST_ID, list_first_obs, min_first_review, 
         see_treat_or_never)

listing_ids <- paste0('(', 
                      paste0("'", ia$KEY_LIST_ID %>% unique(), "'", 
                             collapse = ', '),
                      ')')

#Select the Sample--------------------------------------------------------------
ia_sample <- dbGetQuery(iadb,
           paste0(
             "SELECT * FROM [CURATED.TAB_LISTING_ANALYTICS]
                WHERE KEY_LIST_ID IN ", listing_ids
           )) %>%
  inner_join(ia, by = c("KEY_LIST_ID", "KEY_HOST_ID")) 

#Identify Treatment Group-------------------------------------------------------
ia_sample %>%
  mutate(treatment_group = case_when(
    see_treat_or_never == TRUE &
      min_first_review < as.Date("9999-12-31") ~ "Treated",
    see_treat_or_never == TRUE & 
      min_first_review == "9999-12-31" ~ "Never Treated",
    see_treat_or_never == FALSE & 
      min_first_review < as.Date("9999-12-31") ~ "Always Treated",
    see_treat_or_never == FALSE & 
      min_first_review == "9999-12-31" ~ "Never Treated"
  )) %>%
  mutate(year = substr(DATE, 1, 4)) %>%
  mutate(month = substr(DATE, 5, 6)) %>%
  select(-see_treat_or_never) %>%
  mutate(treated = 
           ifelse(treatment_group == "Never Treated", 0, 1)) -> ia_sample

#Handle City ID changing--------------------------------------------------------
ia_sample %>%
  group_by(KEY_LIST_ID) %>%
  arrange(DATE) %>%
  mutate(first = dplyr::first(CITY_ID)) %>%
  mutate(multiple_city = (CITY_ID != first)) %>%
  mutate(multiple_city = max(multiple_city)) %>%
  filter(multiple_city == 0) -> ia_sample

#Filter to Sample Cities--------------------------------------------------------
ia_sample %>%
  filter(
    CITY_ID %in% c(
      "11", "12", "15", "23", "24", "25", "26", "28", "30", "33", "34", "38", 
      "40"
    )
  ) -> ia_sample

#Get Rid of Instances Where No Price--------------------------------------------
ia_sample %>%
  group_by(KEY_LIST_ID) %>%
  mutate(has_na_prices = max(is.na(price))) %>%
  filter(has_na_prices == 0) -> ia_sample

#Cleanup
ia_sample %>%
  select(-first, -multiple_city, -has_na_prices) -> ia_sample

#Write Full Sample File to IAAN-------------------------------------------------
dbWriteTable(iadb,
             "DATA.FULL_SAMPLE",
             ia_sample)

#Remove Unnecessary Objects-----------------------------------------------------
rm(ia, listing_ids)

}