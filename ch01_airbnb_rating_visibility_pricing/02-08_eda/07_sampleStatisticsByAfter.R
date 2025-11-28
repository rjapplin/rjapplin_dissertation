#Treated Sample-----------------------------------------------------------------
if(
  !("TABLE.SUM_STATS_BY_AFTER" %in% dbListTables(iaan)) |
  !("TABLE.SUM_STATS_LIST_TYPE_BY_AFTER" %in% dbListTables(iaan)) |
  !("TABLE.SUM_STATS_ROOM_TYPE_BY_AFTER" %in% dbListTables(iaan))
) {
  
  treated <- dbGetQuery(iaan,
                        "SELECT * FROM [DATA.FULL_SAMPLE]
                        WHERE treatment_group = 'Treated'") %>%
    left_join(cpi, by = c("year", "month")) %>%
    mutate(real_price = (price/cpi_b2015)*100) %>%
    select(-cpi, -cpi_b2015)
  
  treated <- treated %>%
    group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
    mutate(first_review_date = min(as.Date(first_review_date), na.rm = T)) %>%
    mutate(date = as.Date(paste0(year, "-", month, "-", substr(DATE, 7, 8)))) %>%
    mutate(after = ifelse(date >= first_review_date, 1, 0)) %>%
    mutate(flag = ifelse(after == 0 & !(is.na(overall_rating)), 1, 0))
  
  treated <- treated %>%
    group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
    mutate(ever_flag = max(flag))
  
  treated <- treated %>%
    filter(ever_flag == 0)
  
} else {
  
}

#Numeric------------------------------------------------------------------------
if(!("TABLE.SUM_STATS_BY_AFTER" %in% dbListTables(iaan))){
  
  treated_num <- treated %>%
    ungroup() %>%
    select(after, price, real_price, capacity, bathrooms, bedrooms,
           min_night, max_night, available, available_030, available_090,
           available_365, number_of_reviews, 
           overall_rating, accom_rating, clean_rating, checkin_rating,
           com_rating, loc_rating, value_rating, amenity_word_count,
           amenity_count, amenity_char_count, loc_overview_word_count,
           descrip_word_count, loc_overview_char_count, descrip_char_count,
           host_response_rate, host_accept_rate, host_is_superhost,
           host_listing_count, host_has_pic, host_verified,
           host_about_word_count, host_about_char_count,
           starts_with("AC"))
  
  groupSumStats <- describeBy(treated_num, treated_num$after)
  
  before <- groupSumStats$`0`
  beforeTable <- before %>%
    as.data.frame() %>%
    rownames_to_column(var = "Variable") %>%
    filter(Variable != "after") %>%
    select(Variable, mean, sd, se, n) %>%
    dplyr::rename(
      before_mean = mean,
      before_sd = sd,
      before_se = se,
      before_n = n
    )
  
  after <- groupSumStats$`1`
  afterTable <- after %>%
    as.data.frame() %>%
    rownames_to_column(var = "Variable") %>%
    filter(Variable != "after") %>%
    select(Variable, mean, sd, se, n) %>%
    dplyr::rename(
      after_mean = mean,
      after_sd = sd,
      after_se = se,
      after_n = n
    )
  
  groupSampTable <- beforeTable %>%
    inner_join(afterTable, by = "Variable") 
  
  groupSampTable %>%
    mutate(after_before = after_mean - before_mean) %>%
    mutate(t_after_before = (after_before)/(sqrt(after_se + before_se))) %>%
    mutate(
      v_after_before = computeV(after_sd, before_sd, after_n, before_n),
    ) %>%
    mutate(p_after_before = 2*pt(abs(t_after_before), 
                                 df = v_after_before,
                                 lower.tail = F)
    ) %>%
    select(Variable,
           before_mean,
           after_mean,
           p_after_before) %>%
    mutate(
      before_mean = round(before_mean, 3),
      after_mean = round(after_mean, 3),
      p_after_before = round(p_after_before, 3)
    ) %>%
    left_join(
      read.csv("amenity_map.csv") %>% select(HUMAN_LABEL, AMENITY_CLEAN_CODE) %>%
        dplyr::rename(Variable = AMENITY_CLEAN_CODE),
      by = "Variable"
    ) %>%
    mutate(
      Variable = ifelse(is.na(HUMAN_LABEL), Variable, HUMAN_LABEL)
    ) %>%
    select(-HUMAN_LABEL) %>%
    dplyr::rename(
      Before = before_mean,
      After = after_mean,
      `After - Before p` = p_after_before
    ) -> groupSampTable2
  
  dbWriteTable(
    iaan, 
    "TABLE.SUM_STATS_BY_AFTER",
    groupSampTable2
  )
  
  rm(after, afterTable, before, beforeTable, groupSampTable, groupSampTable2,
     groupSumStats, treated_num)
  
} else {
  
}

#Factors------------------------------------------------------------------------
if(
  !("TABLE.SUM_STATS_LIST_TYPE_BY_AFTER" %in% dbListTables(iaan)) |
  !("TABLE.SUM_STATS_ROOM_TYPE_BY_AFTER" %in% dbListTables(iaan))
){

treated %>%
  ungroup() %>%
  select(after, listing_type, room_type) %>%
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
  ) -> facts
  
}

if(
  !("TABLE.SUM_STATS_LIST_TYPE_BY_AFTER" %in% dbListTables(iaan))
){

  listingTypeTable <- (table(facts$listing_type, facts$after)) %>%
    as.data.frame() %>% 
    pivot_wider(names_from = Var2, values_from = Freq) %>%
    dplyr::rename(
      Variable = Var1,
      after = `0`,
      before = `1`,
    ) %>%
    mutate(
      before_n = sum(before, na.rm = T),
      after_n = sum(after, na.rm = T)
    ) %>%
    mutate(before_prop = before/sum(before, na.rm = T),
           after_prop = after/sum(after, na.rm = T)) %>%
    group_by(Variable) %>%
    mutate(after_before_p = prop.test(x = c(before, after),
                                      n = c(before_n, after_n))$p.value
    ) %>%
    select(Variable,before, before_prop, after, after_n, after_prop, 
           after_before_p) %>%
    mutate(
      before_prop = round(before_prop, 3),
      after_prop = round(after_prop, 3),
      after_before_p = round(after_before_p, 3)
    ) %>%
    dplyr::rename(
      Before = before_prop,
      After = after_prop,
      `After - Before p` = after_before_p
    ) %>%
    mutate(Before = paste0(Before, " (", before, ")"),
           After = paste0(After, " (", after, ")")) %>%
    select(Variable, Before, After, `After - Before p`)
  
  dbWriteTable(iaan,
               "TABLE.SUM_STATS_LIST_TYPE_BY_AFTER",
               listingTypeTable)
  rm(listingTypeTable)
  
} else {
  
}

if(
  !("TABLE.SUM_STATS_ROOM_TYPE_BY_AFTER" %in% dbListTables(iaan))
) {
  
  roomTypeTable <- (table(facts$room_type, facts$after)) %>%
    as.data.frame() %>% 
    pivot_wider(names_from = Var2, values_from = Freq) %>%
    dplyr::rename(
      Variable = Var1,
      after = `0`,
      before = `1`,
    ) %>%
    mutate(
      before_n = sum(before, na.rm = T),
      after_n = sum(after, na.rm = T)
    ) %>%
    mutate(before_prop = before/sum(before, na.rm = T),
           after_prop = after/sum(after, na.rm = T)) %>%
    group_by(Variable) %>%
    mutate(after_before_p = prop.test(x = c(before, after),
                                      n = c(before_n, after_n))$p.value
    ) %>%
    select(Variable,before, before_prop, after, after_n, after_prop, 
           after_before_p) %>%
    mutate(
      before_prop = round(before_prop, 3),
      after_prop = round(after_prop, 3),
      after_before_p = round(after_before_p, 3)
    ) %>%
    dplyr::rename(
      Before = before_prop,
      After = after_prop,
      `After - Before p` = after_before_p
    ) %>%
    mutate(Before = paste0(Before, " (", before, ")"),
           After = paste0(After, " (", after, ")")) %>%
    select(Variable, Before, After, `After - Before p`)
  
  dbWriteTable(iaan,
               "TABLE.SUM_STATS_ROOM_TYPE_BY_AFTER",
               roomTypeTable)
  rm(roomTypeTable)
  
} else {
  
}

rm(facts, treated)

#Summary Stats Tables-----------------------------------------------------------
sum_stats_by_after <- getTableMaster(
  "SUM_STATS_BY_AFTER"
)
sum_stats_list_type_by_after <- getTableMaster(
  "SUM_STATS_LIST_TYPE_BY_AFTER"
)
sum_stats_room_type_by_after <- getTableMaster(
  "SUM_STATS_ROOM_TYPE_BY_AFTER"
)
