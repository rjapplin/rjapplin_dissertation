#Get Full Sample----------------------------------------------------------------
if(!("TABLE.SUM_STATS_BY_AFTER" %in% dbListTables(iaan))) {

full_sample <- dbReadTable(iaan, "DATA.FULL_SAMPLE") %>%
  left_join(cpi, by = c("year", "month")) %>%
  mutate(real_price = (price/cpi_b2015)*100)

#Group Sum Stats by Treatment Group---------------------------------------------
groupSampStats <- describeBy(full_sample, full_sample$treatment_group)
rm(full_sample)

#Always-------------------------------------------------------------------------
alwaysSampStats <- groupSampStats$`Always Treated`
alwaysSampStatsTable <- alwaysSampStats %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(vars = ifelse(Variable == "price", -1, vars)) %>%
  mutate(vars = ifelse(Variable == "real_price", 0, vars)) %>%
  arrange(vars) %>%
  filter(Variable %in% c("price", "real_price", "capacity", "bathrooms", "bedrooms",
                         "min_night", "max_night", "available",
                         "available_030", "available_060", "available_090",
                         "available_365",
                         "number_of_reviews",
                         "overall_rating", "accom_rating", "clean_rating",
                         "checkin_rating", "com_rating", "loc_rating",
                         "value_rating",
                         "amenity_word_count", "amenity_count",
                         "amenity_char_count", "loc_overview_word_count",
                         "descrip_word_count", "loc_overview_char_count",
                         "descrip_char_count", "host_response_rate",
                         "host_accept_rate", "host_is_superhost",
                         "host_listing_count", "host_has_pic", "host_verified",
                         "host_about_word_count", "host_about_char_count") |
           grepl("AC", Variable)) %>%
  select(Variable, mean, sd, se, n) %>%
  dplyr::rename(
    always_mean = mean,
    always_sd = sd,
    always_se = se,
    always_n = n
  )
  
#Treated------------------------------------------------------------------------
treatdSampStats <- groupSampStats$Treated
treatdSampStatsTable <- treatdSampStats %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(vars = ifelse(Variable == "price", -1, vars)) %>%
  mutate(vars = ifelse(Variable == "real_price", 0, vars)) %>%
  arrange(vars) %>%
  filter(Variable %in% c("price", "real_price", "capacity", "bathrooms", "bedrooms",
                         "min_night", "max_night", "available",
                         "available_030", "available_060", "available_090",
                         "available_365",
                         "number_of_reviews",
                         "overall_rating", "accom_rating", "clean_rating",
                         "checkin_rating", "com_rating", "loc_rating",
                         "value_rating",
                         "amenity_word_count", "amenity_count",
                         "amenity_char_count", "loc_overview_word_count",
                         "descrip_word_count", "loc_overview_char_count",
                         "descrip_char_count", "host_response_rate",
                         "host_accept_rate", "host_is_superhost",
                         "host_listing_count", "host_has_pic", "host_verified",
                         "host_about_word_count", "host_about_char_count") |
           grepl("AC", Variable)) %>%
  select(Variable, mean, sd, se, n) %>%
  dplyr::rename(
    treatd_mean = mean,
    treatd_sd = sd,
    treatd_se = se,
    treatd_n = n
  )

#Never--------------------------------------------------------------------------
neverdSampStats <- groupSampStats$`Never Treated`
neverdSampStatsTable <- neverdSampStats %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(vars = ifelse(Variable == "price", -1, vars)) %>%
  mutate(vars = ifelse(Variable == "real_price", 0, vars)) %>%
  arrange(vars) %>%
  filter(Variable %in% c("price", "real_price", "capacity", "bathrooms", "bedrooms",
                         "min_night", "max_night", "available",
                         "available_030", "available_060", "available_090",
                         "available_365",
                         "number_of_reviews",
                         "overall_rating", "accom_rating", "clean_rating",
                         "checkin_rating", "com_rating", "loc_rating",
                         "value_rating",
                         "amenity_word_count", "amenity_count",
                         "amenity_char_count", "loc_overview_word_count",
                         "descrip_word_count", "loc_overview_char_count",
                         "descrip_char_count", "host_response_rate",
                         "host_accept_rate", "host_is_superhost",
                         "host_listing_count", "host_has_pic", "host_verified",
                         "host_about_word_count", "host_about_char_count") |
           grepl("AC", Variable)) %>%
  select(Variable, mean, sd, se, n) %>%
  dplyr::rename(
    never_mean = mean,
    never_sd = sd,
    never_se = se,
    never_n = n
  )

#Bring Together-----------------------------------------------------------------
groupSampTable <- alwaysSampStatsTable %>%
  inner_join(treatdSampStatsTable, by = "Variable") %>%
  inner_join(neverdSampStatsTable, by = "Variable") 

#Clean Up-----------------------------------------------------------------------
rm(alwaysSampStatsTable, alwaysSampStats, treatdSampStatsTable, treatdSampStats,
   neverdSampStats, neverdSampStatsTable, groupSampStats)

#Compute t-stats----------------------------------------------------------------
groupSampTable %>%
  mutate(treat_always = treatd_mean - always_mean,
         treat_never = treatd_mean - never_mean,
         always_never = always_mean - never_mean
  ) %>%
  mutate(
         t_treat_always = (treat_always)/(sqrt(treatd_se + always_se)),
         t_treat_never = (treat_never)/(sqrt(treatd_se + never_se)),
         t_always_never = (always_never)/(sqrt(always_se + never_se))
  ) %>%
  mutate(
    v_treat_always = computeV(treatd_sd, always_sd, treatd_n, always_n),
    v_treat_never = computeV(treatd_sd, never_sd, treatd_n, never_n),
    v_always_never = computeV(always_sd, never_sd, always_n, never_n)
  ) %>%
  mutate(p_treat_always = 2*pt(abs(t_treat_always), 
                               df = v_treat_always,
                               lower.tail = F),
         p_treat_never = 2*pt(abs(t_treat_never),
                              df = v_treat_never,
                              lower.tail = F),
         p_always_never = 2*pt(abs(t_always_never),
                               df = v_always_never,
                               lower.tail = F)) %>%
  select(Variable,
         treatd_mean,
         always_mean,
         never_mean,
         p_treat_always,
         p_treat_never,
         p_always_never) %>%
  mutate(
    treatd_mean = round(treatd_mean, 3),
    always_mean = round(always_mean, 3),
    never_mean = round(never_mean, 3),
    p_treat_always = round(p_treat_always, 3),
    p_treat_never = round(p_treat_never, 3),
    p_always_never = round(p_always_never, 3)
  ) %>%
  dplyr::rename(
    Treated = treatd_mean,
    Always = always_mean,
    Never = never_mean,
    `Treat - Always p` = p_treat_always,
    `Treat - Never p` = p_treat_never,
    `Always - Never p` = p_always_never
  )  %>%
  left_join(read.csv("amenity_map.csv") %>%
              select(HUMAN_LABEL, AMENITY_CLEAN_CODE) %>%
              dplyr::rename(Variable = AMENITY_CLEAN_CODE),
            by = "Variable") %>%
  mutate(
    Variable = ifelse(is.na(HUMAN_LABEL), Variable, HUMAN_LABEL)
  ) %>%
  select(-HUMAN_LABEL)

dbWriteTable(iaan,
             "TABLE.SUM_STATS_BY_TREAT",
             groupSampTable2,
             overwrite = T)
rm(groupSampTable, groupSampTable2)

} else {
  
}

#Factor Variables---------------------------------------------------------------
if(
  !("TABLE.SUM_STATS_LIST_TYPE_BY_TREAT" %in% dbListTables(iaan)) |
  !("TABLE.SUM_STATS_ROOM_TYPE_BY_TREAT" %in% dbListTables(iaan))
) {

facts <- dbGetQuery(iaan,
                    "SELECT listing_type, room_type, zip, price, 
                    overall_rating, treatment_group FROM [DATA.FULL_SAMPLE]")

facts %>%
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

} else {
  
  
}

#Listing Type Table-------------------------------------------------------------
if(!("TABLE.SUM_STATS_LIST_TYPE_BY_TREAT" %in% dbListTables(iaan))){
  
  listingTypeTable <- (table(facts$listing_type, facts$treatment_group)) %>%
    as.data.frame() %>% 
    pivot_wider(names_from = Var2, values_from = Freq) %>%
    dplyr::rename(
      Variable = Var1,
      always = `Always Treated`,
      never = `Never Treated`,
      treat = `Treated`
    ) %>%
    mutate(
      always_n = sum(always, na.rm = T),
      never_n = sum(never, na.rm = T),
      treat_n = sum(treat, na.rm = T)
    ) %>%
    mutate(always_prop = always/sum(always, na.rm = T),
           never_prop = never/sum(never, na.rm = T),
           treat_prop = treat/sum(treat, na.rm = T)) %>%
    group_by(Variable) %>%
    mutate(treat_always_p = prop.test(x = c(treat, always),
                                      n = c(treat_n, always_n))$p.value,
           treat_never_p = prop.test(x = c(treat, never),
                                     n = c(treat_n, never_n))$p.value,
           always_never_p = prop.test(x = c(always, never),
                                      n = c(always_n, never_n))$p.value
    ) %>%
    select(Variable,always, always_prop, never, never_n, never_prop, 
           treat, treat_prop,
           treat_always_p, treat_never_p, always_never_p) %>%
    mutate(
      always_prop = round(always_prop, 3),
      never_prop = round(never_prop, 3),
      treat_prop = round(treat_prop, 3),
      treat_always_p = round(treat_always_p, 3),
      treat_never_p = round(treat_never_p, 3),
      always_never_p = round(always_never_p, 3)
    ) %>%
    dplyr::rename(
      Always = always_prop,
      Treated = treat_prop,
      Never = never_prop,
      `Treat - Always p` = treat_always_p,
      `Treat - Never p` = treat_never_p,
      `Always - Never p` = always_never_p
    ) %>%
    mutate(Always = paste0(Always, " (", always, ")"),
           Treated = paste0(Treated, " (", treat, ")"),
           Never = paste0(Never, " (", never, ")")) %>%
    select(Variable, Treated, Always, Never, `Treat - Always p`,
           `Treat - Never p`, `Always - Never p`)
  
  dbWriteTable(iaan,
               "TABLE.SUM_STATS_LIST_TYPE_BY_TREAT",
               listingTypeTable,
               overwrite = T)
  rm(listingTypeTable)
  
} else {
  
}
  
#Room Type Table-------------------------------------------------------------
if(!("TABLE.SUM_STATS_ROOM_TYPE_BY_TREAT" %in% dbListTables(iaan))){
  
  roomTypeTable <- (table(facts$room_type, facts$treatment_group)) %>%
    as.data.frame() %>% 
    pivot_wider(names_from = Var2, values_from = Freq) %>%
    dplyr::rename(
      Variable = Var1,
      always = `Always Treated`,
      never = `Never Treated`,
      treat = `Treated`
    ) %>%
    mutate(
      always_n = sum(always, na.rm = T),
      never_n = sum(never, na.rm = T),
      treat_n = sum(treat, na.rm = T)
    ) %>%
    mutate(always_prop = always/sum(always, na.rm = T),
           never_prop = never/sum(never, na.rm = T),
           treat_prop = treat/sum(treat, na.rm = T)) %>%
    group_by(Variable) %>%
    mutate(treat_always_p = prop.test(x = c(treat, always),
                                      n = c(treat_n, always_n))$p.value,
           treat_never_p = prop.test(x = c(treat, never),
                                     n = c(treat_n, never_n))$p.value,
           always_never_p = prop.test(x = c(always, never),
                                      n = c(always_n, never_n))$p.value
    ) %>%
    select(Variable,always, always_prop, never, never_n, never_prop, 
           treat, treat_prop,
           treat_always_p, treat_never_p, always_never_p) %>%
    mutate(
      always_prop = round(always_prop, 3),
      never_prop = round(never_prop, 3),
      treat_prop = round(treat_prop, 3),
      treat_always_p = round(treat_always_p, 3),
      treat_never_p = round(treat_never_p, 3),
      always_never_p = round(always_never_p, 3)
    ) %>%
    dplyr::rename(
      Always = always_prop,
      Treated = treat_prop,
      Never = never_prop,
      `Treat - Always p` = treat_always_p,
      `Treat - Never p` = treat_never_p,
      `Always - Never p` = always_never_p
    ) %>%
    mutate(Always = paste0(Always, " (", always, ")"),
           Treated = paste0(Treated, " (", treat, ")"),
           Never = paste0(Never, " (", never, ")")) %>%
    select(Variable, Treated, Always, Never, `Treat - Always p`,
           `Treat - Never p`, `Always - Never p`)
  
  dbWriteTable(iaan,
               "TABLE.SUM_STATS_ROOM_TYPE_BY_TREAT",
               roomTypeTable,
               overwrite = T)
  rm(roomTypeTable)

} else {
  
}

#Cleanup------------------------------------------------------------------------
rm(facts, groupSampTable, groupSampTable2, listingTypeTable, roomTypeTable)

#Read Tables--------------------------------------------------------------------

sum_stats_by_treat <- getTableMaster("SUM_STATS_BY_TREAT")
sum_stats_list_type_by_treat <- getTableMaster("SUM_STATS_LIST_TYPE_BY_TREAT")
sum_stats_room_type_by_treat <- getTableMaster("SUM_STATS_ROOM_TYPE_BY_TREAT")





