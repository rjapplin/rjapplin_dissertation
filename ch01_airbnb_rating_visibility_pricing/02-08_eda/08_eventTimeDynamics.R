ia_sample <- dbGetQuery(iaan,
                          "SELECT KEY_LIST_ID, KEY_HOST_ID, DATE,
                          year, month, first_review_date, treatment_group,
                          price, overall_rating FROM [DATA.FULL_SAMPLE]") %>%
  left_join(cpi, by = c("year", "month")) %>%
  mutate(real_price = (price/cpi_b2015)*100) %>%
  select(-cpi, -cpi_b2015) %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  mutate(first_review_date = min(as.Date(first_review_date), na.rm = T)) %>%
  mutate(date = as.Date(paste0(year, "-", month, "-", substr(DATE, 7, 8)))) %>%
  mutate(after = ifelse(date >= first_review_date, 1, 0)) %>%
  mutate(flag = ifelse(after == 0 & !(is.na(overall_rating)), 1, 0)) %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  mutate(ever_flag = max(flag)) %>%
  filter(ever_flag == 0) %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  dplyr::mutate(min_date = 
           min(as.Date(paste0(year, "-", month, "-", substr(DATE, 7, 8))))) %>%
  arrange(KEY_LIST_ID, KEY_HOST_ID, DATE) %>% 
  mutate(lobs = row_number()) %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID, after) %>%
  mutate(time_of_event = min(lobs)) %>%
  mutate(time_of_event = ifelse(after == 1, time_of_event, NA)) %>% 
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  mutate(time_of_event = max(time_of_event, na.rm = T)) %>%
  mutate(event_time = lobs - time_of_event) %>%
  mutate(max_lobs = max(lobs),
         max_et = max(event_time),
         min_et = min(event_time))

ia_sample %>%
  ungroup() %>%
  mutate(d = as.Date(paste0(year, "-", month, "-", substr(DATE, 7, 8)))) %>%
  mutate(event_time2 = d - first_review_date) %>%
  mutate(event_time2 = as.numeric(event_time2)) %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  dplyr::mutate(max_et2 = max(event_time2),
                min_et2 = min(event_time2)) %>%
  select(KEY_LIST_ID, KEY_HOST_ID, DATE, price, real_price, overall_rating,
         after, lobs, treatment_group, time_of_event, event_time, 
         max_lobs, max_et, min_et, event_time2, max_et2, min_et2) -> ia_sample

#Maximum Number of Observations Per Listing-------------------------------------
temp <- ia_sample %>%
  select(KEY_LIST_ID, KEY_HOST_ID, treatment_group, max_lobs) %>%
  distinct()
table(temp$treatment_group, temp$max_lobs) %>%
  as.data.frame() %>%
  dplyr::rename(
    Group = Var1,
    MaxObs = Var2
  ) %>%
  dbWriteTable(iaan,
               "FIGURE.MAX_OBS_PER_LIST",
               .,
               overwrite = T)

#Time Of Event------------------------------------------------------------------
temp <- ia_sample %>%
  filter(treatment_group == "Treated") %>%
  select(KEY_LIST_ID, KEY_HOST_ID, time_of_event) %>%
  distinct()
  
table(temp$time_of_event) %>%
  as.data.frame() %>%
  dplyr::rename(time_of_event = Var1) %>%
  mutate(time_of_event = as.numeric(time_of_event)) %>%
  filter(!(is.infinite(time_of_event))) %>%
  dbWriteTable(iaan,
               "FIGURE.TIME_OF_EVENT_FREQ",
               .,
               overwrite = T)

#Event Time---------------------------------------------------------------------
temp <- ia_sample %>%
  filter(treatment_group == "Treated") %>%
  select(KEY_LIST_ID, KEY_HOST_ID, event_time, min_et, max_et,
         event_time2, min_et2, max_et2, treatment_group) %>%
  distinct()

table(temp$event_time) %>%
  as.data.frame() %>%
  dplyr::rename(event_time = Var1) %>%
  mutate(event_time = as.numeric(as.character(event_time))) %>%
  filter(!(is.infinite(event_time))) %>%
  dbWriteTable(iaan,
               "FIGURE.EVENT_TIME_FREQ",
               .,
               overwrite = T)

temp2 <- temp %>%
  select(KEY_LIST_ID, KEY_HOST_ID, min_et, max_et) %>%
  distinct()

table(temp2$min_et) %>%
  as.data.frame() %>%
  dplyr::rename(min_event_time = Var1) %>%
  mutate(min_event_time = as.numeric(as.character(min_event_time))) %>%
  filter(!(is.infinite(min_event_time))) %>%
  dbWriteTable(iaan,
               "FIGURE.MIN_EVENT_TIME_FREQ",
               .,
               overwrite = T)

table(temp2$max_et) %>%
  as.data.frame() %>%
  dplyr::rename(max_event_time = Var1) %>%
  mutate(max_event_time = as.numeric(as.character(max_event_time))) %>%
  filter(!(is.infinite(max_event_time))) %>%
  dbWriteTable(iaan,
               "FIGURE.MAX_EVENT_TIME_FREQ",
               .,
               overwrite = T)

#Real Event Time----------------------------------------------------------------
temp <- ia_sample %>%
  filter(treatment_group == "Treated" | treatment_group == "Always Treated") %>%
  select(KEY_LIST_ID, KEY_HOST_ID, event_time2, min_et2, max_et2,
         treatment_group) %>%
  distinct()

temp <- temp %>%
  mutate(real_et_m = round(event_time2/30),
         real_et_q = round(event_time2/90)) %>%
  mutate(min_real_et_m = round(min_et2/30),
         min_real_et_q = round(min_et2/90),
         max_real_et_m = round(max_et2/30),
         max_real_et_q = round(max_et2/90)) %>%
  select(-event_time2, -min_et2, -max_et2) %>%
  distinct()

#Real Event Time Frequency------------------------------------------------------
table(temp$real_et_m, temp$treatment_group) %>%
  as.data.frame() %>%
  dplyr::rename(
    real_event_time = Var1,
    Group = Var2
  ) %>%
  filter(Freq > 0) %>%
  mutate(
    real_event_time = as.numeric(as.character(real_event_time))
  ) %>%
  filter(!(is.infinite(real_event_time))) %>%
  mutate(real_type = "month") %>%
  rbind(
    table(temp$real_et_q, temp$treatment_group) %>%
      as.data.frame() %>%
      dplyr::rename(
        real_event_time = Var1,
        Group = Var2
      ) %>%
      filter(Freq > 0) %>%
      mutate(
        real_event_time = as.numeric(as.character(real_event_time))
      ) %>%
      filter(!(is.infinite(real_event_time))) %>%
      mutate(real_type = "quarter")
  ) %>%
  dbWriteTable(iaan,
               "FIGURE.REAL_EVENT_TIME_FREQ",
               .,
               overwrite = T)

#Min and Max Real Event Time----------------------------------------------------
temp2 <- temp %>%
  select(KEY_LIST_ID, KEY_HOST_ID, treatment_group, min_real_et_m, 
         min_real_et_q, max_real_et_m, max_real_et_q) %>%
  distinct()

table(temp2$min_real_et_m, temp2$treatment_group) %>%
  as.data.frame() %>%
  dplyr::rename(
    min_real_event_time = Var1,
    Group = Var2
  ) %>%
  filter(Freq > 0) %>%
  mutate(
    min_real_event_time = as.numeric(as.character(min_real_event_time))
  ) %>%
  filter(!(is.infinite(min_real_event_time))) %>%
  mutate(real_type = "month") %>%
  rbind(
    table(temp2$min_real_et_q, temp2$treatment_group) %>%
      as.data.frame() %>%
      dplyr::rename(
        min_real_event_time = Var1,
        Group = Var2
      ) %>%
      filter(Freq > 0) %>%
      mutate(
        min_real_event_time = as.numeric(as.character(min_real_event_time))
      ) %>%
      filter(!(is.infinite(min_real_event_time))) %>%
      mutate(real_type = "quarter")
  ) %>%
  dbWriteTable(iaan,
               "FIGURE.MIN_REAL_EVENT_TIME_FREQ",
               .,
               overwrite = T)



table(temp2$max_real_et_m, temp2$treatment_group) %>%
  as.data.frame() %>%
  dplyr::rename(
    max_real_event_time = Var1,
    Group = Var2
  ) %>%
  filter(Freq > 0) %>%
  mutate(
    max_real_event_time = as.numeric(as.character(max_real_event_time))
  ) %>%
  filter(!(is.infinite(max_real_event_time))) %>%
  mutate(real_type = "month") %>%
  rbind(
    table(temp2$max_real_et_q, temp2$treatment_group) %>%
      as.data.frame() %>%
      dplyr::rename(
        max_real_event_time = Var1,
        Group = Var2
      ) %>%
      filter(Freq > 0) %>%
      mutate(
        max_real_event_time = as.numeric(as.character(max_real_event_time))
      ) %>%
      filter(!(is.infinite(max_real_event_time))) %>%
      mutate(real_type = "quarter")
  ) %>%
  dbWriteTable(iaan,
               "FIGURE.MAX_REAL_EVENT_TIME_FREQ",
               .,
               overwrite = T)

#Average Price Dynamics---------------------------------------------------------
temp <- ia_sample %>%
  select(KEY_LIST_ID, KEY_HOST_ID, price, real_price, event_time,
         event_time2, treatment_group)

#Nominal Event Time
temp %>%
  filter(event_time >= -24 & event_time <= 24) %>%
  filter(treatment_group == "Treated") %>%
  ungroup() %>%
  group_by(event_time) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan, 
               "FIGURE.PRICES_BY_NOM_EVENT_TIME",
               .,
               overwrite = T)

#Real Event Time - Monthly
temp %>%
  mutate(real_event_time_m = round(event_time2/30)) %>%
  filter(treatment_group == "Treated" | treatment_group == "Always Treated") %>%
  group_by(treatment_group, real_event_time_m) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  filter(real_event_time_m >= -24 & real_event_time_m <= 24) %>%
  dbWriteTable(iaan, 
               "FIGURE.PRICES_BY_REAL_EVENT_TIME_M",
               .,
               overwrite = T)

#Real Event Time - Quarterly
temp %>%
  mutate(real_event_time_q = round(event_time2/90)) %>%
  filter(treatment_group == "Treated" | treatment_group == "Always Treated") %>%
  group_by(treatment_group, real_event_time_q) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  filter(real_event_time_q >= -8 & real_event_time_q <= 8) %>%
  dbWriteTable(iaan, 
               "FIGURE.PRICES_BY_REAL_EVENT_TIME_Q",
               .,
               overwrite = T)

#Price Dynamics by Observation Number-------------------------------------------
temp <- ia_sample %>%
  select(KEY_LIST_ID, KEY_HOST_ID, price, real_price, lobs, treatment_group)

temp %>%
  filter(lobs <= 24) %>%
  group_by(treatment_group, lobs) %>%
  dplyr::summarize(
    price = mean(price, na.rm = T),
    real_price = mean(real_price, na.rm = T)
  ) %>%
  pivot_longer(cols = c(price, real_price)) %>%
  dbWriteTable(iaan,
               "FIGURE.PRICES_BY_OBS_NO",
               .,
               overwrite = T)

#Save Event Time----------------------------------------------------------------
ia_sample %>%
  select(KEY_LIST_ID, KEY_HOST_ID, DATE, lobs, time_of_event,
         event_time, event_time2) %>%
  dbWriteTable(iaan,
               "DATA.EVENT_TIME",
               .,
               overwrite = TRUE)

#Cleanup------------------------------------------------------------------------
rm(temp, temp2, ia_sample)
    
  
  


  


