#Data Wrangling For CS Event Studies - Real Event Time--------------------------
if(!("DATA.CS_SAMPLE" %in% dbListTables(iaan))){

  cs_sample <- dbGetQuery(iaan, "SELECT * FROM [DATA.ANALYSIS_SAMPLE] 
                                  WHERE (event_time2 >= -151 AND
                                  event_time2 <= 121) OR
                                  (treatment_group = 'Never Treated')")
  cs_sample <- cs_sample %>%
    mutate(event_time2 = round(event_time2/30)) %>%
    ungroup() %>%
    mutate(event_time = 
             case_when(
               treatment_group == 'Treated' ~ event_time,
               treatment_group == 'Never Treated' ~ 0,
               treatment_group == 'Always Treated' ~ 999
             )
    ) %>%
    filter(treatment_group != "Always Treated")
    
  cs_sample <- cs_sample %>%
    dplyr::group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
    dplyr::mutate(id = cur_group_id()) %>%
    dplyr::group_by(id) %>%
    select(-KEY_LIST_ID, -KEY_HOST_ID)
  
  cs_sample <- cs_sample %>%
    arrange(year, month) %>%
    group_by(year, month) %>%
    mutate(period = cur_group_id()) %>%
    group_by(id) %>%
    dplyr::mutate(G = ifelse(event_time2 == 0, period, NA)) %>%
    dplyr::mutate(G = min(G, na.rm = T)) %>%
    dplyr::mutate(G = ifelse(treatment_group == "Never Treated", 0, G)) %>%
    dplyr::mutate(G = ifelse(is.infinite(G), 0, G))
  
  cs_sample <- cs_sample %>%
    select(id, period, G, treatment_group, treated_after, price, real_price,
           price1, price2, price3, real_price1, real_price2, real_price3,
           available_030, bathrooms, bedrooms, bedrooms, capacity, covid,
           listing_type, room_type, number_of_reviews, AC7, AC51, AC72, year,
           month, CITY_ID)
  
  cs_sample <- cs_sample %>%
    mutate(d = 30 - available_030)
  
  cs_sample %>%
    dbWriteTable(iaan, "DATA.CS_SAMPLE", .)

} else {
  
  cs_sample <- dbReadTable(iaan, "DATA.CS_SAMPLE")
  
}

cs_sample <- na.omit(cs_sample)

#Estimation---------------------------------------------------------------------
for(i in 1:200){
  
  if(i <= 100){
    cg <- "notyettreated"
  } else {
    cg <- "nevertreated"
  }
  
  seed <- sample(1:999999, 1)
  set.seed(seed)
  Gl <- sample(unique(cs_sample$G), 1, T)
  cs_subsample <- cs_sample %>% filter(G == 0 | G %in% Gl:(Gl+9))
  nid <- length(unique(cs_subsample$id))
  fid <- (0.2*nid) %>% round()
  sid <- sample(unique(cs_subsample$id), fid, T)
  cs_subsample1 <- cs_subsample %>% filter(id %in% sid)
  cs_subsample1 <- cs_subsample1 %>%
    select(id, period, G, real_price, real_price1, bedrooms,
                          bathrooms, capacity, AC7, AC51, AC72, month,
           treatment_group) %>% 
    na.omit() %>%
    group_by(id) %>%
    mutate(n = n_distinct(period)) %>%
    filter(n >= 6)
  
  
  attgt <- try(
    att_gt(yname = "real_price",
           tname = "period",
           idname = "id",
           gname = "G",
           xformla = ~0 + real_price1,
           data = cs_subsample1,
           allow_unbalanced_panel = T,
           control_group = cg,
           biters = 100,
           pl = T,
           cores = 4,
           anticipation = 1)
  )
  
  if("try-error" %in% class(attgt)){
    print(paste0("Error on iteration ", i))
    if(exists("res")){
      res <- res %>%
        rbind(
          data.frame(
            et = NA,
            est = NA,
            est_se = NA,
            overall = NA,
            overall_se = NA,
            iteration = i,
            seed = seed,
            control = cg
          )
        )
    } else {
      res <-data.frame(
        et = NA,
        est = NA,
        est_se = NA,
        overall = NA,
        overall_se = NA,
        iteration = i,
        seed = seed,
        control = cg
      )
    }
  } else {
        
    attgt <- aggte(attgt, type = "dynamic", na.rm = T)
    if(exists("res")){
      res <- res %>% rbind(
        data.frame(
          et = attgt$egt,
          est = attgt$att.egt,
          est_se = attgt$se.egt,
          overall = attgt$overall.att,
          overall_se = attgt$overall.se,
          iteration = i,
          seed = seed,
          control = cg
        )
      )
    } else {
      res <-data.frame(
        et = attgt$egt,
        est = attgt$att.egt,
        est_se = attgt$se.egt,
        overall = attgt$overall.att,
        overall_se = attgt$overall.se,
        iteration = i,
        seed = seed,
        control = cg
      )
    }
    
  }
  
  gc()
  print(i)
  
  
}
  
dbWriteTable(iaan, "MODEL.CS_EVENT_STUDIES", res, overwrite = T)
rm(cs_sample, cs_subsample, cs_subsample1, attgt, cg, fid, Gl, i, nid, seed,
   sid, res)
gc()
