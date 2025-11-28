#BSTS - Prepare Sample----------------------------------------------------------
if("DATA.BSTS_SAMPLE" %in% dbListTables(iaan)){
  brodSample <- dbReadTable(iaan, "DATA.BSTS_SAMPLE")
} else {
  brodSample <- dbReadTable(iaan, "DATA.ANALYSIS_SAMPLE") %>%
    select(KEY_LIST_ID, KEY_HOST_ID,real_price, bathrooms, bedrooms, capacity, covid,
           listing_type, room_type, AC7, AC51, AC72, year, month,
           event_time2, treatment_group, r, amenity_word_count,
           amenity_char_count, descrip_word_count, descrip_char_count, 
           loc_overview_char_count, loc_overview_word_count, host_about_word_count,
           host_about_char_count, host_is_superhost, host_verified, host_response_rate,
           host_accept_rate, CITY_ID) %>%
    mutate(
      listing_type = ifelse(
        listing_type %in% c("apartment", "home", "rental unit", "condo",
                            "guest suite", "townhouse", "guesthouse", "loft",
                            "serviced apartment"), listing_type,
        "other"
      )
    ) %>%
    filter(treatment_group == "Treated") %>%
    mutate(event_time2 = round(event_time2/30)) %>%
    na.omit()
  
  ltypes <- model.matrix(~brodSample$listing_type) %>%
    as.data.frame() %>%
    select(-`(Intercept)`)
  colnames(ltypes) <- paste0("LT", 1:dim(ltypes)[2])
  brodSample <- brodSample %>%
    cbind(ltypes) %>%
    select(-listing_type)
  rm(ltypes)
  
  rtypes <- model.matrix(~brodSample$room_type) %>%
    as.data.frame() %>%
    select(-`(Intercept)`)
  colnames(rtypes) <- paste0("RT", 1:dim(rtypes)[2])
  brodSample <- brodSample %>%
    cbind(rtypes) %>%
    select(-room_type)
  rm(rtypes)
  
  years <- model.matrix(~brodSample$year) %>%
    as.data.frame() %>%
    select(-`(Intercept)`)
  colnames(years) <- paste0("Y", 1:dim(years)[2])
  brodSample <- brodSample %>%
    cbind(years) %>%
    select(-year)
  rm(years)
  
  months <- model.matrix(~brodSample$month) %>%
    as.data.frame() %>%
    select(-`(Intercept)`)
  colnames(months) <- paste0("M", 1:dim(months)[2])
  brodSample <- brodSample %>%
    cbind(months) %>%
    select(-month)
  rm(months)
  
  cities <- model.matrix(~brodSample$CITY_ID) %>%
    as.data.frame() %>%
    select(-`(Intercept)`)
  colnames(cities) <- paste0("C", 1:dim(cities)[2])
  brodSample <- brodSample %>%
    cbind(cities) %>%
    select(-CITY_ID)
  rm(cities)
  
  
  brodSample <- brodSample %>%
    select(-treatment_group)
  
  brodSample <- brodSample %>%
    group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
    mutate(id = cur_group_id()) %>%
    ungroup() %>%
    select(-KEY_LIST_ID, -KEY_HOST_ID)
  
  dbWriteTable(iaan, "DATA.BSTS_SAMPLE", brodSample, overwrite = T)
  
}

#Prepare Database Tables--------------------------------------------------------

if(!("MODEL.BSTS_IND" %in% dbListTables(iaan))){
  dbExecute(iaan,
            "CREATE TABLE IF NOT EXISTS [MODEL.BSTS_IND] (
              observed REAL,
              fitted REAL,
              fitted_lo REAL,
              fitted_hi REAL,
              delta REAL,
              delta_lo REAL,
              delta_hi REAL,
              et INT,
              rating REAL,
              id INT,
              n INT
              )")
}

if(!("MODEL.BSTS_IND_PLAC" %in% dbListTables(iaan))){
  dbExecute(iaan,
            "CREATE TABLE IF NOT EXISTS [MODEL.BSTS_IND_PLAC] (
              observed REAL,
              fitted REAL,
              fitted_lo REAL,
              fitted_hi REAL,
              delta REAL,
              delta_lo REAL,
              delta_hi REAL,
              et INT,
              rating REAL,
              id INT,
              n INT
              )")
}

#Estimation---------------------------------------------------------------------
lapply(unique(brodSample$id), function(cid){
  dsubsample <- brodSample %>%
    filter(id == cid) %>%
    mutate(neg = event_time2 <= -1) %>%
    mutate(neg = sum(neg)) %>%
    arrange(event_time2) %>%
    select(-id) %>%
    na.omit()
  
  if(unique(dsubsample$neg) < 6){
    print(paste0("Less than 6 pre-periods. Skipping ", cid))
    return()
  }

  pre.period <- c(1, unique(dsubsample$neg))
  post.period <- c(unique(dsubsample$neg) + 1, dim(dsubsample)[1])
  
  impact <- try(
    CausalImpact(dsubsample 
                 %>% select(-neg, -event_time2),
                 pre.period = pre.period,
                 post.period = post.period,
                 model.args = list(nseasons = 4,
                                   season.duration = 3))
  )
  
  if("try-error" %in% class(impact)){
    print(paste0("Estimation not successful for ", cid))
    return()
  } else {
    
    impactdf <- impact$series %>%
      as.data.frame() %>%
      select(response, point.pred, point.pred.lower, point.pred.upper,
             point.effect, point.effect.lower, point.effect.upper) %>%
      cbind(
        dsubsample$event_time2,
        dsubsample$r
      ) %>%
      dplyr::rename(
        observed = response,
        fitted = point.pred,
        fitted_lo = point.pred.lower,
        fitted_hi = point.pred.upper,
        delta = point.effect,
        delta_lo = point.effect.lower,
        delta_hi = point.effect.upper
      ) %>%
      dplyr::rename(
        et = `dsubsample$event_time2`,
        rating = `dsubsample$r`,
      ) %>%
      mutate(id = cid,
             n = dim(dsubsample)[1])
    
    dbWriteTable(iaan, "MODEL.BSTS_IND", impactdf, append = T)
    print(paste0("Success for ", cid))
    
  }
}
)

rm(brodSample, dsubsample, impact, impactdf, cid, post.period, pre.period)

#Examine Heterogeneity----------------------------------------------------------
res <- dbReadTable(iaan, "MODEL.BSTS_IND")
ols <- (res %>%
  filter(et >= 0 & et <= 12) %>%
  mutate(et = as.factor(et)) %>%
  felm(delta ~ rating, .) %>%
  summary())$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  filter(Feature == "rating") %>%
  select(Feature, Estimate, `Std. Error`, `Pr(>|t|)`) %>%
  dplyr::rename(p = `Pr(>|t|)`,
                SE = `Std. Error`) %>%
  mutate(method = "OLS")

fe <- (res %>%
         filter(et >= 0 & et <= 12) %>%
         mutate(et = as.factor(et)) %>%
         felm(delta ~ rating | id, .) %>%
         summary())$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  filter(Feature == "rating") %>%
  select(Feature, Estimate, `Std. Error`, `Pr(>|t|)`) %>%
  dplyr::rename(p = `Pr(>|t|)`,
                SE = `Std. Error`) %>%
  mutate(method = "FE")

es_ols <- (res %>%
             filter(et >= 0 & et <= 12) %>%
             mutate(et = as.factor(et)) %>%
             felm(delta ~ 0 + rating + et + et:rating, .) %>%
             summary())$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  select(Feature, Estimate, `Std. Error`, `Pr(>|t|)`) %>%
  dplyr::rename(p = `Pr(>|t|)`,
                SE = `Std. Error`) %>%
  mutate(et = gsub("[^0-9]", "", Feature)) %>%
  mutate(method = "ES_OLS")

es_fe <- (res %>%
            filter(et >= 0 & et <= 12) %>%
            mutate(et = as.factor(et)) %>%
            felm(delta ~ 0 + rating + et + et:rating | id, .) %>%
            summary())$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  select(Feature, Estimate, `Std. Error`, `Pr(>|t|)`) %>%
  dplyr::rename(p = `Pr(>|t|)`,
                SE = `Std. Error`) %>%
  mutate(et = gsub("[^0-9]", "", Feature)) %>%
  mutate(method = "ES_FE") 

df <- plyr::rbind.fill(ols, fe, es_ols, es_fe)
df <- df %>%
  mutate(sig = ifelse(p <= 0.01, "*", ""))

dbWriteTable(iaan, "MODEL.BSTS_HET_BY_RATING", df, overwrite = T)
rm(df, es_fe, es_ols, fe, ols, res)
  
#Placebo------------------------------------------------------------------------
plac_sample <- dbReadTable(iaan, "DATA.BSTS_SAMPLE") %>%
  filter(id %in% (dbReadTable(iaan, "MODEL.BSTS_IND")$id %>% unique()))

lapply(unique(plac_sample$id), function(cid){
  dsubsample <- plac_sample %>%
    filter(id == cid) %>%
    mutate(neg = event_time2 <= -1) %>%
    mutate(neg = sum(neg)) %>%
    arrange(event_time2) %>%
    select(-id) %>%
    na.omit()
  
  plac_shift <- 3
  
  dsubsample <- dsubsample %>%
    mutate(plac_event_time = event_time2 + plac_shift) %>%
    mutate(plac_neg = plac_event_time <= -1) %>%
    mutate(plac_neg = sum(plac_neg)) %>%
    arrange(plac_event_time)
  
  pre.period <- c(1, unique(dsubsample$plac_neg))
  post.period <- c(unique(dsubsample$plac_neg) + 1, dim(dsubsample)[1])
  
  impact <- try(
    CausalImpact(dsubsample 
                 %>% select(-neg, -event_time2, -plac_neg, -plac_event_time),
                 pre.period = pre.period,
                 post.period = post.period,
                 model.args = list(nseasons = 4,
                                   season.duration = 3))
  )
  
  if("try-error" %in% class(impact)){
    print(paste0("Estimation not successful for ", cid))
    return()
  } else {
    
    impactdf <- impact$series %>%
      as.data.frame() %>%
      select(response, point.pred, point.pred.lower, point.pred.upper,
             point.effect, point.effect.lower, point.effect.upper) %>%
      cbind(
        dsubsample$event_time2,
        dsubsample$r
      ) %>%
      dplyr::rename(
        observed = response,
        fitted = point.pred,
        fitted_lo = point.pred.lower,
        fitted_hi = point.pred.upper,
        delta = point.effect,
        delta_lo = point.effect.lower,
        delta_hi = point.effect.upper
      ) %>%
      dplyr::rename(
        et = `dsubsample$event_time2`,
        rating = `dsubsample$r`,
      ) %>%
      mutate(id = cid,
             n = dim(dsubsample)[1])
    
    dbWriteTable(iaan, "MODEL.BSTS_IND_PLAC", impactdf, append = T)
    print(paste0("Success for ", cid))
    
  }
}
)

rm(dsubsample,cid, plac_shift, post.period, pre.period, cid, impact, impactdf,
   plac_sample)
gc()

#Evidence on Assumptions--------------------------------------------------------
vars <- c("bathrooms", "bedrooms", "capacity", "covid", "AC7", "AC51", "AC72",
          "capacity")
res <- list()
for(i in 1:length(vars)){
  form <- paste0("real_price ~ 0 + ", vars[i], "+", paste0("I(", vars[i], "*", "Y", 1:9, ")",
                                         collapse = " + "), "+",
                 paste0("Y", 1:9, collapse = " + "))
  res[[i]] <- summary(lm(form, data = brodSample))$coefficients
}

res <- lapply(res, function(r){
    (r %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      filter(grepl("\\*", Feature)) %>%
      select(Estimate))$Estimate %>% range()
  })

rm(res, vars, form, i)

#Varian Method - Nominal Price--------------------------------------------------
compVarianEst(nominal = T, iter = 100)
