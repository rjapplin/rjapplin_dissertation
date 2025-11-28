# Get Sample--------------------------------------------------------------------
if("DATA.IV_SAMPLE" %in% dbListTables(iaan)) {
  iv_sample <- dbReadTable(iaan, "DATA.IV_SAMPLE")
} else {

  iv_sample <- dbGetQuery(iaan, "SELECT * FROM [DATA.ANALYSIS_SAMPLE_2] 
                                    WHERE (event_time2 >= -151 AND
                                    event_time2 <= 121) OR
                                    (treatment_group = 'Never Treated')")
  iv_sample <- iv_sample %>%
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
  
  iv_sample <- iv_sample %>%
    dplyr::group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
    dplyr::mutate(id = cur_group_id()) %>%
    dplyr::group_by(id) %>%
    select(-KEY_LIST_ID, -KEY_HOST_ID)
  
  iv_sample <- iv_sample %>%
    arrange(year, month) %>%
    group_by(year, month) %>%
    mutate(period = cur_group_id()) %>%
    group_by(id) %>%
    dplyr::mutate(G = ifelse(event_time2 == 0, period, NA)) %>%
    dplyr::mutate(G = min(G, na.rm = T)) %>%
    dplyr::mutate(G = ifelse(treatment_group == "Never Treated", 0, G)) %>%
    dplyr::mutate(G = ifelse(is.infinite(G), 0, G))
  
  dbWriteTable(iaan, "DATA.IV_SAMPLE", iv_sample, overwrite = T)
  
}

# Function----------------------------------------------------------------------
parse_k_terms <- function(coef_names, zname){
  # 1) keep only interaction terms that contain '#'
  idx <- grepl("#", coef_names, fixed = TRUE)
  if (!any(idx)) {
    return(data.frame(name = character(0), k = integer(0), stringsAsFactors = FALSE))
  }
  tmp <- coef_names[idx]
  
  # 2) variable name is everything AFTER '#'
  rhs <- sub(".*#", "", tmp)
  
  # 3) strip quoting/prefixes that fixest may add
  rhs <- gsub("^`|`$", "", rhs)   # remove backticks if present
  rhs <- gsub("^c\\.", "", rhs)   # remove a possible 'c.' prefix (rare)
  
  # 4) keep only the terms tied to the instrument we care about
  keep <- rhs == zname
  tmp <- tmp[keep]
  if (!length(tmp)) {
    return(data.frame(name = character(0), k = integer(0), stringsAsFactors = FALSE))
  }
  
  # 5) extract the event-time integer sitting between '::' and '#'
  #    works for patterns like '...::-3#Z_T1' or '...::(0)#`Z_T1`'
  k_str <- sub(".*::\\(?(-?\\d+)\\)?#.*", "\\1", tmp)
  
  data.frame(name = tmp, k = as.integer(k_str), stringsAsFactors = FALSE)
}

eventstudy_iv_pretrends <- function(
    df,
    id,                  # bare column name for unit id
    time,                # bare column name for time (numeric or ordered)
    y,                   # bare column name for outcome
    D,                   # bare column name for treatment (binary 0/1)
    Z,                   # bare column name for continuous instrument
    controls = NULL,     # character vector, e.g. c("x1","x2")
    thresh = 0,          # exposure threshold for Z (first |Z| > thresh defines cohort)
    k_lead = 5,          # max leads to include (must be >= 1)
    k_lag  = 5,          # max lags to include
    cluster = c("id"),   # "id", "time", or c("id","time")
    drop_never_exposed = TRUE # drop units that never cross the exposure threshold
){
  # ---- capture bare column names ----
  id   <- rlang::ensym(id)
  time <- rlang::ensym(time)
  y    <- rlang::ensym(y)
  D    <- rlang::ensym(D)
  Z    <- rlang::ensym(Z)
  z_name <- rlang::as_name(Z)
  
  if (k_lead < 1) stop("k_lead must be >= 1 so that '-1' exists as a lead.")
  
  # ---- prep: numeric time and cohort/event time ----
  df <- df %>%
    mutate(`.time_num` = as.numeric(!!time)) %>%
    arrange(!!id, `.time_num`) %>%
    group_by(!!id) %>%
    dplyr::mutate(
      `.exposed_flag` = abs(!!Z) > thresh,
      `.E` = ifelse(any(`.exposed_flag`), `.time_num`[which(`.exposed_flag`)[1]], NA_real_),
      `.k` = ceiling((`.time_num` - `.E`)/30)
    ) %>%
    ungroup()
  
  if (isTRUE(drop_never_exposed)) {
    df <- df %>% filter(!is.na(.E))
  }
  
  # ---- keep an event-time window that INCLUDES k == -1 ----
  df <- df %>%
    filter(!is.na(.k), .k >= -k_lead, .k <= k_lag) %>%
    mutate(.k = as.integer(.k))
  
  # ---- factor with explicit string levels including "-1"; ref must be a STRING ----
  all_levels <- as.character(seq(-k_lead, k_lag))
  df <- df %>%
    mutate(.k_fac = factor(as.character(.k), levels = all_levels))
  
  # internal sanity checks
  if (!any(df$.k == -1)) {
    stop("k == -1 not present in the data used. Increase k_lead or verify event-time construction.")
  }
  if (!any(levels(df$.k_fac) == "-1")) {
    stop("'-1' not present among .k_fac levels. This should not happen; check k_lead and level construction.")
  }
  
  # ---- build RHS term dynamically: i(.k_fac, <Z>, ref = "-1") ----
  iv_term <- paste0("i(.k_fac, ", z_name, ", ref = \"-1\")")
  
  # ---- controls (as character names) ----
  controls_str <- if (is.null(controls) || length(controls) == 0) {
    ""
  } else {
    paste(controls, collapse = " + ")
  }
  
  # ---- FE part ----
  fe_part <- paste(rlang::as_name(id), rlang::as_name(time), sep = " + ")
  
  # ---- formulas ----
  rhs_with_controls <- paste(
    iv_term,
    if (nzchar(controls_str)) paste("+", controls_str) else "",
    sep = " "
  )
  fstage_formula  <- as.formula(paste(rlang::as_name(D), "~", rhs_with_controls, "|", fe_part))
  redform_formula <- as.formula(paste(rlang::as_name(y), "~", rhs_with_controls, "|", fe_part))
  
  # ---- clustering ----
  clust_vec <- switch(
    length(cluster),
    `0` = NULL,
    `1` = cluster,
    `2` = paste0(cluster, collapse = " + ")
  )
  
  # ---- fit models ----
  fs_fit <- feols(fstage_formula,  data = df, cluster = clust_vec)
  rf_fit <- feols(redform_formula, data = df, cluster = clust_vec)
  
  # ---- helper to parse event-time k from fixest coefficient names ----
  parse_k_terms <- function(coef_names, zname){
    # Keep dose-by-event terms; they end with '#<zname>'
    keep <- grepl(paste0("#", zname, "$"), coef_names)
    names_kept <- coef_names[keep]
    # Extract the event-time label that fixest encodes after '::'
    # Examples: 'i(.k_fac, Z, ref = "-1")::-3#Z' or '.k_fac::-3#Z'
    k_str <- gsub(".*::\\(?(-?\\d+)\\)?#", "\\1", names_kept)
    suppressWarnings(
      data.frame(name = names_kept, k = as.integer(k_str), stringsAsFactors = FALSE)
    )
  }
  
  coef_names_fs <- names(coef(fs_fit))
  coef_names_rf <- names(coef(rf_fit))
  fs_terms <- parse_k_terms(coef_names_fs, z_name)
  rf_terms <- parse_k_terms(coef_names_rf, z_name)
  
  lead_fs <- dplyr::filter(fs_terms, k < 0)
  lead_rf <- dplyr::filter(rf_terms, k < 0)
  
  # ---- joint Wald tests: all pre-leads = 0 ----
  wald_or_null <- function(fit, terms_df){
    if (nrow(terms_df) == 0) return(NULL)
    wald_str <- paste(paste0("`", terms_df$name, "` = 0"), collapse = " & ")
    wald(fit, wald_str)
  }
  fs_wald <- wald_or_null(fs_fit, lead_fs)
  rf_wald <- wald_or_null(rf_fit, lead_rf)
  
  # ---- tidy coeffs for plotting ----
  tidy_for_plot <- function(fit, zname){
    cn <- names(coef(fit))
    tm <- parse_k_terms(cn, zname)
    est <- coef(fit)[tm$name]
    V   <- vcov(fit)
    se  <- sqrt(diag(V))[tm$name]
    data.frame(k = tm$k, estimate = est, se = se,
               ci_lo = est - 1.96*se, ci_hi = est + 1.96*se)
  }
  fs_plot_df <- tidy_for_plot(fs_fit, z_name)
  rf_plot_df <- tidy_for_plot(rf_fit, z_name)
  
  p_fs <- ggplot(fs_plot_df, aes(x = k, y = estimate)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point() +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.1) +
    labs(title = "First stage: dose-by-event-time effects of Z on D",
         subtitle = "Pretrend test: all k<0 equal 0",
         x = "Event time k (relative to first exposure)", y = paste0("ΔD per 1-unit ", z_name))
  
  p_rf <- ggplot(rf_plot_df, aes(x = k, y = estimate)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point() +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.1) +
    labs(title = "Reduced form: dose-by-event-time effects of Z on Y",
         subtitle = "Pretrend test: all k<0 equal 0",
         x = "Event time k (relative to first exposure)", y = paste0("ΔY per 1-unit ", z_name))
  
  list(
    data_used = df,
    first_stage = fs_fit,
    reduced_form = rf_fit,
    wald_pretrends_first_stage = fs_wald,
    wald_pretrends_reduced_form = rf_wald,
    plot_first_stage = p_fs,
    plot_reduced_form = p_rf
  )
}

# Estimation--------------------------------------------------------------------

  
iv_sample <- iv_sample %>% mutate(time = as.Date(paste0(year, "-", month, "-01")))


# Host Response Rate------------------------------------------------------------
  
thresh_vec <- seq(0.2, 0.9, 0.2)
results <- list()

for(i in 1:length(thresh_vec)){
  eventstudy_iv_pretrends(
    iv_sample,
    id = id,
    time = time,
    y = real_price,
    D = treated_after,
    Z = host_response_rate,
    controls = c("real_price1", "real_price2", "real_price3", "bathrooms",
                 "bedrooms", "capacity", "covid", "host_is_superhost",
                 "AC51", "AC72", "AC72", "as.factor(listing_type)",
                 "as.factor(room_type)"),
    thres = thresh_vec[i],
    k_lead=5,
    k_lag=5,
    cluster=c("KEY_LIST_ID")
  ) -> x
  results[[i]] <- x
}

fs <- list()
ss <- list()
for(i in 1:length(results)){
  fs[[i]] <- results[[i]]$first_stage$coeftable %>%
    rownames_to_column("feature") %>%
    filter(grepl("host_response_rate", feature)) %>%
    mutate(et = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5)) %>%
    mutate(thresh = thresh_vec[i]) %>%
    mutate(stage="first_stage") %>%
    mutate(feature = sub(".*?[0-9]:", "", feature))
  ss[[i]] <- results[[i]]$reduced_form$coeftable  %>%
    rownames_to_column("feature") %>%
    filter(grepl("host_response_rate", feature)) %>%
    mutate(et = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5)) %>%
    mutate(thresh = thresh_vec[i]) %>%
    mutate(stage="second_stage") %>%
    mutate(feature = sub(".*?[0-9]:", "", feature))
}

fs <- fs %>%
  Reduce(rbind, .)
ss <- ss %>%
  Reduce(rbind, .)
final <- rbind(fs, ss)

dbWriteTable(iaan, "MODEL.iv_pretrend_tests", final)

# Host Accept Rate--------------------------------------------------------------
thresh_vec <- seq(0.2, 0.9, 0.2)
results <- list()

for(i in 1:length(thresh_vec)){
  eventstudy_iv_pretrends(
    iv_sample,
    id = id,
    time = time,
    y = real_price,
    D = treated_after,
    Z = host_accept_rate,
    controls = c("real_price1", "real_price2", "real_price3", "bathrooms",
                 "bedrooms", "capacity", "covid", "host_is_superhost",
                 "AC51", "AC72", "AC72", "as.factor(listing_type)",
                 "as.factor(room_type)"),
    thres = thresh_vec[i],
    k_lead=5,
    k_lag=5,
    cluster=c("KEY_LIST_ID")
  ) -> x
  results[[i]] <- x
}

fs <- list()
ss <- list()
for(i in 1:length(results)){
  fs[[i]] <- results[[i]]$first_stage$coeftable %>%
    rownames_to_column("feature") %>%
    filter(grepl("host_accept_rate", feature)) %>%
    mutate(et = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5)) %>%
    mutate(thresh = thresh_vec[i]) %>%
    mutate(stage="first_stage") %>%
    mutate(feature = sub(".*?[0-9]:", "", feature))
  ss[[i]] <- results[[i]]$reduced_form$coeftable  %>%
    rownames_to_column("feature") %>%
    filter(grepl("host_accept_rate", feature)) %>%
    mutate(et = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5)) %>%
    mutate(thresh = thresh_vec[i]) %>%
    mutate(stage="second_stage") %>%
    mutate(feature = sub(".*?[0-9]:", "", feature))
}

fs <- fs %>%
  Reduce(rbind, .)
ss <- ss %>%
  Reduce(rbind, .)
final <- rbind(fs, ss)

dbWriteTable(iaan, "MODEL.iv_pretrend_tests", final, append = T)

# Host About Word Count---------------------------------------------------------
thresh_vec <- summary(iv_sample$host_about_word_count)[3:4] %>% as.vector()
results <- list()

for(i in 1:length(thresh_vec)){
  eventstudy_iv_pretrends(
    iv_sample,
    id = id,
    time = time,
    y = real_price,
    D = treated_after,
    Z = host_about_word_count,
    controls = c("real_price1", "real_price2", "real_price3", "bathrooms",
                 "bedrooms", "capacity", "covid", "host_is_superhost",
                 "AC51", "AC72", "AC72", "as.factor(listing_type)",
                 "as.factor(room_type)"),
    thres = thresh_vec[i],
    k_lead=5,
    k_lag=5,
    cluster=c("KEY_LIST_ID")
  ) -> x
  results[[i]] <- x
}

fs <- list()
ss <- list()
for(i in 1:length(results)){
  fs[[i]] <- results[[i]]$first_stage$coeftable %>%
    rownames_to_column("feature") %>%
    filter(grepl("host_about_word_count", feature)) %>%
    mutate(et = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5)) %>%
    mutate(thresh = thresh_vec[i]) %>%
    mutate(stage="first_stage") %>%
    mutate(feature = sub(".*?[0-9]:", "", feature))
  ss[[i]] <- results[[i]]$reduced_form$coeftable  %>%
    rownames_to_column("feature") %>%
    filter(grepl("host_about_word_count", feature)) %>%
    mutate(et = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5)) %>%
    mutate(thresh = thresh_vec[i]) %>%
    mutate(stage="second_stage") %>%
    mutate(feature = sub(".*?[0-9]:", "", feature))
}

fs <- fs %>%
  Reduce(rbind, .)
ss <- ss %>%
  Reduce(rbind, .)
final <- rbind(fs, ss)

dbWriteTable(iaan, "MODEL.iv_pretrend_tests", final, append = T)

# Amenity Word Count------------------------------------------------------------
thresh_vec <- summary(iv_sample$amenity_word_count)[3:4] %>% as.vector()
results <- list()

for(i in 1:length(thresh_vec)){
  eventstudy_iv_pretrends(
    iv_sample,
    id = id,
    time = time,
    y = real_price,
    D = treated_after,
    Z = amenity_word_count,
    controls = c("real_price1", "real_price2", "real_price3", "bathrooms",
                 "bedrooms", "capacity", "covid", "host_is_superhost",
                 "AC51", "AC72", "AC72", "as.factor(listing_type)",
                 "as.factor(room_type)"),
    thres = thresh_vec[i],
    k_lead=5,
    k_lag=5,
    cluster=c("KEY_LIST_ID")
  ) -> x
  results[[i]] <- x
}

fs <- list()
ss <- list()
for(i in 1:length(results)){
  fs[[i]] <- results[[i]]$first_stage$coeftable %>%
    rownames_to_column("feature") %>%
    filter(grepl("amenity_word_count", feature)) %>%
    mutate(et = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5)) %>%
    mutate(thresh = thresh_vec[i]) %>%
    mutate(stage="first_stage") %>%
    mutate(feature = sub(".*?[0-9]:", "", feature))
  ss[[i]] <- results[[i]]$reduced_form$coeftable  %>%
    rownames_to_column("feature") %>%
    filter(grepl("amenity_word_count", feature)) %>%
    mutate(et = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5)) %>%
    mutate(thresh = thresh_vec[i]) %>%
    mutate(stage="second_stage") %>%
    mutate(feature = sub(".*?[0-9]:", "", feature))
}

fs <- fs %>%
  Reduce(rbind, .)
ss <- ss %>%
  Reduce(rbind, .)
final <- rbind(fs, ss)

dbWriteTable(iaan, "MODEL.iv_pretrend_tests", final, append = T)

# Descrip Word Count------------------------------------------------------------
thresh_vec <- summary(iv_sample$descrip_word_count)[3:4] %>% as.vector()
results <- list()

for(i in 1:length(thresh_vec)){
  eventstudy_iv_pretrends(
    iv_sample,
    id = id,
    time = time,
    y = real_price,
    D = treated_after,
    Z = descrip_word_count,
    controls = c("real_price1", "real_price2", "real_price3", "bathrooms",
                 "bedrooms", "capacity", "covid", "host_is_superhost",
                 "AC51", "AC72", "AC72", "as.factor(listing_type)",
                 "as.factor(room_type)"),
    thres = thresh_vec[i],
    k_lead=5,
    k_lag=5,
    cluster=c("KEY_LIST_ID")
  ) -> x
  results[[i]] <- x
}

fs <- list()
ss <- list()
for(i in 1:length(results)){
  fs[[i]] <- results[[i]]$first_stage$coeftable %>%
    rownames_to_column("feature") %>%
    filter(grepl("descrip_word_count", feature)) %>%
    mutate(et = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5)) %>%
    mutate(thresh = thresh_vec[i]) %>%
    mutate(stage="first_stage") %>%
    mutate(feature = sub(".*?[0-9]:", "", feature))
  ss[[i]] <- results[[i]]$reduced_form$coeftable  %>%
    rownames_to_column("feature") %>%
    filter(grepl("descrip_word_count", feature)) %>%
    mutate(et = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5)) %>%
    mutate(thresh = thresh_vec[i]) %>%
    mutate(stage="second_stage") %>%
    mutate(feature = sub(".*?[0-9]:", "", feature))
}

fs <- fs %>%
  Reduce(rbind, .)
ss <- ss %>%
  Reduce(rbind, .)
final <- rbind(fs, ss)

dbWriteTable(iaan, "MODEL.iv_pretrend_tests", final, append = T)

# Loc Word Count----------------------------------------------------------------
thresh_vec <- summary(iv_sample$loc_overview_word_count)[3:4] %>% as.vector()
thresh_vec <- thresh_vec + c(1,0)
results <- list()

for(i in 1:length(thresh_vec)){
  eventstudy_iv_pretrends(
    iv_sample,
    id = id,
    time = time,
    y = real_price,
    D = treated_after,
    Z = loc_overview_word_count,
    controls = c("real_price1", "real_price2", "real_price3", "bathrooms",
                 "bedrooms", "capacity", "covid", "host_is_superhost",
                 "AC51", "AC72", "AC72", "as.factor(listing_type)",
                 "as.factor(room_type)"),
    thres = thresh_vec[i],
    k_lead=5,
    k_lag=5,
    cluster=c("KEY_LIST_ID")
  ) -> x
  results[[i]] <- x
}

fs <- list()
ss <- list()
for(i in 1:length(results)){
  fs[[i]] <- results[[i]]$first_stage$coeftable %>%
    rownames_to_column("feature") %>%
    filter(grepl("loc_overview_word_count", feature)) %>%
    mutate(et = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5)) %>%
    mutate(thresh = thresh_vec[i]) %>%
    mutate(stage="first_stage") %>%
    mutate(feature = sub(".*?[0-9]:", "", feature))
  ss[[i]] <- results[[i]]$reduced_form$coeftable  %>%
    rownames_to_column("feature") %>%
    filter(grepl("loc_overview_word_count", feature)) %>%
    mutate(et = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5)) %>%
    mutate(thresh = thresh_vec[i]) %>%
    mutate(stage="second_stage") %>%
    mutate(feature = sub(".*?[0-9]:", "", feature))
}

fs <- fs %>%
  Reduce(rbind, .)
ss <- ss %>%
  Reduce(rbind, .)
final <- rbind(fs, ss)

dbWriteTable(iaan, "MODEL.iv_pretrend_tests", final, append = T)

# Host Verified-----------------------------------------------------------------
df = iv_sample %>%
  group_by(id) %>%
  mutate(
    t0 = {
      t_first <- min(period[host_verified==1], na.rm = TRUE)
      if (is.infinite(t_first)) NA else t_first
    },
    event_time = period - t0
  ) %>%
  filter(event_time >= -5 & event_time <= 5)

df = df %>%
  mutate(
    event_time = as.factor(event_time)
  ) %>%
  mutate(event_time = relevel(event_time, ref='-1'))

fs <- summary(felm(
  treated_after ~ event_time + real_price1 + real_price2 + real_price3 +
    bedrooms + capacity + covid + host_is_superhost +
    AC51 + AC7 + AC72 + as.factor(listing_type) +
    as.factor(room_type) | id + period, data=df
))$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  filter(grepl("event_time", feature)) %>%
  arrange(feature) %>%
  mutate(et = c(-2, -3, -4, -5, 0, 1, 2, 3, 4, 5)) %>%
  mutate(thresh = 0, stage = "first_stage") %>%
  mutate(feature = "host_verified")

ss <- summary(felm(
  real_price ~ event_time + real_price1 + real_price2 + real_price3 +
    bedrooms + capacity + covid + host_is_superhost +
    AC51 + AC7 + AC72 + as.factor(listing_type) +
    as.factor(room_type) | id + period, data=df
))$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  filter(grepl("event_time", feature)) %>%
  arrange(feature) %>%
  mutate(et = c(-2, -3, -4, -5, 0, 1, 2, 3, 4, 5)) %>%
  mutate(thresh = 0, stage = "second_stage") %>%
  mutate(feature = "host_verified")

final <- rbind(fs, ss)

dbWriteTable(iaan, "MODEL.iv_pretrend_tests", final, append = T)

# Host Has Pic------------------------------------------------------------------
df = iv_sample %>%
  group_by(id) %>%
  mutate(
    t0 = {
      t_first <- min(period[host_has_pic==1], na.rm = TRUE)
      if (is.infinite(t_first)) NA else t_first
    },
    event_time = period - t0
  ) %>%
  filter(event_time >= -5 & event_time <= 5)

df = df %>%
  mutate(
    event_time = as.factor(event_time)
  ) %>%
  mutate(event_time = relevel(event_time, ref='-1'))

fs <- summary(felm(
  treated_after ~ event_time + real_price1 + real_price2 + real_price3 +
    bedrooms + capacity + covid + host_is_superhost +
    AC51 + AC7 + AC72 + as.factor(listing_type) +
    as.factor(room_type) | id + period, data=df
))$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  filter(grepl("event_time", feature)) %>%
  arrange(feature) %>%
  mutate(et = c(-2, -3, -4, -5, 0, 1, 2, 3, 4, 5)) %>%
  mutate(thresh = 0, stage = "first_stage") %>%
  mutate(feature = "host_has_pic")

ss <- summary(felm(
  real_price ~ event_time + real_price1 + real_price2 + real_price3 +
    bedrooms + capacity + covid + host_is_superhost +
    AC51 + AC7 + AC72 + as.factor(listing_type) +
    as.factor(room_type) | id + period, data=df
))$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  filter(grepl("event_time", feature)) %>%
  arrange(feature) %>%
  mutate(et = c(-2, -3, -4, -5, 0, 1, 2, 3, 4, 5)) %>%
  mutate(thresh = 0, stage = "second_stage") %>%
  mutate(feature = "host_has_pic")

final <- rbind(fs, ss)

dbWriteTable(iaan, "MODEL.iv_pretrend_tests", final, append = T)

# Host Within a Few Hours-------------------------------------------------------
df = iv_sample %>%
  group_by(id) %>%
  mutate(
    t0 = {
      t_first <- min(period[host_response_time=="within an hour"], na.rm = TRUE)
      if (is.infinite(t_first)) NA else t_first
    },
    event_time = period - t0
  ) %>%
  filter(event_time >= -5 & event_time <= 5)

df = df %>%
  mutate(
    event_time = as.factor(event_time)
  ) %>%
  mutate(event_time = relevel(event_time, ref='-1'))

fs <- summary(felm(
  treated_after ~ event_time + real_price1 + real_price2 + real_price3 +
    bedrooms + capacity + covid + host_is_superhost +
    AC51 + AC7 + AC72 + as.factor(listing_type) +
    as.factor(room_type) | id + period, data=df
))$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  filter(grepl("event_time", feature)) %>%
  arrange(feature) %>%
  mutate(et = c(-2, -3, -4, -5, 0, 1, 2, 3, 4, 5)) %>%
  mutate(thresh = 0, stage = "first_stage") %>%
  mutate(feature = "quick_rt")

ss <- summary(felm(
  real_price ~ event_time + real_price1 + real_price2 + real_price3 +
    bedrooms + capacity + covid + host_is_superhost +
    AC51 + AC7 + AC72 + as.factor(listing_type) +
    as.factor(room_type) | id + period, data=df
))$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  filter(grepl("event_time", feature)) %>%
  arrange(feature) %>%
  mutate(et = c(-2, -3, -4, -5, 0, 1, 2, 3, 4, 5)) %>%
  mutate(thresh = 0, stage = "second_stage") %>%
  mutate(feature = "quick_rt")

final <- rbind(fs, ss)

dbWriteTable(iaan, "MODEL.iv_pretrend_tests", final, append = T)

# Host Somewhat Quick-----------------------------------------------------------
df = iv_sample %>%
  group_by(id) %>%
  mutate(
    t0 = {
      t_first <- min(period[host_response_time=="within a few hours"], na.rm = TRUE)
      if (is.infinite(t_first)) NA else t_first
    },
    event_time = period - t0
  ) %>%
  filter(event_time >= -5 & event_time <= 5)

df = df %>%
  mutate(
    event_time = as.factor(event_time)
  ) %>%
  mutate(event_time = relevel(event_time, ref='-1'))

fs <- summary(felm(
  treated_after ~ event_time + real_price1 + real_price2 + real_price3 +
    bedrooms + capacity + covid + host_is_superhost +
    AC51 + AC7 + AC72 + as.factor(listing_type) +
    as.factor(room_type) | id + period, data=df
))$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  filter(grepl("event_time", feature)) %>%
  arrange(feature) %>%
  mutate(et = c(-2, -3, -4, -5, 0, 1, 2, 3, 4, 5)) %>%
  mutate(thresh = 0, stage = "first_stage") %>%
  mutate(feature = "somewhat_quick_rt")

ss <- summary(felm(
  real_price ~ event_time + real_price1 + real_price2 + real_price3 +
    bedrooms + capacity + covid + host_is_superhost +
    AC51 + AC7 + AC72 + as.factor(listing_type) +
    as.factor(room_type) | id + period, data=df
))$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  filter(grepl("event_time", feature)) %>%
  arrange(feature) %>%
  mutate(et = c(-2, -3, -4, -5, 0, 1, 2, 3, 4, 5)) %>%
  mutate(thresh = 0, stage = "second_stage") %>%
  mutate(feature = "somewhat_quick_rt")

final <- rbind(fs, ss)

dbWriteTable(iaan, "MODEL.iv_pretrend_tests", final, append = T)

# Host Slow---------------------------------------------------------------------
df = iv_sample %>%
  group_by(id) %>%
  mutate(
    t0 = {
      t_first <- min(period[host_response_time=="within a day"], na.rm = TRUE)
      if (is.infinite(t_first)) NA else t_first
    },
    event_time = period - t0
  ) %>%
  filter(event_time >= -5 & event_time <= 5)

df = df %>%
  mutate(
    event_time = as.factor(event_time)
  ) %>%
  mutate(event_time = relevel(event_time, ref='-1'))

fs <- summary(felm(
  treated_after ~ event_time + real_price1 + real_price2 + real_price3 +
    bedrooms + capacity + covid + host_is_superhost +
    AC51 + AC7 + AC72 + as.factor(listing_type) +
    as.factor(room_type) | id + period, data=df
))$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  filter(grepl("event_time", feature)) %>%
  arrange(feature) %>%
  mutate(et = c(-2, -3, -4, -5, 0, 1, 2, 3, 4, 5)) %>%
  mutate(thresh = 0, stage = "first_stage") %>%
  mutate(feature = "slow_rt")

ss <- summary(felm(
  real_price ~ event_time + real_price1 + real_price2 + real_price3 +
    bedrooms + capacity + covid + host_is_superhost +
    AC51 + AC7 + AC72 + as.factor(listing_type) +
    as.factor(room_type) | id + period, data=df
))$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  filter(grepl("event_time", feature)) %>%
  arrange(feature) %>%
  mutate(et = c(-2, -3, -4, -5, 0, 1, 2, 3, 4, 5)) %>%
  mutate(thresh = 0, stage = "second_stage") %>%
  mutate(feature = "slow_rt")

final <- rbind(fs, ss)

dbWriteTable(iaan, "MODEL.iv_pretrend_tests", final, append = T)

# Host Very Slow----------------------------------------------------------------
df = iv_sample %>%
  group_by(id) %>%
  mutate(
    t0 = {
      t_first <- min(period[host_response_time=="a few days or more"], na.rm = TRUE)
      if (is.infinite(t_first)) NA else t_first
    },
    event_time = period - t0
  ) %>%
  filter(event_time >= -5 & event_time <= 5)

df = df %>%
  mutate(
    event_time = as.factor(event_time)
  ) %>%
  mutate(event_time = relevel(event_time, ref='-1'))

fs <- summary(felm(
  treated_after ~ event_time + real_price1 + real_price2 + real_price3 +
    bedrooms + capacity + covid + host_is_superhost +
    AC51 + AC7 + AC72 + as.factor(listing_type) +
    as.factor(room_type) | id + period, data=df
))$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  filter(grepl("event_time", feature)) %>%
  arrange(feature) %>%
  mutate(et = c(-2, -3, -4, -5, 0, 1, 2, 3, 4, 5)) %>%
  mutate(thresh = 0, stage = "first_stage") %>%
  mutate(feature = "very_slow_rt")

ss <- summary(felm(
  real_price ~ event_time + real_price1 + real_price2 + real_price3 +
    bedrooms + capacity + covid + host_is_superhost +
    AC51 + AC7 + AC72 + as.factor(listing_type) +
    as.factor(room_type) | id + period, data=df
))$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  filter(grepl("event_time", feature)) %>%
  arrange(feature) %>%
  mutate(et = c(-2, -3, -4, -5, 0, 1, 2, 3, 4, 5)) %>%
  mutate(thresh = 0, stage = "second_stage") %>%
  mutate(feature = "very_slow_rt")

final <- rbind(fs, ss)

dbWriteTable(iaan, "MODEL.iv_pretrend_tests", final, append = T)
