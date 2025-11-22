#General Ban Wrangling

#Setup--------------------------------------------------------------------------
ban_cfs <- dbGetQuery(
  results2, 
  "SELECT * FROM counterfactuals WHERE cfid = 10"
) %>%
  select(-sell_comp, -buy_comp, -sell_brands) %>%
  inner_join(
    sdf2 %>% 
      select(cdid, market, brand, delta, inShare_hat_approx, marginal_cost,
             category, tob, lo, ul) %>% 
      distinct(), by = c("cdid", "brand")
  ) %>%
  inner_join(
    dbReadTable(main, "market_size") %>%
      select(marketid, market_size_real) %>%
      dplyr::rename(market = marketid),
    by = "market") %>%
  mutate(
    rev = price*((inShare_hat_approx*market_size_real)/price),
    cost = marginal_cost*((inShare_hat_approx*market_size_real)/price),
    profit = rev - cost,
  ) %>%
  mutate(delta_cf = delta - 
           price*(theta1_bar %>% 
                    filter(variable == "price") %>%
                    select(coef))$coef +
           cfprice*(theta1_bar %>% 
                      filter(variable == "price") %>%
                      select(coef))$coef) %>%
  group_by(cdid, cfid, tax) %>%
  dplyr::mutate(delta_sum_cf = sum(exp(delta_cf))) %>%
  dplyr::mutate(inShare_hat_approx_cf = (exp(delta_cf)/(1 + delta_sum_cf))) %>%
  select(-delta_sum_cf) %>%
  ungroup() %>% 
  mutate(
    gov_rev = tax*((inShare_hat_approx_cf*market_size_real)/cfprice),
    rev_cf = inShare_hat_approx_cf*cfprice*market_size_real,
    marginal_cost_cf = case_when(
      cfid == 2 ~ marginal_cost,
      .default = marginal_cost + tax
    )
  )

#Solve for MC-------------------------------------------------------------------
if("ban_markup_cf2" %in% dbListTables(results2)){
  ban_cfs <- dbReadTable(results2, "ban_markup_cf2")
} else {
  
  temp <- ban_cfs %>%
    select(brand, cdid, cfprice, delta_cf, banned_brand) %>%
    mutate(cf = 1) %>%
    group_by(banned_brand) %>%
    dplyr::mutate(ban_cf_group = cur_group_id()) %>%
    arrange(ban_cf_group) %>%
    mutate(cf_markup = NA)
  
  unique_groups <- unique(temp$ban_cf_group)
  
  for(i in 1:max(unique_groups)){
    
    tmp <-  data.frame(
      cdid = sdf2$cdid,
      brand = sdf2$brand,
      price = sdf2$price,
      delta = sdf2$delta
    ) %>%
      left_join(
        temp %>% filter(ban_cf_group == i), by = c("brand", "cdid")
      ) %>%
      mutate(price = ifelse(is.na(cf), price, cfprice),
             delta = ifelse(is.na(cf), delta, delta_cf)) %>%
      select(-cfprice, -delta_cf) %>%
      distinct()
    
    tmp_blp <- update_BLP_data(tmp %>% 
                                 select(-cf,
                                        -banned_brand,
                                        -cf_markup), sdf2_blp)
    
    tmpShareObj <- getShareInfo(tmp_blp, theta2_bar_alt)
    rm(tmp_blp)
    gc()
    
    banned_brand <- temp %>% filter(ban_cf_group == i) %>% pull(banned_brand) %>% unique()
    
    markup_list <- list()
    sh <- tmpShareObj$shares
    cf_markets <- temp$cdid %>% unique()
    c_ownership <- ownership
    
    for(j in 1:length(cf_markets)){
      
      loop_data <- tmp %>% filter(cdid == cf_markets[j]) %>%
        arrange(brand)
      
      brands <- unique(loop_data$brand)
      share_i <- sh[grepl(paste0("\\_", cf_markets[j], "$"), names(sh))]
      prices_pre_i <- loop_data$price
      scalar_i <- matrix(1 / share_i) %*% matrix(prices_pre_i, nrow = 1)
      elas_i <- get_elasticities(
        sdf2_blp,
        sdf2_shareObj,
        (theta1_bar %>%
           filter(variable == "price"))$coef,
        "price",
        market = cf_markets[j]
      )
      derivatives_i <- elas_i / scalar_i
      owner_i <- c_ownership %>% ungroup() %>%
        filter(cdid == cf_markets[j]) %>%
        filter(brand %in% brands) %>%
        select(-cdid, -brand) %>%
        as.matrix() 
      owner_i <- owner_i %*% t(owner_i)
      markups <- try(((solve(derivatives_i * owner_i) %*% share_i))*-1)
      
      if("try-error" %in% class(markups)){
        temp <- temp %>%
          left_join(
            data.frame(
              cdid = loop_data$cdid,
              brand = loop_data$brand,
              ma_cf_group = loop_data$ma_cf_group,
              tmp_markup = NA
            ),
            by = c("cdid", "brand", "ma_cf_group")
          ) %>%
          mutate(cf_markup = ifelse(is.na(tmp_markup), cf_markup, tmp_markup)) %>%
          select(-tmp_markup)
        
        print(j)
        
      } else {
        
        markups <- markups %>%
          as.data.frame() %>%
          rownames_to_column("brand") %>%
          dplyr::rename(tmp_markup = V1) %>%
          mutate(cdid = loop_data$cdid,
                 ban_cf_group = loop_data$ban_cf_group)
        
        temp <- temp %>%
          left_join(markups, by = c("cdid", "brand", "ban_cf_group")) %>%
          mutate(cf_markup = ifelse(is.na(tmp_markup), cf_markup, tmp_markup)) %>%
          select(-tmp_markup)
        
        print(j)
        
      }
      
      print(i)
      
    }
    
  }
  
}

# Finish up---------------------------------------------------------------------
ban_cfs <- ban_cfs %>%
  left_join(
    temp %>%
      select(cdid, brand, banned_brand, ban_cf_group,
             cf_markup) %>% distinct(),
    by = c("cdid", "brand", "banned_brand")
  ) %>%
  mutate(marginal_cost_cf = cfprice - cf_markup) %>% 
  mutate(markup = price - marginal_cost) %>%
  mutate(
    welfare = exp(delta),
    welfare_cf = exp(delta_cf)
  ) %>%
  group_by(cdid, ban_cf_group, banned_brand) %>%
  dplyr::mutate(welfare = sum(welfare),
                welfare_cf = sum(welfare_cf)) %>%
  dplyr::mutate(welfare = log(welfare+1),
                welfare_cf = log(welfare_cf+1)) %>%
  dplyr::mutate(a = (theta1_bar %>% 
                       filter(variable == "price") %>%
                       select(coef))$coef) %>%
  dplyr::mutate(welfare = (-1/a)*welfare,
                welfare_cf = (-1/a)*welfare_cf) %>%
  ungroup() %>%
  select(market_size_real, cdid, brand, banned_brand,
         price, cfprice,
         delta, delta_cf,
         inShare_hat_approx, inShare_hat_approx_cf,
         marginal_cost_cf, marginal_cost,
         markup, cf_markup,
         welfare, welfare_cf,
         rev, cost, profit,
         ban_cf_group) %>%
  mutate(
    brand_is_banned = brand %in% banned_brand
  ) %>%
  distinct() %>%
ungroup() %>%
  group_by(cdid, ban_cf_group) %>%
  dplyr::mutate(delta_sum_cf = sum(exp(delta_cf))) %>%
  dplyr::mutate(inShare_hat_approx_cf = (exp(delta_cf)/(1 + delta_sum_cf))) %>%
  select(-delta_sum_cf) %>%
  mutate(rev_cf = cfprice*((inShare_hat_approx_cf*market_size_real)/cfprice),
         cost_cf = marginal_cost_cf*((inShare_hat_approx_cf*market_size_real)/cfprice),
         profit_cf = rev_cf - cost_cf) 

dbWriteTable(results2, "random_ban_counterfactuals", ban_cfs)
