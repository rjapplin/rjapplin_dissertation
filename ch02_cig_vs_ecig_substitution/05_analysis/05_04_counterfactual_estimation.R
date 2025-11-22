#Markets for Counterfactuals--------------------------------------------------
source("code/05_analysis/05_func_foc_bertrand_mkt.R")
market_id <- as.numeric(sdf2_blp$parameters$market_id_char_in)
markets_2020 <- (sdf2 %>%
                   filter(year == 2020) %>%
                   group_by(cdid) %>%
                   dplyr::mutate(min_cat = min(category)) %>%
                   filter(min_cat == 7460) %>%
                   ungroup() %>%
                   filter(!is.na(marginal_cost)))$cdid %>% unique()
markets_2020_sub <- sample(markets_2020, 100, F)

#Prepare Env
if(exists("elasticity_list") & exists("elasticity_list2")){
  save(elasticity_list, elasticity_list2, file = "code/05_analysis/elasticity_lists2.RData")
}
rm(df2, elasticity_list, elasticity_list2, markups)
gc()

#Prepare Result Table
dbExecute(
  results2,
  "CREATE TABLE IF NOT EXISTS counterfactuals (
    brand VARCHAR,
    cdid INT,
    price REAL,
    cfprice REAL,
    cfid INT,
    tax REAL
  )
  "
)

#Determine if Counterfactual Results Exist
doNotRun <- unique(dbReadTable(results2, "counterfactuals")$cfid)

#CF1: National $1 Tax Counterfactual on E-Cigs----------------------------------
if(1 %in% doNotRun){
  
} else {
  cfid = 1
  for (ms in 1:length(markets_2020_sub)) {
    k <- markets_2020_sub[ms]
    market_ind <- market_id == k
    temp <- sdf2 %>% filter(cdid ==k ) 
    brands <- unique(temp$brand)
    owner_i <- ownership %>% ungroup() %>%
      filter(cdid == k) %>%
      filter(brand %in% brands) %>%
      select(-cdid, -brand) %>%
      as.matrix() 
    owner_i <- owner_i %*% t(owner_i)
    price_pre_i <- temp$price
    marg_cost <- (temp %>%
                    mutate(marginal_cost = ifelse(category == 7467, marginal_cost,
                                                  marginal_cost + 1)))$marginal_cost
    
    solution <-try(nleqslv(
      x = price_pre_i, foc_bertrand_mkt, # startingguesses: price_pre_i
      own_prod = owner_i,
      blp_data = sdf2_blp,
      mkt = k,
      marg_cost = marg_cost,
      theta_lin = (theta1_bar %>%
                     filter(variable == "price"))$coef,
      theta_rc = theta2_bar_alt
    ))
    if(
      "try-error" %in% class(solution)
    ) {
      print(paste0("error with market ", k))
      gc()
    } else {
      
      temp %>%
        select(cdid, brand, price) %>%
        mutate(cfprice = solution$x,
               cfid = cfid) %>%
        dbWriteTable(results2,
                     "counterfactuals",
                     .,
                     append = T)
      
      print(ms)
      gc()
      
    }
    
  }
  
}

#CF2 Altria NJOY Aquisition-----------------------------------------------------
if(2 %in% doNotRun){
  
} else {
  c_ownership <- ownership %>%
    mutate(PMUSA = ifelse(brand == "NJOY", 1, PMUSA)) %>%
    select(-NJOY)
  cfid = 2
  
  for(ms in 1:length(markets_2020_sub)) {
    k <- markets_2020_sub[ms]
    market_ind <- market_id == k
    temp <- sdf2 %>% filter(cdid ==k ) 
    brands <- unique(temp$brand)
    owner_i <- c_ownership %>% ungroup() %>%
      filter(cdid == k) %>%
      filter(brand %in% brands) %>%
      select(-cdid, -brand) %>%
      as.matrix() 
    owner_i <- owner_i %*% t(owner_i)
    price_pre_i <- temp$price
    marg_cost <- temp$marginal_cost
    
    solution <-try(nleqslv(
      x = price_pre_i, foc_bertrand_mkt, # startingguesses: price_pre_i
      own_prod = owner_i,
      blp_data = sdf2_blp,
      mkt = k,
      marg_cost = marg_cost,
      theta_lin = (theta1_bar %>%
                     filter(variable == "price"))$coef,
      theta_rc = theta2_bar_alt
    ))
    if(
      "try-error" %in% class(solution)
    ) {
      print(paste0("error with market ", k))
      gc()
    } else {
      
      temp %>%
        select(cdid, brand, price) %>%
        mutate(cfprice = solution$x,
               cfid = cfid,
               tax = NA) %>%
        dbWriteTable(results2,
                     "counterfactuals",
                     .,
                     append = T)
      
      print(ms)
      gc()
      
    }
    
    
  }
  
}

#CF3 Border Tax-----------------------------------------------------------------
if(3 %in% doNotRun){
  
} else {
  
  cfid = 3
  
  if("bordering_states" %in% dbListTables(results)){
    sdf2 <- sdf2 %>%
      left_join(
        dbReadTable(results2, "bordering_states"),
        by = c("cdid")
      )
  } else {
    options(tigris_use_cache = TRUE)
    us_counties <- counties(cb = TRUE, resolution = "5m") %>%
      dplyr::rename(fips_code = GEOID)
    us_states <- states(cb = TRUE, resolution = "5m") %>%
      filter(!(NAME %in% c("Puerto Rico", "Guam", "American Samoa", "Alaska",
                           "United States Virgin Islands", 
                           "Commonwealth of the Northern Mariana Islands",
                           "Hawaii", "Alaska")))
    neighbors <- st_touches(us_counties, sparse = TRUE)
    bordering_states <- sapply(seq_along(neighbors), function(i) {
      # Get the state of the current county
      current_state <- us_counties$STATEFP[i]
      
      # Get the states of neighboring counties
      neighbor_states <- us_counties$STATEFP[neighbors[[i]]]
      
      # Check if any neighboring counties belong to a different state
      paste0(unique(neighbor_states[which(neighbor_states != current_state)]), collapse = ", ")
    })
    us_counties <- us_counties %>%
      mutate(bordering_states = bordering_states)
    
    sdf2 <- sdf2 %>%
      mutate(fips_code = sprintf("%02d%03d", fips_state_code, fips_county_code)) %>%
      left_join(us_counties %>% ungroup() %>% select(fips_code, bordering_states), 
                by = "fips_code") %>%
      select(-geometry) 
    
    sdf2 %>%
      select(cdid, bordering_states) %>%
      distinct() %>%
      dbWriteTable(results, "bordering_states", ., overwrite = T)
    
  }
  
  markets_2020 <- (sdf2 %>%
                     filter(year == 2020) %>%
                     group_by(cdid) %>%
                     dplyr::mutate(min_cat = min(category)) %>%
                     filter(min_cat == 7460) %>%
                     ungroup() %>%
                     filter(!is.na(marginal_cost)) %>%
                     mutate(inner_county = 
                              ifelse(bordering_states == "", 1, 0)))$cdid %>%
    unique()
  
  all_tn <- (sdf2 %>% 
               filter(fips_state_code == "47") %>%
               select(cdid, bordering_states) %>% 
               filter(cdid %in% markets_2020) %>%
               distinct())$cdid %>% unique()
  
  tn_border <- (sdf2 %>% 
                  filter(fips_state_code == "47") %>% 
                  select(bordering_states) %>% 
                  distinct() %>%
                  mutate() %>%
                  separate(bordering_states, paste0("bs_", 1:2), sep = ",") %>%
                  pivot_longer(c(bs_1, bs_2)) %>%
                  select(value) %>%
                  mutate(value = gsub(" ", "", value)) %>%
                  distinct() %>%
                  filter(!is.na(value) & value != ""))$value
  
  border_tn_states_all <- (sdf2 %>% 
                             filter(fips_state_code %in% tn_border) %>%
                             select(cdid, bordering_states) %>% 
                             filter(cdid %in% markets_2020) %>%
                             distinct())$cdid %>% unique()
  
  markets_to_analyze <- all_tn
  
  for (ms in 1:length(all_tn)) {
    k <- markets_to_analyze[ms]
    market_ind <- market_id == k
    temp <- sdf2 %>% filter(cdid ==k )
    brands <- unique(temp$brand)
    owner_i <- ownership %>% ungroup() %>%
      filter(cdid == k) %>%
      filter(brand %in% brands) %>%
      select(-cdid, -brand) %>%
      as.matrix() 
    owner_i <- owner_i %*% t(owner_i)
    price_pre_i <- temp$price
    marg_cost <- (temp %>%
                    mutate(is_tn = ifelse(fips_state_code == "47", 1, 0)) %>%
                    mutate(marginal_cost = ifelse(category == 7460, marginal_cost + 1, marginal_cost))
    )$marginal_cost
    
    solution <-try(nleqslv(
      x = price_pre_i, foc_bertrand_mkt, # startingguesses: price_pre_i
      own_prod = owner_i,
      blp_data = sdf2_blp,
      mkt = k,
      marg_cost = marg_cost,
      theta_lin = (theta1_bar %>%
                     filter(variable == "price"))$coef,
      theta_rc = theta2_bar_alt
    ))
    
    if("try-error" %in% class(solution)) {
      print(paste0("error with market ", k))
      gc()
    } else {
      
      temp %>%
        select(cdid, brand, price) %>%
        mutate(cfprice = solution$x,
               cfid = cfid) %>%
        dbWriteTable(results2,
                     "counterfactuals",
                     .,
                     append = T)
      
      print(ms)
      gc()
      
    }
    
  }
  
}




#CF4 Laffer Curve Analysis------------------------------------------------------
if(4 %in% doNotRun){
  
} else {
  cfid = 4
  tax_vec <- seq(0.1, 2, by = 0.1)
  markets_to_analyze <- unique(
    (dbReadTable(results2, "counterfactuals") %>% filter(cfid == 1))$cdid
  )
  for(t in tax_vec){
    
    for(ms in 1:length(markets_to_analyze)){
      
      if(markets_to_analyze[ms] != 4986){
        
      } else {
        
        k <- markets_to_analyze[ms]
        market_ind <- market_id == k
        temp <- sdf2 %>% filter(cdid ==k ) 
        brands <- unique(temp$brand)
        owner_i <- ownership %>% ungroup() %>%
          filter(cdid == k) %>%
          filter(brand %in% brands) %>%
          select(-cdid, -brand) %>%
          as.matrix() 
        owner_i <- owner_i %*% t(owner_i)
        price_pre_i <- temp$price
        marg_cost <- (temp %>%
                        mutate(marginal_cost = ifelse(category == 7467, marginal_cost,
                                                      marginal_cost + t)))$marginal_cost
        
        solution <-try(nleqslv(
          x = price_pre_i, foc_bertrand_mkt, # startingguesses: price_pre_i
          own_prod = owner_i,
          blp_data = sdf2_blp,
          mkt = k,
          marg_cost = marg_cost,
          theta_lin = (theta1_bar %>%
                         filter(variable == "price"))$coef,
          theta_rc = theta2_bar_alt
        ))
        if(
          "try-error" %in% class(solution)
        ) {
          print(paste0("error with market ", k))
          gc()
        } else {
          
          temp %>%
            select(cdid, brand, price) %>%
            mutate(cfprice = solution$x,
                   cfid = cfid, tax = t) %>%
            dbWriteTable(results2,
                         "counterfactuals",
                         .,
                         append = T)
          
          print(paste0(ms, ", ", t, "in ", k))
          gc()
          
        }
        
      }
      
    }
    
  }
  
}

#CF5 1% Tax---------------------------------------------------------------------
if(5 %in% doNotRun){
  
  #Cleanup-
} else {
  cfid = 5
  for (ms in 1:length(markets_2020_sub)) {
    k <- markets_2020_sub[ms]
    market_ind <- market_id == k
    temp <- sdf2 %>% filter(cdid ==k ) 
    brands <- unique(temp$brand)
    owner_i <- ownership %>% ungroup() %>%
      filter(cdid == k) %>%
      filter(brand %in% brands) %>%
      select(-cdid, -brand) %>%
      as.matrix() 
    owner_i <- owner_i %*% t(owner_i)
    price_pre_i <- temp$price
    marg_cost <- (temp %>%
                    mutate(marginal_cost = ifelse(category == 7467, marginal_cost,
                                                  marginal_cost + price*0.01)))$marginal_cost
    
    solution <-try(nleqslv(
      x = price_pre_i, foc_bertrand_mkt, # startingguesses: price_pre_i
      own_prod = owner_i,
      blp_data = sdf2_blp,
      mkt = k,
      marg_cost = marg_cost,
      theta_lin = (theta1_bar %>%
                     filter(variable == "price"))$coef,
      theta_rc = theta2_bar_alt
    ))
    if(
      "try-error" %in% class(solution)
    ) {
      print(paste0("error with market ", k))
      gc()
    } else {
      
      temp %>%
        select(cdid, brand, price) %>%
        mutate(cfprice = solution$x,
               cfid = cfid) %>%
        dbWriteTable(results2,
                     "counterfactuals",
                     .,
                     append = T)
      
      print(ms)
      gc()
      
    }
    
  }
  
}

#CF6 Juul Ban-------------------------------------------------------------------
if(6 %in% doNotRun){
  
} else {
  juul_markets <- sample((sdf2 %>%
    select(cdid, brand, year) %>%
    filter(brand == "JUUL" & year == 2020))$cdid, 100)
  cfid = 6
  for (ms in 1:length(juul_markets)) {
    k <- juul_markets[ms]
    market_ind <- market_id == k
    temp <- sdf2 %>% filter(cdid ==k ) 
    brands <- unique(temp$brand)
    owner_i <- ownership %>% ungroup() %>%
      filter(cdid == k) %>%
      filter(brand %in% brands) %>%
      select(-cdid, -brand) %>%
      as.matrix() 
    owner_i <- owner_i %*% t(owner_i)
    price_pre_i <- temp$price
    marg_cost <- (temp %>%
                    mutate(marginal_cost = ifelse(brand == "JUUL", marginal_cost+10,
                                                  marginal_cost)))$marginal_cost
    
    solution <-try(nleqslv(
      x = price_pre_i, foc_bertrand_mkt, # startingguesses: price_pre_i
      own_prod = owner_i,
      blp_data = sdf2_blp,
      mkt = k,
      marg_cost = marg_cost,
      theta_lin = (theta1_bar %>%
                     filter(variable == "price"))$coef,
      theta_rc = theta2_bar_alt
    ))
    if(
      "try-error" %in% class(solution)
    ) {
      print(paste0("error with market ", k))
      gc()
    } else {
      
      temp %>%
        select(cdid, brand, price) %>%
        mutate(cfprice = solution$x,
               cfid = cfid) %>%
        dbWriteTable(results2,
                     "counterfactuals",
                     .,
                     append = T)
      
      print(ms)
      gc()
      
    }
    
  }
  
}
  
#CF7 Stength Tax----------------------------------------------------------------
if(7 %in% doNotRun){
  
  #Cleanup-
} else {
  cfid = 7
  for (ms in 1:length(markets_2020_sub)) {
    k <- markets_2020_sub[ms]
    market_ind <- market_id == k
    temp <- sdf2 %>% filter(cdid ==k ) 
    brands <- unique(temp$brand)
    owner_i <- ownership %>% ungroup() %>%
      filter(cdid == k) %>%
      filter(brand %in% brands) %>%
      select(-cdid, -brand) %>%
      as.matrix() 
    owner_i <- owner_i %*% t(owner_i)
    price_pre_i <- temp$price
    marg_cost <- (temp %>%
                    mutate(marginal_cost = marginal_cost + (1 - temp$lo - temp$ul)*1)
    )$marginal_cost
    
    solution <-try(nleqslv(
      x = price_pre_i, foc_bertrand_mkt, # startingguesses: price_pre_i
      own_prod = owner_i,
      blp_data = sdf2_blp,
      mkt = k,
      marg_cost = marg_cost,
      theta_lin = (theta1_bar %>%
                     filter(variable == "price"))$coef,
      theta_rc = theta2_bar_alt
    ))
    if(
      "try-error" %in% class(solution)
    ) {
      print(paste0("error with market ", k))
      gc()
    } else {
      
      temp %>%
        select(cdid, brand, price) %>%
        mutate(cfprice = solution$x,
               cfid = cfid) %>%
        dbWriteTable(results2,
                     "counterfactuals",
                     .,
                     append = T)
      
      print(ms)
      gc()
      
    }
    
  }
  
}

#CF8 Flavor Tax---------------------------------------------------------------------
if(8 %in% doNotRun){
  
} else {
  cfid = 8
  for (ms in 1:length(markets_2020_sub)) {
    k <- markets_2020_sub[ms]
    market_ind <- market_id == k
    temp <- sdf2 %>% filter(cdid ==k ) 
    brands <- unique(temp$brand)
    owner_i <- ownership %>% ungroup() %>%
      filter(cdid == k) %>%
      filter(brand %in% brands) %>%
      select(-cdid, -brand) %>%
      as.matrix() 
    owner_i <- owner_i %*% t(owner_i)
    price_pre_i <- temp$price
    marg_cost <- (temp %>%
                    mutate(marginal_cost = marginal_cost + (1 - temp$tob)*1)
    )$marginal_cost
    
    solution <-try(nleqslv(
      x = price_pre_i, foc_bertrand_mkt, # startingguesses: price_pre_i
      own_prod = owner_i,
      blp_data = sdf2_blp,
      mkt = k,
      marg_cost = marg_cost,
      theta_lin = (theta1_bar %>%
                     filter(variable == "price"))$coef,
      theta_rc = theta2_bar_alt
    ))
    if(
      "try-error" %in% class(solution)
    ) {
      print(paste0("error with market ", k))
      gc()
    } else {
      
      temp %>%
        select(cdid, brand, price) %>%
        mutate(cfprice = solution$x,
               cfid = cfid) %>%
        dbWriteTable(results2,
                     "counterfactuals",
                     .,
                     append = T)
      
      print(ms)
      gc()
      
    }
    
  }
  
}
#CF9 Anonmyized Aq Analysis-----------------------------------------------------
dbExecute(
  results2,
  "ALTER TABLE counterfactuals_all_metrics ADD COLUMN buy_comp"
)
dbExecute(
  results2,
  "ALTER TABLE counterfactuals_all_metrics ADD COLUMN sell_comp"
)
dbExecute(
  results2,
  "ALTER TABLE counterfactuals_all_metrics ADD COLUMN sell_brands"
)

dbExecute(
  results2,
  "ALTER TABLE counterfactuals ADD COLUMN buy_comp"
)
dbExecute(
  results2,
  "ALTER TABLE counterfactuals ADD COLUMN sell_comp"
)
dbExecute(
  results2,
  "ALTER TABLE counterfactuals ADD COLUMN sell_brands"
)

cfid = 9
cig_comps <- c("Reynolds", "ITG", "PMUSA", "Liggett", "Dosal")
ecig_comps <- names(ownership %>% select(-cdid, -brand))[
  !(names(ownership %>% select(-cdid, -brand)) %in% cig_comps)
]
ecig_brands <- ownership %>% 
  select(brand, ecig_comps) %>%
  distinct() %>%
  dplyr::mutate(
    is_ecig = rowSums(across(where(is.numeric) & !matches("brand")))
  ) %>%
  filter(is_ecig == 1) %>%
  select(-is_ecig)
  

for(i in 1:100){
  
  buying_comp <- sample(cig_comps, 1)
  selling_comp <- sample(ecig_comps, 1)
  selling_brands <- ownership %>% 
    select(brand, selling_comp) %>%
    distinct() %>%
    filter(eval(as.name(selling_comp)) == 1) %>%
    select(brand) %>%
    distinct() %>%
    pull()
    

  c_ownership <- ownership %>%
    mutate(x = brand %in% selling_brands) %>%
    dplyr::mutate(!!sym(buying_comp) := ifelse(x == T, 1, !!sym(buying_comp))) %>%
    select(-!!sym(selling_comp)) %>%
    select(-x) 
  
  if(c_ownership %>%
    select(-cdid) %>%
    distinct() %>%
    dplyr::mutate(
      check = rowSums(across(where(is.numeric) & !matches("brand")))
    ) %>%
    pull(check) %>%
    mean() != 1) {
    stop("A brand corresponds to more than one company.")
  }
  
  markets_to_do <- sample(markets_2020_sub, 5, F)
  
  for(ms in 1:length(markets_to_do)) {
    k <- markets_2020_sub[ms]
    market_ind <- market_id == k
    temp <- sdf2 %>% filter(cdid ==k ) 
    brands <- unique(temp$brand)
    owner_i <- c_ownership %>% ungroup() %>%
      filter(cdid == k) %>%
      filter(brand %in% brands) %>%
      select(-cdid, -brand) %>%
      as.matrix() 
    owner_i <- owner_i %*% t(owner_i)
    price_pre_i <- temp$price
    marg_cost <- temp$marginal_cost
    
    solution <-try(nleqslv(
      x = price_pre_i, foc_bertrand_mkt, # startingguesses: price_pre_i
      own_prod = owner_i,
      blp_data = sdf2_blp,
      mkt = k,
      marg_cost = marg_cost,
      theta_lin = (theta1_bar %>%
                     filter(variable == "price"))$coef,
      theta_rc = theta2_bar_alt
    ))
    if(
      "try-error" %in% class(solution)
    ) {
      print(paste0("error with market ", k))
      gc()
    } else {
      
      temp %>%
        select(cdid, brand, price) %>%
        mutate(cfprice = solution$x,
               cfid = cfid,
               tax = NA) %>%
        mutate(buy_comp = buying_comp,
               sell_comp = selling_comp,
               sell_brands = paste0(selling_brands, collapse = ",")) %>%
        dbWriteTable(results2,
                     "counterfactuals",
                     .,
                     append = T)
      
      print(ms)
      gc()
      
    }
    
    
  }
  
  print(i)
  
}

#CF10 Random ECig Brand Bans----------------------------------------------------
dbExecute(
  results2,
  "ALTER TABLE counterfactuals ADD COLUMN banned_brand"
)

if(10 %in% doNotRun){
  
} else {
  cfid = 10
  for (ms in 1:length(juul_markets)) {
    k <- juul_markets[ms]
    market_ind <- market_id == k
    temp <- sdf2 %>% filter(cdid ==k ) 
    brands <- unique(temp$brand)
    brand_to_ban <- sample((temp %>% filter(category == 7460))$brand, 1)
    owner_i <- ownership %>% ungroup() %>%
      filter(cdid == k) %>%
      filter(brand %in% brands) %>%
      select(-cdid, -brand) %>%
      as.matrix() 
    owner_i <- owner_i %*% t(owner_i)
    price_pre_i <- temp$price
    marg_cost <- (temp %>%
                    mutate(marginal_cost = ifelse(brand == brand_to_ban, marginal_cost+10,
                                                  marginal_cost)))$marginal_cost
    
    solution <-try(nleqslv(
      x = price_pre_i, foc_bertrand_mkt, # startingguesses: price_pre_i
      own_prod = owner_i,
      blp_data = sdf2_blp,
      mkt = k,
      marg_cost = marg_cost,
      theta_lin = (theta1_bar %>%
                     filter(variable == "price"))$coef,
      theta_rc = theta2_bar_alt
    ))
    if(
      "try-error" %in% class(solution)
    ) {
      print(paste0("error with market ", k))
      gc()
    } else {
      
      temp %>%
        select(cdid, brand, price) %>%
        mutate(cfprice = solution$x,
               cfid = cfid,
               banned_brand = brand_to_ban) %>%
        dbWriteTable(results2,
                     "counterfactuals",
                     .,
                     append = T)
      
      print(ms)
      gc()
      
    }
    
  }
  
}
     