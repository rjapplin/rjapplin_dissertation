#Data Setup-------------------------------------------------------------------
if(!exists("df2")){
  df2 <- dbReadTable(results, "data.post_est")
} else {

}

if("data.post_est_sample_mkts" %in% dbListTables(results)){
  sdf2 <- df2 %>%
    filter(cdid %in% dbReadTable(results, "data.post_est_sample_mkts")$cdid) %>%
    filter(!(is.na(delta)))
} else {
  sdf2 <- sdf2 %>%
    filter(cdid %in% sample(unique(sdf2$cdid), 10000, T)) %>%
    filter(!is.na(delta)) 
  sf_markets <- unique(sdf2$cdid)
  dbWriteTable(results, "data.post_est_sample_mkts", data.frame(cdid = sf_markets))
  rm(sf_markets)
  
}

sdf2_blp <- BLP_data(
  model = "inShare ~ price + men + lo + border_county + as.factor(year) +
    border_county:as.factor(year) + as.factor(brand)  | 
    men + lo + border_county + as.factor(year) +
    border_county:as.factor(year) + as.factor(brand)  | 
    price  |
    0 + iv2 + iv3 + iv8",
  market_identifier = "cdid",
  product_identifier = "brand",
  productData = sdf2,
  blp_inner_tol = 1e-6, blp_inner_maxit = 5000,
  integration_method = "MLHS",
  integration_accuracy = 1000,
  integration_seed = 1,
  par_delta = "delta"
)

rm(df2)
gc()

theta1_bar <- dbReadTable(results2, "theta1_bar")
theta2_bar <- dbReadTable(results2, "theta2_bar")

theta2_bar_alt <- theta2_bar$coef %>% matrix()
rownames(theta2_bar_alt) <- c("(Intercept)", "price")
colnames(theta2_bar_alt) <- "unobs_sd"

#Shares-------------------------------------------------------------------------
sdf2_shareObj <- getShareInfo(
  blp_data = sdf2_blp,
  par_theta2 = theta2_bar_alt,
)
#Elasticity---------------------------------------------------------------------
if("avg_elas" %in% dbListTables(results) &
   file.exists("code/05_analysis/elasticity_lists.RData")){
  
  avg_elas <- dbReadTable(results, "avg_elas")
  
} else {

  elasticity_list <- list()
  markets <- unique(sdf2$cdid)
  allBrands <- unique(sdf2$brand)
  
  
  for(m in 1:length(markets)){
    
    edf <- get_elasticities(
      blp_data = sdf2_blp,
      share_info = sdf2_shareObj,
      theta_lin = (theta1_bar %>%
        filter(variable == "price"))$coef,
      variable = "price",
      market = markets[m]
    ) %>%
      as.data.frame() 
    
    for(b in 1:length(allBrands)){
      
      if(!(allBrands[b] %in% colnames(edf))){
        
        edf[, allBrands[b]] <- as.numeric(NA)
        edf[allBrands[b], ] <- as.numeric(NA)
        
      }
      
    }
    
    edf <- edf %>%
      mutate(market = markets[m])
    
    elasticity_list[[m]] <- edf
    print(m)
    
  }
  
  elasticity_list2 <- lapply(elasticity_list,
                             function(x){
                               x %>%
                                 select(-market) -> temp
                               
                               temp <- temp %>% 
                                 select((order(colnames(temp)))) %>%
                                 rownames_to_column() %>%
                                 arrange(rowname) %>%
                                 column_to_rownames("rowname")
                               temp <- as.matrix(temp)
                               
                               return(temp[1:36, 1:36])
                             })
  
  mean_narm <- function(X){
    mean(X, na.rm = TRUE)
  }
  
  avg_elas <- apply(simplify2array(elasticity_list2), 
                                              1:2, mean_narm) %>%
    as.data.frame() 
  rm(edf)
  
  colnames(avg_elas) <- paste0("B_", colnames(avg_elas))
  rownames(avg_elas) <- paste0("B_", rownames(avg_elas))
  avg_elas <- avg_elas %>%
    rownames_to_column("Brand")
  dbWriteTable(results2, "avg_elas", avg_elas, overwrite = T)
  
}

#Markups/Costs------------------------------------------------------------------
if("ownership" %in% dbListTables(results)){
  ownership <- dbReadTable(results2, "ownership")
  colnames(ownership) <- c("cdid", "brand", 
                           gsub("F_", "", colnames(ownership)[3:15]))
  
} else {
  
  ownership <- sdf2 %>%
    inner_join(
      dbReadTable(main, "brands") %>%
        select(brand_descr, subparent) %>%
        dplyr::rename(brand = brand_descr),
      by = "brand"
    ) %>%
    select(cdid, brand, subparent) %>%
    mutate(b2 = brand) %>%
    pivot_wider(id_cols = c(cdid, brand),
                names_from = subparent,
                values_from = b2,
                values_fn = n_distinct,
                values_fill = 0)
  ownership_temp <- ownership
  colnames(ownership_temp) <- c("cdid", "brand", 
                                paste0("F_", colnames(ownership_temp)[3:15]))
  dbWriteTable(results, "ownership", ownership_temp, overwrite = T)
  rm(ownership_temp)
}

if("markup_and_cost" %in% dbListTables(results2)){
  sdf2 <- sdf2 %>%
    left_join(dbReadTable(results, "markup_and_cost"),
              by = c("cdid", "brand"))
} else {
  
  markup_list <- list()
  sh <- sdf2_shareObj$shares
  if(!exists("elasticity_list")){
    load("code/05_analysis/elasticity_lists.RData")
  }
  names(elasticity_list) <- paste0("M", markets)
  #BLP Markup Loop
  for(j in 1:length(markets)){
    
    loop_data <- sdf2 %>% filter(cdid == markets[j]) %>%
      arrange(brand)
    
    brands <- unique(loop_data$brand)
    share_i <- sh[grepl(paste0("\\_", markets[j], "$"), names(sh))]
    prices_pre_i <- loop_data$price
    scalar_i <- matrix(1 / share_i) %*% matrix(prices_pre_i, nrow = 1)
    elas_i <- get_elasticities(
      sdf2_blp,
      sdf2_shareObj,
      (theta1_bar %>%
         filter(variable == "price"))$coef,
      "price",
      market = markets[j]
    )
    derivatives_i <- elas_i / scalar_i
    owner_i <- ownership %>% ungroup() %>%
      filter(cdid == markets[j]) %>%
      filter(brand %in% brands) %>%
      select(-cdid, -brand) %>%
      as.matrix() 
    owner_i <- owner_i %*% t(owner_i)
    markups <- try(((solve(derivatives_i * owner_i) %*% share_i))*-1)
    if("try-error" %in% class(markups)){
      markup_list[[j]] <- data.frame(
        brand = NA,
        markup = NA,
        market = markets[j]
      )
    } else {
      markup_list[[j]] <- markups %>%
        as.data.frame() %>%
        rownames_to_column(var = "brand") %>%
        dplyr::rename("markup" = V1) %>%
        mutate(market = markets[j])
    }
    print(j)
    
  }
  markups <- Reduce(rbind, markup_list)
  markups <- markups %>%
    mutate(
      markup = ifelse(markup > 100 | markup < -100, NA, markup)
    )
  sdf2 %>%
    left_join(markups %>% dplyr::rename(cdid = market),
               by = c("cdid", "brand")) %>%
    mutate(marginal_cost = price - markup,
           percent_markup = markup/price) -> sdf2
  rm(loop_data, markup_list, owner_i, scalar_i, j, prices_pre_i,
     derivatives_i, elas_i, share_i, brands, sh, b, m)
  
  sdf2 %>%
    select(cdid, brand, markup, marginal_cost, percent_markup) %>%
    dbWriteTable(results2, "markup_and_cost", ., overwrite = T)
  
}

#Cleanup------------------------------------------------------------------------
rm(c_ownership, markups, b, neighbors, solution, temp, us_counties,
   us_states, all_tn, bc, border_tn_states_all, bordering_states,
   brands, cfid, doMoreIters, doNotRun, i, j, k, marg_cost, market_ind,
   markets_2020, markets_2020_sub, markets_to_analyze, ms, price_pre_i,
   s, sub_mkts, t, tax_vec, tn_border, market_id, nmkts, avg_elas)
