if("counterfactuals_all_metrics" %in% dbListTables(results2)){
  
} else {
  
#Initial Setup----------------------------------------------------------------
  cfs <- dbReadTable(results2, "counterfactuals") %>%
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
      tax = case_when(
        cfid == 1 & category == 7460 ~ 1,
        cfid == 1 & category == 7467 ~ 0,
        cfid == 2 ~ 0,
        cfid == 3 & category == 7460 ~ 1,
        cfid == 3 & category == 7467 ~ 0,
        cfid == 4 ~ tax,
        cfid == 5 & category == 7460 ~ price*0.01,
        cfid == 5 & category == 7467 ~ 0,
        cfid == 6 & brand == "MASKED" ~ 10,
        cfid == 6 & brand != "MASKED" ~ 0,
        cfid == 7 ~ (1 - lo - ul),
        cfid == 8 ~ (1 - tob)
      )
    ) %>%
    mutate(
      gov_rev = tax*((inShare_hat_approx_cf*market_size_real)/cfprice),
      rev_cf = inShare_hat_approx_cf*cfprice*market_size_real,
      marginal_cost_cf = case_when(
        cfid == 2 ~ marginal_cost,
        .default = marginal_cost + tax
      )
    )
  

#Cleanup------------------------------------------------------------------------
dim(dbReadTable(results, "counterfactuals_all_metrics"))
rm(cfs)
