#Moel Performance---------------------------------------------------------------


#Average Own--------------------------------------------------------------------
avg_elas <- dbReadTable(results2, "avg_elas")
colnames(avg_elas) <- colnames(avg_elas) %>%
  gsub("B_", "", .) %>%
  gsub("\\.", " ", .) %>%
  gsub("MASKED", .) %>%
  gsub("MASKED", .)
rownames(avg_elas) <- colnames(avg_elas)[2:37]

avg_elas %>%
  rownames_to_column(var = "BRAND") %>%
  reshape2::melt() %>%
  mutate(BRAND = sub("*B_", "", BRAND),
         variable = sub("*B_", "", variable)) %>%
  mutate(variable = sub("\\.", " ", variable)) %>%
  mutate(variable = sub("L ..M", "L & M", variable)) %>%
  mutate(variable = sub("MASKED", variable)) %>%
  mutate(variable = sub("MASKED", variable)) %>%
  inner_join((dbReadTable(main, "brands") %>% dplyr::rename(BRAND = brand_descr) %>% select(BRAND, category)),
             by = "BRAND") %>%
  filter(BRAND == variable) %>%
  mutate(category = ifelse(category == 7467, 0, 1)) %>%
  arrange(category, BRAND) %>%
  group_by(category) %>%
  dplyr::summarize(m = mean(value))

#Cross Elasticities-------------------------------------------------------------------
avg_elas <- dbReadTable(results2, "avg_elas")
colnames(avg_elas) <- colnames(avg_elas) %>%
  gsub("B_", "", .) %>%
  gsub("\\.", " ", .) %>%
  gsub("MASKED", .) %>%
  gsub("MASKED", .)
rownames(avg_elas) <- colnames(avg_elas)


avg_elas %>%
  rownames_to_column(var = "BRAND") %>%
  reshape2::melt() %>%
  inner_join((dbReadTable(main, "brands") %>% dplyr::rename(BRAND = brand_descr) %>% select(BRAND, category)),
             by = "BRAND") %>%
  filter(BRAND != variable) %>%
  mutate(category = ifelse(category == 7467, 0, 1)) %>%
  arrange(category, BRAND) %>% 
  inner_join(sdf2 %>% ungroup() %>% 
               select(brand, category) %>% 
               dplyr::rename(variable = brand, category.p = category) %>%
               distinct() %>% mutate(category.p = 
                                       ifelse(category.p == 7467, 0, 1)), 
             by = "variable") %>% 
  mutate(elas_type = case_when(category == 0 & category.p == 0 ~ "c_wrt_c", 
                               category == 0 & category.p == 1 ~ "c_wrt_e", 
                               category == 1 & category.p == 0 ~ "e_wrt_c", 
                               category == 1 & category.p == 1 ~ "e_wrt_e")) %>% 
  group_by(elas_type) %>% dplyr::summarize(m = mean(value, na.rm = T))

#Average Cross-Price Remove Outliers--------------------------------------------
avg_elas <- dbReadTable(results2, "avg_elas")
colnames(avg_elas) <- colnames(avg_elas) %>%
  gsub("B_", "", .) %>%
  gsub("\\.", " ", .) %>%
  gsub("L   M", "L & M", .) %>%
  gsub("MASKED", .)
rownames(avg_elas) <- colnames(avg_elas)

avg_elas %>%
  rownames_to_column(var = "BRAND") %>%
  reshape2::melt() %>%
  inner_join((dbReadTable(main, "brands") %>% dplyr::rename(BRAND = brand_descr) %>% select(BRAND, category)),
             by = "BRAND") %>%
  filter(BRAND != variable) %>%
  mutate(category = ifelse(category == 7467, 0, 1)) %>%
  arrange(category, BRAND) %>% 
  inner_join(sdf2 %>% ungroup() %>% 
               select(brand, category) %>% 
               dplyr::rename(variable = brand, category.p = category) %>%
               distinct() %>% mutate(category.p = 
                                       ifelse(category.p == 7467, 0, 1)), 
             by = "variable") %>% 
  mutate(elas_type = case_when(category == 0 & category.p == 0 ~ "c_wrt_c", 
                               category == 0 & category.p == 1 ~ "c_wrt_e", 
                               category == 1 & category.p == 0 ~ "e_wrt_c", 
                               category == 1 & category.p == 1 ~ "e_wrt_e")) %>% 
  filter(value < 1) %>%
  group_by(elas_type) %>% dplyr::summarize(m = mean(value, na.rm = T),
                                           s = sd(value, na.rm = T))

#Counterfactual Nat. Tax Results--------------------------------------------------
dbReadTable(results2, "counterfactuals_all_metrics") %>%
  filter(cfid == 1 | cfid == 5) %>%
  mutate(sim_type = ifelse(cfid == 1, "flat one dollar", "ad val 1%")) %>%
  select(sim_type, cdid, category, brand, tax, market_size_real,
         price, cfprice, 
         inShare_hat_approx, inShare_hat_approx_cf,
         markup, markup_cf,
         marginal_cost, marginal_cost_cf,
         profit, profit_cf, 
         welfare, welfare_cf,
         gov_rev) %>%
  # mutate(
  #   profit = ifelse(category == 7467, profit/20, profit),
  #   profit_cf = ifelse(category == 7467, profit_cf/20, profit_cf)
  # ) %>%
  mutate(q0 = (inShare_hat_approx*market_size_real)/price,
         q1 = (inShare_hat_approx_cf*market_size_real/cfprice)
  ) %>%
  mutate(dq = q1 - q0) %>%
  group_by(sim_type, cdid, category) %>%
  dplyr::mutate(cat_share = sum(inShare_hat_approx),
         cat_share_cf = sum(inShare_hat_approx_cf)) %>%
  filter(gov_rev >= 0) %>%
  group_by(sim_type, category, cdid) %>%
  dplyr::mutate(b = n_distinct(brand)) %>%
  group_by(sim_type, category) %>%
  dplyr::summarize(
    tax = mean(tax),
    dp = mean(cfprice - price),
    dp_percent = mean(cfprice - price)/mean(price),
    ds = mean(cat_share_cf -cat_share),
    ds_percent = mean(cat_share_cf - cat_share)/mean(cat_share),
    dmk = mean(markup_cf - markup),
    dmk_percent = mean(markup_cf - markup)/mean(markup),
    dc = mean(marginal_cost_cf - marginal_cost),
    dc_percent = mean(marginal_cost_cf - marginal_cost)/mean(marginal_cost),
    dpi = mean(profit_cf - profit),
    dpi_percent = mean(profit_cf - profit)/mean(profit),
    dwi = mean(welfare_cf - welfare),
    dWb = mean(welfare_cf*(q1) - welfare*q1),
    dw_percent = mean(welfare_cf - welfare)/mean(welfare),
    mgr = mean(gov_rev),
    q0 = mean(q0),
    q1 = mean(q1),
    n = mean(b)
  ) %>%
  group_by(sim_type) %>%
  dplyr::mutate(dwi = mean(dwi),
                dw_percent = mean(dw_percent)) %>%
  mutate(
    dW = dWb*n,
    dPi = dpi*n,
    GR = mgr*n,
    DWL = dW + dPi + GR
  ) %>%
  #mutate_at(2:25, round, 3) %>%
  select(-starts_with("sd")) %>%
  t() %>%
  as.data.frame()

#Border Tax Averages------------------------------------------------------------
dbReadTable(results2, "counterfactuals_all_metrics") %>%
  filter(cfid == 3) %>%
  select(cdid, category, brand, tax, market_size_real,
         price, cfprice, 
         inShare_hat_approx, inShare_hat_approx_cf,
         markup, markup_cf,
         marginal_cost, marginal_cost_cf,
         profit, profit_cf, 
         welfare, welfare_cf,
         gov_rev) %>%
  inner_join(
    sdf2 %>% select(cdid, border_county, fips_code, fips_state_code,
                    fips_county_code, quarter, bordering_states) %>%
      distinct(),
    by = "cdid"
  ) %>%
  mutate(
    profit = ifelse(category == 7467, profit/20, profit),
    profit_cf = ifelse(category == 7467, profit_cf/20, profit_cf)
  ) %>%
  mutate(q0 = (inShare_hat_approx*market_size_real)/price,
         q1 = (inShare_hat_approx_cf*market_size_real/cfprice)
  ) %>%
  mutate(dq = q1 - q0) %>%
  group_by(cdid, category) %>%
  dplyr::mutate(cat_share = sum(inShare_hat_approx),
                cat_share_cf = sum(inShare_hat_approx_cf)) %>%
  filter(gov_rev >= 0) %>%
  group_by(category, cdid) %>%
  dplyr::mutate(b = n_distinct(brand)) %>%
  mutate(
    dW = q1*(welfare_cf - welfare),
    dPi = profit_cf - profit,
    dP = cfprice - price,
    dC = marginal_cost_cf - marginal_cost,
    dS = cat_share_cf - cat_share,
    dQ = q1 - q0,
    dM = markup_cf - markup,
    n = n_distinct(brand)
  ) %>%
  select(category, brand, cdid, fips_county_code, quarter, border_county, dW, dPi, dP, dC,
         dS, dQ, dM, gov_rev, price, welfare, welfare_cf, profit_cf, marginal_cost,
         markup, profit, cat_share, q1, bordering_states) %>%
  mutate(DWL = dW + dPi + gov_rev) -> temp

temp %>%
  filter(category == 7460) %>%
  mutate(
    group = case_when(
      border_county == FALSE ~ "inner",
      border_county == TRUE & bordering_states == 37 ~ "Borders Taxed",
      border_county == TRUE & bordering_states != 37 ~ "Borders Non-Taxed"
    )
  ) %>%
  group_by(group) %>%
  dplyr::summarize(
    dP = mean(dP),
    dP_percent = mean(dP/price),
    dW = mean(dW)*n_distinct(brand),
    dW_percent = mean(dW)/mean(welfare*q1),
    dPi = mean(dPi)*n_distinct(brand),
    dPi_percent = mean(dPi)/mean(profit),
    dC = mean(dC),
    dC_percent = mean(dC)/mean(marginal_cost),
    dS = mean(dS),
    dS_percent = mean(dS)/mean(cat_share),
    dM = mean(dM),
    dM_percent = mean(dM)/mean(markup),
    GR = mean(gov_rev)*n_distinct(brand),
    DWL = mean(DWL)*n_distinct(brand),
    n = n_distinct(brand),
    dw = mean(welfare_cf - welfare),
    dpi = mean(profit_cf - profit),
    dpi_percent = mean((profit_cf - profit)/profit),
    dw_percent = mean((welfare_cf - welfare)/welfare)
  ) 

reslist <- list()
deps <- c("dP", "dS", "dM", "dpi", "dw", "dW", "dPi", "gov_rev", "DWL")
for(y in 1:length(deps)){
  
  form <- paste0(deps[y], " ~ border_county + borders_taxed + as.factor(fips_county_code) +
                   as.factor(brand) + as.factor(quarter)")
  reg <- lm(form, temp %>%
              mutate(dpi = profit_cf - profit,
                     dw = welfare_cf - welfare,
                     dPi = dPi*n_distinct(brand),
                     gov_rev = gov_rev*n_distinct(brand),
                     DWL = DWL*n_distinct(brand)) %>%
              mutate(borders_taxed = ifelse(bordering_states == 37, 1, 0)) %>%
              filter(category == 7460)
  )
  reslist[[y]] <- summary(reg)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("feature") %>%
    filter(grepl("border", feature)) %>%
    mutate(dependent = deps[y]) %>%
    dplyr::rename(se = `Std. Error`) %>%
    select(dependent, feature, Estimate, se) %>%
    mutate(
      feature = ifelse(grepl("TRUE", feature), "border", "border_taxed")
    ) %>%
    pivot_wider(id_cols = dependent, names_from = feature, values_from = c(Estimate, se))
  
} 
border_res <- reduce(reslist, rbind)
border_res %>%
  mutate_at(2:5, round, 3) %>%
  as.data.frame(
  )
  
  
  
  
  
  
  
  
#Strength and Flavor Taxes------------------------------------------------------
dbReadTable(results2, "counterfactuals_all_metrics") %>%
  filter(cfid == 7 | cfid == 8) %>%
  mutate(sim_type = ifelse(cfid == 7, "Strength", "Flavor")) %>%
  select(sim_type, cdid, category, brand, tax, market_size_real,
         price, cfprice, 
         inShare_hat_approx, inShare_hat_approx_cf,
         markup, markup_cf,
         marginal_cost, marginal_cost_cf,
         profit, profit_cf, 
         welfare, welfare_cf,
         gov_rev) %>%
  mutate(q0 = (inShare_hat_approx*market_size_real)/price,
         q1 = (inShare_hat_approx_cf*market_size_real/cfprice)
  ) %>%
  mutate(dq = q1 - q0) %>%
  group_by(sim_type, cdid, category) %>%
  dplyr::mutate(cat_share = sum(inShare_hat_approx),
                cat_share_cf = sum(inShare_hat_approx_cf)) %>%
  group_by(sim_type, category, cdid) %>%
  dplyr::mutate(b = n_distinct(brand)) %>%
  group_by(sim_type, category) %>%
  dplyr::summarize(
    tax = mean(tax),
    dp = mean(cfprice - price),
    dp_percent = mean(cfprice - price)/mean(price),
    ds = mean(cat_share_cf -cat_share),
    ds_percent = mean(cat_share_cf - cat_share)/mean(cat_share),
    dmk = mean(markup_cf - markup),
    dmk_percent = mean(markup_cf - markup)/mean(markup),
    dc = mean(marginal_cost_cf - marginal_cost),
    dc_percent = mean(marginal_cost_cf - marginal_cost)/mean(marginal_cost),
    dpi = mean(profit_cf - profit),
    dpi_percent = mean(profit_cf - profit)/mean(profit),
    dwi = mean(welfare_cf - welfare),
    dWb = mean(welfare_cf*(q1) - welfare*q1),
    dw_percent = mean(welfare_cf - welfare)/mean(welfare),
    mgr = mean(gov_rev),
    q0 = mean(q0),
    q1 = mean(q1),
    n = mean(b)
  ) %>%
  group_by(sim_type) %>%
  dplyr::mutate(dwi = mean(dwi),
                dw_percent = mean(dw_percent)) %>%
  mutate(
    dW = dWb*n,
    dPi = dpi*n,
    GR = mgr*n,
    DWL = dW + dPi + GR
  ) %>%
  #mutate_at(2:25, round, 3) %>%
  select(-starts_with("sd")) %>%
  t() %>%
  as.data.frame()

#NJOY Aquisition----------------------------------------------------------------
dbReadTable(results2, "ma_counterfactuals") %>%
  inner_join(
    sdf2 %>% select(brand, category) %>% distinct(),
    by = "brand"
  ) %>%
  mutate(category = ifelse(category == 7467, "cigarette", "e-cigarette")) %>%
  select(cdid, category, brand, market_size_real,
         price, cfprice, 
         inShare_hat_approx, inShare_hat_approx_cf,
         markup, cf_markup,
         marginal_cost, marginal_cost_cf,
         profit, profit_cf, 
         welfare, welfare_cf) %>%
  mutate(q0 = (inShare_hat_approx*market_size_real)/price,
         q1 = (inShare_hat_approx_cf*market_size_real/cfprice)
  ) %>%
  mutate(dq = q1 - q0) %>%
  group_by(cdid, category) %>%
  dplyr::mutate(cat_share = sum(inShare_hat_approx),
                cat_share_cf = sum(inShare_hat_approx_cf)) %>%
  group_by(category, cdid) %>%
  dplyr::mutate(b = n_distinct(brand)) %>%
  group_by(category) %>%
  dplyr::summarize(
    dp = mean(cfprice - price),
    dp_percent = mean(cfprice - price)/mean(price),
    ds = mean(cat_share_cf -cat_share),
    ds_percent = mean(cat_share_cf - cat_share)/mean(cat_share),
    dmk = mean(cf_markup - markup, na.rm = T),
    dmk_percent = mean(cf_markup - markup, na.rm = T)/mean(markup, na.rm = T),
    dc = mean(marginal_cost_cf - marginal_cost, na.rm = T),
    dc_percent = mean(marginal_cost_cf - marginal_cost, na.rm = T)/mean(marginal_cost, na.rm = T),
    dpi = mean(profit_cf - profit, na.rm = T),
    dpi_percent = mean(profit_cf - profit, na.rm = T)/mean(profit, na.rm = T),
    dwi = mean(welfare_cf - welfare, na.rm = T),
    dWb = mean(welfare_cf*(q1) - welfare*q1, na.rm = T),
    dw_percent = mean(welfare_cf - welfare, na.rm = T)/mean(welfare, na.rm = T),
    q0 = mean(q0),
    q1 = mean(q1),
    n = mean(b)
  ) %>%
  ungroup() %>%
  dplyr::mutate(dwi = mean(dwi),
                dw_percent = mean(dw_percent)) %>%
  mutate(
    dW = dWb*n,
    dPi = dpi*n,
    DWL = dW + dPi
  ) %>%
  #mutate_at(2:25, round, 3) %>%
  select(-starts_with("sd")) %>%
  t() %>%
  as.data.frame()






#First Stage for IV-------------------------------------------------------------
lm(price ~ men + lo + border_county + 
     as.factor(year) + border_county:as.factor(year) + as.factor(brand) +
     iv1 + iv2 + iv3 + iv4 + iv8 + iv9 + iv10, df2)

#Overall Ealsticity-------------------------------------------------------------
dbReadTable(results2, "avg_elas_long") %>%
  inner_join(
    df2 %>%
      select(brand, category, inShare) %>%
      group_by(brand, category) %>%
      dplyr::mutate(inShare = mean(inShare)) %>%
      inner_join(dbReadTable(results2, "brand_map_and_mask"), by = "brand") %>%
      select(MASKED_NAME, broader_prefix, inShare) %>%
      mutate(broader_prefix = 
               ifelse(broader_prefix < 10, paste0("0", broader_prefix), broader_prefix)) %>%
      mutate(num_name = paste0(broader_prefix, ". ", MASKED_NAME)) %>%
      select(num_name, category, inShare) %>%
      group_by(num_name) %>%
      distinct() %>%
      group_by(num_name, category) %>%
      dplyr::summarize(inShare = sum(inShare)),
    by = "num_name"
  ) %>%
  na.omit() %>%
  mutate(e = elas*inShare) %>%
  group_by(category) %>%
  dplyr::summarize(e = sum(e))

#MC and Elas Cov----------------------------------------------------------------
dbReadTable(results2, "avg_elas_long") %>%
  filter(elas < 0) %>%
  inner_join(
    sdf2 %>%
      select(cdid, brand, inShare, marginal_cost) %>%
      inner_join(dbReadTable(results2, "brand_map_and_mask"), by = "brand") %>%
      select(cdid, MASKED_NAME, broader_prefix, inShare, marginal_cost) %>%
      mutate(broader_prefix = 
               ifelse(broader_prefix < 10, paste0("0", broader_prefix), broader_prefix)) %>%
      mutate(num_name = paste0(broader_prefix, ". ", MASKED_NAME)) %>%
      select(cdid, num_name, inShare, marginal_cost) %>%
      group_by(num_name) %>%
      distinct() %>%
      group_by(num_name, cdid) %>%
      dplyr::summarize(inShare = mean(inShare), marginal_cost = mean(marginal_cost)),
    by = "num_name"
  ) %>%
  group_by(num_name) %>%
  dplyr::summarize(
    elas = mean(elas, na.rm = T),
    marginal_cost = mean(marginal_cost, na.rm = T)) -> temp

cov(temp$elas, temp$marginal_cost, use = "complete.obs")
