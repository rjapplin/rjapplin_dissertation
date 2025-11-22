df <- dbReadTable(results, "data.pre_est")
df %>%
  inner_join(
    dbReadTable(main, "X_Count") %>% select(brand_code_uc, marketid, prodN) %>%
      inner_join(
        dbReadTable(main, "brands") %>% select(brand_code_uc, brand_descr),
        "brand_code_uc") %>% select(marketid, brand_descr, prodN) %>%
      dplyr::rename(market = marketid,
                    brand = brand_descr),
    by = c("market", "brand")
    
  ) %>%
  mutate(tob = tobacco/prodN, men = menthol/prodN, lo = low/prodN,
         reg = regular/prodN, mi = mid/prodN, ul = ultralow/prodN, 
         reg = regular/prodN) %>%
  select(market, brand, outShare, inShare, category, price, tob, men, lo, reg, mi, ul, 
         reg, prodN) %>%
  group_by(market) %>%
  dplyr::mutate(
    iv1 = sum(tob) - tob,
    iv2 = sum(men) - men,
    iv3 = sum(lo) - lo,
    iv4 = sum(reg) - reg,
    iv5 = sum(mi) - mi,
    iv6 = sum(reg) - reg,
    iv7 = sum(ul) - ul,
    npercent= prodN/sum(prodN)
  ) %>%
  dplyr::mutate(
    iv8 = sum(npercent) - npercent,
  ) -> df2

df2 <- df2 %>%
  group_by(market) %>%
  dplyr::mutate(cdid = cur_group_id())
rm(df)

options(tigris_use_cache = TRUE)
us_counties <- counties(cb = TRUE, resolution = "5m") %>%
  dplyr::rename(fips_code = GEOID)
us_states <- states(cb = TRUE, resolution = "5m") %>%
  filter(!(NAME %in% c("Puerto Rico", "Guam", "American Samoa", "Alaska",
                       "United States Virgin Islands", 
                       "Commonwealth of the Northern Mariana Islands",
                       "Hawaii", "Alaska")))
neighbors <- st_touches(us_counties, sparse = TRUE)
is_border_county <- sapply(seq_along(neighbors), function(i) {
  # Get the state of the current county
  current_state <- us_counties$STATEFP[i]
  
  # Get the states of neighboring counties
  neighbor_states <- us_counties$STATEFP[neighbors[[i]]]
  
  # Check if any neighboring counties belong to a different state
  any(neighbor_states != current_state)
})
us_counties <- us_counties %>%
  mutate(border_county = is_border_county)  

df2 <- df2 %>%
  inner_join(dbReadTable(main, "markets") %>% select(marketid, fips_state_code, 
                                                     fips_county_code) %>%
               dplyr::rename(market = marketid), by = "market") %>%
  mutate(fips_code = sprintf("%02d%03d", fips_state_code, fips_county_code)) %>%
  left_join(us_counties %>% select(fips_code, border_county), 
            by = "fips_code") %>%
  select(-geometry) 

df2 <- df2 %>%
  inner_join(dbReadTable(main, "markets") %>% select(year, quarter, marketid) %>%
               dplyr::rename(market = marketid), by = "market")

df2 <- df2 %>%
  inner_join(
    read_tsv("data/derived/taxes/tob_taxes_qtr_master.tsv") %>% 
      filter(product %in% c("Cigarette", "E-Cigarette")) %>% 
      mutate(tax = ifelse(productID == 7460, taxAmount, taxAmount_20mu_tv)) %>% 
      select(year, quarter, state, productID, tax) %>% 
      mutate(category = ifelse(productID == 7460, 7467, 7460)) %>%
      select(-productID) %>%
      dplyr::rename(fips_state_code = state),
    by = c("year", "quarter", "fips_state_code", "category")
  ) 

df2 <- na.omit(df2)
df2 <- df2 %>% 
  mutate(price = ifelse(category == 7467, price*20, price))
rm(neighbors, us_counties, us_states, is_border_county, markets)
gc()

tax <- df2 %>% select(category, cdid, tax) %>%
  distinct() %>%
  pivot_wider(names_from = "category", values_from = "tax", names_prefix = "t_") 

dbReadTable(main, "movement") %>% 
  inner_join(dbReadTable(main, "shares") %>%
               select(marketid, brand_code_uc, share_vol, out_vol),
             by = c("marketid", "brand_code_uc")) %>%
  inner_join(dbReadTable(main, "brands") %>% 
               select(brand_code_uc, brand_descr) %>%
               dplyr::rename(brand = brand_descr)) %>%
  select(marketid, brand, brand_mkt_vol, price_wvol,price_wsale, share_vol, out_vol) %>%
  dplyr::rename(market = marketid) %>%
  inner_join(df2, by = c("brand", "market")) %>% 
  mutate(price_alt = ifelse(category == 7467, price_wvol*20, price_wvol)) %>%
  inner_join(tax, by = c("cdid")) %>%
  mutate(oppose_tax = ifelse(tax == t_7467, t_7460, t_7467)) %>%
  arrange(brand, fips_state_code, fips_county_code, year, quarter) %>%
  group_by(brand, fips_state_code, fips_county_code) %>%
  dplyr::mutate(iv9 = lag(price_alt)*(tax - lag(tax)),
                iv10 = lag(price_alt)*(oppose_tax - lag(oppose_tax))) %>%
  dbWriteTable(results, "data.ready_for_est", ., overwrite = T)
