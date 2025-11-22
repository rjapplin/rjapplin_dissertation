transCharacter <- function(db, chartable, nm){
  
  X <- dbReadTable(db, chartable) %>%
    mutate(fruit = fruit*prodN,
           menthol = menthol*prodN,
           tobacco = tobacco*prodN,
           f_other = f_other*prodN,
           f_unknown = f_unknown*prodN,
           device = device*prodN,
           filter = filter*prodN,
           kit = kit*prodN,
           nofilter = nofilter*prodN,
           refill = refill*prodN,
           t_unknown = t_unknown*prodN,
           t_other = t_other*prodN,
           low = low*prodN,
           mid = mid*prodN,
           s_other = s_other*prodN,
           regular = regular*prodN,
           ultralow = ultralow*prodN,
           strong = strong*prodN,
           s_unknown = s_unknown*prodN)
  
  dbWriteTable(db, nm, X, overwrite = TRUE)
  
}

transIV <- function(db, xtab, ivtab, nm){
  
  iv_counts <- dbReadTable(main, xtab) %>%
    group_by(marketid) %>%
    dplyr::mutate(IV_fruit = sum(fruit) - fruit,
                  IV_menthol = sum(menthol) - menthol,
                  IV_tobacco = sum(tobacco) - tobacco,
                  IV_f_other = sum(f_other) - f_other,
                  IV_f_unknown = sum(f_unknown) - f_unknown,
                  IV_device = sum(device) - device,
                  IV_filter = sum(filter) - filter,
                  IV_kit = sum(kit) - kit,
                  IV_nofilter = sum(filter) - nofilter,
                  IV_refill = sum(refill) - refill,
                  IV_t_unknown = sum(t_unknown) - t_unknown,
                  IV_t_other = sum(t_other) - t_other,
                  IV_low = sum(low) - low,
                  IV_mid = sum(mid) - mid,
                  IV_s_other = sum(s_other) - s_other,
                  IV_regular = sum(regular) - regular,
                  IV_ultralow = sum(ultralow) - ultralow,
                  IV_strong = sum(strong) - strong,
                  IV_s_unknown = sum(s_unknown) - s_unknown,
                  IV_total = n - prodN) %>%
    select(brand_code_uc, marketid, IV_fruit, IV_menthol, IV_tobacco, IV_f_other,
           IV_f_unknown, IV_device, IV_filter, IV_kit,
           IV_nofilter, IV_refill, IV_t_unknown,
           IV_t_other, IV_low, IV_mid, IV_s_other,
           IV_regular, IV_ultralow, IV_strong, IV_s_unknown,
           IV_total)
  
  dbReadTable(main, iv_tab) %>%
    select(brand_code_uc, marketid, IV_tax1, IV_tax2, IV_tax3, IV_tax_avg) %>%
    inner_join(iv_counts, c("brand_code_uc", "marketid")) -> ivs
  
  dbWriteTable(main, nm, ivs)
  
}

