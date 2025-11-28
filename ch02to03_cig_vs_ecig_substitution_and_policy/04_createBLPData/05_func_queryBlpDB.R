queryBlpDB <- function(P, S, X, IV, D, B, where, db){
  
  #-Data Dictionary-------------------------------------------------------------
  varLocations <- data.frame(
    
    variable = c("price_wvol", "price_wsale", "share_vol", "share_sal",
                 "out_vol", "out_sal",
                 "menthol", "tobacoo", "f_other", "f_unknown",
                 "device", "filter", "kit", "nofilter", "refill",
                 "t_unknown", "t_other", "low", "mid", "s_other",
                 "regular", "ultralow", "strong", "s_unknown",
                 "IV_f_fruit", "IV_f_menthol", "IV_f_tobacco", "IV_f_other",
                 "IV_f_NA", "IV_t_device", "IV_t_filter", "IV_t_kit",
                 "IV_t_nofilter", "IV_t_refill", "IV_t_NA", "IV_t_other",
                 "IV_s_low", "IV_s_mid", "IV_s_other", "IV_s_regular",
                 "IV_s_ultralow", "IV_s_strong", "IV_s_NA", "IV_tax1",
                 "IV_tax2", "IV_tax3", "IV_tax_avg",
                 "feature_wvol", "feature_wsale", "display_wvol", "display_wsale",
                 "brand_descr", "subparent", "parent", "category",
                 "year", "quarter", "fips_state_code", "fips_county_code",
                 "market_size_vol", "market_size_real"),
    
    
    location = c("movement", "movement", "shares", "shares", "shares", "shares",
                "character", "character", "character", "character",
                "character", "character", "character", "character", "character",
                "character", "character", "character", "character", "character",
                "character", "character", "character", "character",
                "ivs", "ivs", "ivs", "ivs",
                "ivs", "ivs", "ivs", "ivs",
                "ivs", "ivs", "ivs", "ivs",
                "ivs", "ivs", "ivs", "ivs",
                "ivs", "ivs", "ivs", "ivs",
                "ivs", "ivs", "ivs",
                "feature", "feature", "feature", "feature",
                "brands", "brands", "brands", "brands",
                "markets", "markets", "markets", "markets",
                "market_size", "market_size"),
    
    mergeOnBrand = c(rep(1, 55), rep(0, 6)),
    
    mergeOnMarket = c(rep(1, 51), rep(0, 4), rep(1, 6))
    
  )
  
  #-Generate SQL Statement------------------------------------------------------
  sqlStatement <- paste0(
    "SELECT M.marketid, ", paste0(B, collapse = ", "), ", ", paste0(P, collapse = ", "), ", ",
    paste0(S, collapse = ", "), ", ", paste0(X, collapse = ", "), ", ",
    paste0(IV, collapse = ", "), " ", 
    "FROM 
      (SELECT marketid, brand_code_uc, ", P, " FROM movement) AS M
     INNER JOIN 
      (SELECT marketid, brand_code_uc,", paste0(S, collapse = ", "), " FROM shares) AS S
        ON M.marketid = S.marketid AND M.brand_code_uc = S.brand_code_uc
     INNER JOIN
      (SELECT marketid, brand_code_uc,", paste0(X, collapse = ", "), " FROM character) AS X
        ON S.marketid = X.marketid AND S.brand_code_uc = X.brand_code_uc
     INNER JOIN
      (SELECT marketid, brand_code_uc,", paste0(IV, collapse = ", "), " FROM ivs) AS I
        ON X.marketid = I.marketid AND X.brand_code_uc = I.brand_code_uc
     INNER JOIN
      (SELECT marketid,", paste0(D, collapse = ", "), " FROM markets) AS D
        ON I.marketid = D.marketid 
     INNER JOIN
      (SELECT brand_code_uc,", paste0(B, collapse = ", "), " FROM brands) AS B
        ON M.brand_code_uc = B.brand_code_uc ",
    paste0(where)
  )
  
  #-Run Query Using Statement---------------------------------------------------
  return(
    dbGetQuery(main, sqlStatement)
  )
  
  
}
