genBLPDataStructure01_aggregate <- function(productID, year, freq = "quarter",
                                            method = "weight", db){
  
  if(year >= 2018){
    prefix <- "rms_mg_pre18_"
  } else if(year < 2019){
    prefix <- "rms_mg_"
  }
  
  writeSQL <- function(productID, year, freq, method){
    
    if(method == "weight"){
      
      if(freq == "quarter"){
        
        selectStatement <- paste0("
        SELECT A.upc, A.upc_ver_uc, A.fips_state_code,
        A.fips_county_code, A.brand_code_uc,
        SUM(((price/prmult)/(multi*size1_amount))*((units*size1_amount*multi)/qtvol)) AS price_wvol,
        SUM(((price/prmult)/(multi*size1_amount))*(((price/prmult)*units)/qtsales)) AS price_wsale,
        SUM((units*size1_amount*multi)) AS volume,
        SUM((price/prmult)*units) AS sales,
        SUM(display*((units*size1_amount*multi)/qtvol)) AS display_wvol,
        SUM(feature*((units*size1_amount*multi)/qtvol)) AS feature_wvol,
        SUM(display*(((price/prmult)*units)/qtsales)) AS display_wsale,
        SUM(feature*(((price/prmult)*units)/qtsales)) AS feature_wsale, ",
                                  "A.", freq
        )
        
        mainFromStatement <- paste0("FROM (SELECT *,
          (COALESCE(NULLIF((SUBSTR(week_end, 5, 2) - 1) / 3, 0), 0) + 1) AS quarter 
          FROM ", prefix, productID, "_", year, " ", ") AS A")
        
        joinStatement <- paste0(
          "
          INNER JOIN",
          "(SELECT (COALESCE(NULLIF((SUBSTR(week_end, 5, 2) - 1) / 3, 0), 0) + 1) AS quarter,
           upc, upc_ver_uc, fips_state_code, fips_county_code,
           SUM(units*size1_amount*multi) AS qtvol,
           SUM((price/prmult)*units) AS qtsales
           FROM ", prefix, productID, "_", year, " ",
          "GROUP BY ", freq, ", upc, upc_ver_uc, fips_state_code,
           fips_county_code) AS B
           "
        )
        onStatement <- " ON A.quarter = B.quarter AND A.upc = B.upc
        AND A.upc_ver_uc = B.upc_ver_uc AND A.fips_state_code = B.fips_state_code
        AND A.fips_county_code = B.fips_county_code"
        groupStatement <- "GROUP BY A.upc, A.upc_ver_uc, A.quarter,
        A.fips_state_code, A.fips_county_code"
        
        sqlStatement <- paste(selectStatement, mainFromStatement, joinStatement,
                              onStatement, groupStatement, sep = " ")
        
      } else if(freq == "month"){
        
        selectStatement <- paste0("
        SELECT A.upc, A.upc_ver_uc, A.fips_state_code,
        A.fips_county_code, A.brand_code_uc,
        SUM((price/prmult)/(multi*size1_amount)*((units*size1_amount*multi)/mtvol)) AS price_wvol,
        SUM(((price/prmult)/(multi*size1_amount))*(((price/prmult)*units)/mtsales)) AS price_wsale,
        SUM((price/prmult)*units) AS sales,
        SUM((units*size1_amount*multi)) AS volume,
        SUM(display*((units*size1_amount*multi)/mtvol)) AS display_wvol,
        SUM(feature*((units*size1_amount*multi)/mtvol)) AS feature_wvol,
        SUM(display*(((price/prmult)*units)/mtsales)) AS display_wsale,
        SUM(feature*(((price/prmult)*units)/mtsales)) AS feature_wsale, ",
                                  "A.", freq
        )
        
        mainFromStatement <- paste0("FROM (SELECT *,
          SUBSTR(week_end, 5, 2) AS month
          FROM ", prefix, productID, "_", year, " ", ") AS A")
        
        joinStatement <- paste0(
          "
          INNER JOIN",
          "(SELECT SUBSTR(week_end, 5, 2) AS month,
          upc, upc_ver_uc, fips_state_code, fips_county_code,
          SUM(units*size1_amount*multi) AS mtvol,
          SUM((price/prmult)*units) AS mtsales
          FROM ", prefix, productID, "_", year, " ",
          "GROUP BY ", freq, ", upc, upc_ver_uc, fips_state_code,
          fips_county_code) AS B"
        )
        
        onStatement <- " ON A.month = B.month AND A.upc = B.upc
        AND A.upc_ver_uc = B.upc_ver_uc AND A.fips_state_code = B.fips_state_code
        AND A.fips_county_code = B.fips_county_code"
        
        groupStatement <- "GROUP BY A.upc, A.upc_ver_uc, A.month,
        A.fips_state_code, A.fips_county_code"
        
        sqlStatement <- paste(selectStatement, mainFromStatement, joinStatement,
                              onStatement, groupStatement, sep = " ")
      }
      
      
    } else if(method == "arith"){
      
      if(freq == "quarter"){
        selectStatement <- "SELECT upc, upc_ver_uc, fips_state_code,
      fips_county_code, brand_code_uc,
      AVG((price/prmult)/(multi*size1_amount)) AS price,
      AVG(feature) AS feature,
      AVG(display) AS display,
      SUM((price/prmult)*units) AS sales,
      SUM((units*size1_amount*multi)) AS volume,
      (COALESCE(NULLIF((SUBSTR(week_end, 5, 2) - 1) / 3, 0), 0) + 1) AS quarter
      "
        fromStatement <- paste0("FROM ", prefix, productID, "_", year)
        groupStatement <- paste0("GROUP BY upc, upc_ver_uc, quarter,
                               fips_state_code, fips_county_code")
      } else if(freq == "month"){
        selectStatement <- "SELECT upc, upc_ver_uc, fips_state_code,
      fips_county_code, brand_code_uc,
      AVG((price/prmult)/(multi*size1_amount)) AS price,
      SUM((price/prmult)*units) AS sales,
      SUM((units*size1_amount*multi)) AS volume,
      AVG(feature) AS feature,
      AVG(display) AS display,
      SUBSTR(week_end, 5, 2) AS month
      "
        fromStatement <- paste0("FROM ", prefix, productID, "_", year)
        groupStatement <- paste0("GROUP BY upc, upc_ver_uc, month,
                               fips_state_code, fips_county_code")
      }
      
      sqlStatement <- paste(selectStatement, fromStatement, groupStatement,
                            sep = " ")
      
    }
    
    return(sqlStatement)
    
  }
  
  dbGetQuery(db,
             writeSQL(productID, year, freq, method)
  )
             
  
}
