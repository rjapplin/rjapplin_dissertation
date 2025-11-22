getMainQuery <- function(db, insideShareVar, outsideShareVar, priceVar,
                         xVars, ivVars, start_year = 2013, end_year = 2020){
  
  #' @description
    #' This function generates and returns a valid SQL query to make
    #' extracting analysis ready data from the tobacco database easy and
    #' repeatable.
  #' 
  #' @param db The database connection to the Tobacco database.
  #' @param insideShareVar The inside share variable that comes from the shares
    #' table. This should be either 'share_sal' (market shares computed using
    #' sales) or 'share_vol' (market shares computed using volume/quantity sold)
  #' @param outsideShareVar The outside share variable that comes from the
    #' shares table. This should be either 'out_sal' (1 minus the sum of
    #' share_sal by marketid) or 'out_vol' (1 minus the sum of share_vol by
    #' marketid)
  #' @param priceVar The price variable that comes from the movement table.
    #' This should be either price_wvol (volume weighted average price for
    #' a brand in a given market) or price_wsale (sales weighted average price for
    #' a brand in a given market).
  #' @param xVars Product characteristics from the X_Count table. Refer to
    #' other documentation for information on the variables available in this
    #' table.
  #' @param ivVars Instrumental variables from the IV_Count table. Refer to
    #' other documentation for information on the variables available in this
    #' table.
    #' @param start_year Optional - defaults to 2013. Used to subset to a specific
    #' year range. The data runs from 2006 to 2020 - but certain data is only
    #' available starting 2013.
    #' @param end_year Optional - defaults to 2020. Used to subset to a specific
    #' year range. The data runs from 2006 to 2020 - but certain data is only
    #' available starting 2013.
  
  #Some Defensive Programming to Avoid Errors Later On--------------------------
  
  #Make sure year range is valid
  if(!(start_year %in% 2006:2020 & end_year %in% 2006:2020)){
    stop("The start_year and end_year must be between 2006 and 2020 inclusive.")
  }
  
  if(end_year < start_year){
    stop("The end_year must be greater than or equal to start_year.")
  }
  
  #Make sure inside_share, outside share variables, and price variables are valid
  if(!(insideShareVar %in% c("share_sal", "share_vol") 
       & outsideShareVar %in% c("out_sal", "out_vol")
       & priceVar %in% c("price_wvol", "price_wsale"))){
    stop("insideShareVar must equal either share_sal or share_vol, 
         outsideShareVar must equal either out_sal or out_vol,
         and priceVar must equal either price_wvol or price_wsale")
  }
  
  #Make sure X variables are valid
  ifelse(xVars %in% dbListFields(main, "X_Count"), "", 
         {invalidX <- xVars[which(!(xVars %in% dbListFields(main, "X_Count")))]
           stop(paste0("The following xVars are invalid/not available in X_Count: ",
                       paste0(invalidX, collapse = ", ")))
           })
  
  #Make sure IV variables are valid
  ifelse(ivVars %in% dbListFields(main, "IV_Count"), "", 
         {invalidIV <- ivVars[which(!(ivVars %in% dbListFields(main, "IV_Count")))]
         stop(paste0("The following ivVars are invalid/not available in IV_Count: ",
                     paste0(invalidIV, collapse = ", ")))
         })
  
  
  #Write SQL Query for extraction-----------------------------------------------
  paste0(
    
    #Select brand and market ids which denote correspond to rows
    "SELECT MOVE.brand_code_uc AS brandid, MOVE.marketid AS market, ",
    
    #Select time and location information that links to marketids
    "MARKET.year, MARKET.quarter, MARKET.fips_state_code AS state, 
    MARKET.fips_county_code AS county, ",
    
    #Select brand information that links to brand_code_uc
    "BRAND.brand_descr AS brand, BRAND.subparent, BRAND.parent, 
    BRAND.category, ",
    
    #Select price variable
    "MOVE.", priceVar, " AS price, ",
    
    #Select share variables
    "SHARE.", paste0(insideShareVar), " AS inShare, SHARE.", 
    paste0(outsideShareVar), 
    " AS outShare ",
    
    #Select Product Characteristics
    ifelse(is.null(xVars), "", paste0(", X.", paste0(xVars, collapse = ", X."), 
                                      " ")),
    
    #Select IVs
    ifelse(is.null(ivVars), "", paste0(", IV.", 
                                       paste0(ivVars, collapse = ", IV."), " ")),
    
    #FROM Statment
    " FROM movement AS MOVE ",
    
    #Joins
    "INNER JOIN shares AS SHARE ON MOVE.brand_code_uc = SHARE.brand_code_uc AND 
    MOVE.marketid = SHARE.marketid  
    INNER JOIN X_COUNT AS X ON MOVE.brand_code_uc = X.brand_code_uc 
    AND MOVE.marketid = X.marketid  
    INNER JOIN IV_COUNT AS IV ON MOVE.brand_code_uc = IV.brand_code_uc 
    AND MOVE.marketid = IV.marketid 
    INNER JOIN markets AS MARKET ON SHARE.marketid = MARKET.marketid 
    INNER JOIN brands AS BRAND ON SHARE.brand_code_uc = BRAND.brand_code_uc ",
    
    #Subset to start_year to end_year range
    "WHERE MARKET.year >= ", start_year, " AND MARKET.year <= ", end_year
    
    
  ) %>%
    #Execute Query using database db
    dbGetQuery(db, .) -> dataToReturn
  
  #Do Diagnostics on the Return Data to give heads up for potential issues------
  has_missing <- identical(na.omit(dataToReturn), dataToReturn) == FALSE
  if(has_missing == TRUE){
    
    #Identify columns with missing data
    cols_with_missing <-names(which(sapply(dataToReturn, anyNA)))
    warning(paste0("The following columns have missing values: ", 
                   paste0(cols_with_missing, collapse = ", ")))
    
  }
  
  #Check for the Existence of Constant Columns (Certain Estimation Procedures
  #will return an error for constant columns. This may happen when
  #subsetting to a very specific time period and/or market where some variables
  #may not have any variability)
  const_cols <- names(
    dataToReturn[, 
                 sapply(dataToReturn, 
                        function(v){
                          #Constant characters do not matter
                          if(is.character(v)){
                            FALSE
                            } else {
                              #constant column means variance of column v is 0
                              var(v, na.rm=TRUE)==0
                              }
                          }
                        )
                 ] 
    )
  

  if(length(const_cols) == 0){
    
  } else {
    #If selecting a single year of data, year will be inside of const_cols - 
    #this removes it
    const_cols <- const_cols[which(const_cols != "year")]
    warning(paste0("The following columns are constant in the return data: ",
                   paste0(const_cols, collapse = ", ")))
  }
  
  return(dataToReturn)
  
}








