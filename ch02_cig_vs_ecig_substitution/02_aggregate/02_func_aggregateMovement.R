aggregateMovement <- function(year, productID, freq = "week", bygroup = NULL, 
                              x, xNames = NULL){
  
  writeSQL <- function(year, productID, freq, bygroup, x, xNames){
  
    writeSelect <- function(freq, bygroup, x, xNames){
      
      selectFreq<- paste("SELECT", freqMap[[freq]])
      selectGroup <- paste0(paste(bygroup, collapse = ", "), ",")
      
      if(!(is.null(xNames))){
        selectVars <- mapply(
          function(var, varname){
            paste(var, "AS", varname)
          }, x, xNames) 
      } else {
        selectVars <- paste(x, collapse = ", ")
      }
      
      selectVars <- paste(selectVars, collapse = ", ")
      
      if(is.null(bygroup)){
        selectStatement <- paste(selectFreq, selectVars)
      } else {
        selectStatement <- paste(selectFreq, selectGroup, selectVars)
      }
      
      return(selectStatement)
      
    }
    
    freqMap <- list("week_end AS week,", 
                    "SUBSTRING(week_end, 5, 2) AS month,",
                    "panel_year AS year,")
    names(freqMap) <- c("week", "month", "year")
    selectStatement <- writeSelect(freq, bygroup, x, xNames)
    
    
    fromStatement <- paste0("FROM rms_mg_", productID, "_", year)
    
    #Group Statement
    if(is.null(bygroup)){
      groupStatement <- paste("GROUP BY",
                              paste0(gsub(" AS.*", "", freqMap[[freq]]))
                              )
    } else {
      groupStatement <- paste("GROUP BY", 
                              paste0(gsub(" AS.*", "", freqMap[[freq]]), ","),
                              paste(bygroup, collapse = ", "))
    }
      
    return(paste(selectStatement, fromStatement, groupStatement))
      
  }
  sqlStatement <- writeSQL(year, productID, freq, bygroup, x, xNames)
  dbGetQuery(tob, sqlStatement)
  
}

  
  