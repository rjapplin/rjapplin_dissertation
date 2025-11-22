#This function provides a means for automating the creation of indexes
#for the tables in the database. It supports generating multiple indexes
#for tables at once.

genIndexes <- function(db, source, fileType,
                                   startYear = 2006, endYear = 2020,
                                   key_vars, 
                                   mg = TRUE){
  
  genPrefix <- function(source, fileType, mg){
    if(source == "RMS"){
      if(mg == TRUE){
        prefix = paste0("rms_mg_", fileType)
      } else {
        prefix = paste0("rms_", fileType)
      }
    } else if(source == "HMS"){
      prefix = paste0("hms_", fileType)
    }
    return(prefix)
  }
  genTableNames <- function(prefix, fileType, startYear, endYear){
   
   if(fileType %in% c("products", "brand_variations", "retailers")){
     tables <- paste0(prefix)
   } else if(!(prefix %in% c("products", " brand_variations", "retailers"))){
     tables <- paste0(prefix, "_", startYear:endYear)
   }
   return(tables)
  }
  genIndexName  <- function(kv, tab){
    
    paste0("i_", tab, "_", paste(kv, collapse = "_"))
    # lapply(key_vars,
    #        function(kv){
    #          paste0("i_", tab, "_", paste(kv, collapse = "_"))
    #        }
    # )
    
  }
  writeSQL <- function(kv, tab){
    
    paste("CREATE INDEX", genIndexName(kv, tab), "ON", tab, paste0("(",
          paste(kv, collapse = ", "), ")")
          )
    
    
  }
  generateIndexes <- function(db, tables, key_vars){
    
    for(tab in 1:length(tables)){
      
      for(kv in 1:length(key_vars)){
        
        #print(writeSQL(key_vars[[kv]], tables[tab]))
        sql_state <- writeSQL(key_vars[[kv]], tables[tab])
        dbExecute(db,
                  sql_state)
        
      }
      
    }
    
  }
  
  
  prefix <- genPrefix(source, fileType)
  tables <- genTableNames(prefix, fileType, startYear, endYear)
  generateIndexes(db, tables, key_vars)


}
