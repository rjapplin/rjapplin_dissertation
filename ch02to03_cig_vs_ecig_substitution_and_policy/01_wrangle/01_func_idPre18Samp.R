
idPre18Samp <- function(years, prodIDs){
  
  stores <- vector()
  
  for(year in years){
    
    for(productID in prodIDs){
      
      stores <- union(stores,
                       (dbGetQuery(tob,
                                   paste0("SELECT DISTINCT store_code_uc
                                          FROM rms_mg_",
                                               productID, "_", year)
                                   )
                        )$store_code_uc
                       ) 
      
    }
    
  }
  
  stores <- unique(stores)
  return(stores)
  
}
    
    
