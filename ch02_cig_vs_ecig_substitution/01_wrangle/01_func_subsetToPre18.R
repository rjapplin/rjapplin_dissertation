subsetToPre18Sample <- function(db, year, productID){
  
  if(year < 2018){
    stop("This function is intended to only subset data from 2018 and onward
         to the store sample for the 2006 to 2017 period.")
  }
  
  
  storesToKeep <- read_tsv(
    paste0(direcs$dat.der, "nielsen/stores_06_17_sample.tsv")
  )$stores
  
  dbExecute(db,
            glue_sql(
              paste0("CREATE TABLE rms_mg_pre18_", productID, "_", year, " AS 
                      SELECT * FROM rms_mg_", productID, "_", year,
                      " WHERE store_code_uc in ({storesToKeep*})"
              )
            )
  )
          
  
}
