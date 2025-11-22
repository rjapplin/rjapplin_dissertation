
if(exists("genIndexes") == FALSE){
  source("code/00_setup/00_func_genIndexes.R")
}

#Generate Indexes on RMS Files-------------------------------------------------- 

  #Generate Index on RMS Store Files (Available for all years - 2006 to 2020)
  genIndexes(db = tob, source = "RMS", fileType = "stores",
             startYear = 2006, endYear = 2020,
             key_vars = list(c("store_code_uc"), c("store_code_uc", "year")))
  
  #Generate Index on RMS Products File (Only one master file)
  genIndexes(db = tob, source = "RMS", fileType = "products",
             key_vars = list(c("upc", "upc_ver_uc"), c("product_module_code")))
  
  #Generate Index on RMS RMS versions (Available for all years)
  genIndexes(db = tob, source = "RMS", fileType = "rms_versions",
             startYear = 2006, endYear = 2020,
             key_vars = 
               list(c("upc"), c("upc", "upc_ver_uc"), c("upc", "upc_ver_uc",
                                                        "panel_year"))
             )
  
  #Generate Indexes on RMS Product Extra Files (Available for all years)
  genIndexes(db = tob, source = "RMS", fileType = "products_extra",
             startYear = 2006, endYear = 2020,
             key_vars = list(c("upc"), c("upc", "upc_ver_uc"),
                             c("upc", "upc_ver_uc", "panel_year"))
            )
  
  #Generate Indexes on Brand Variations File (only one master file)
  genIndexes(db = tob, source = "RMS", fileType = "brand_variations",
             key_vars = list(c("brand_code_uc"))
             )
  
  #Generate Indexes on Movement Files
  
    #For Traditional Cigarettes (Available for all years) - Takes about 2
    #hours but only needs to be done once
    genIndexes(db = tob, source = "RMS", fileType = prodID$tcigte,
               startYear = 2006, endYear = 2020,
               key_vars = 
                 list(c("upc"),
                      c("store_code_uc"))
               )
    
    #For E-Cigarettes (Only available from 2013)
    genIndexes(db = tob, source = "RMS", fileType = prodID$ecigte,
               startYear = 2013, endYear = 2020,
               key_vars = list(c("upc"),
                               c("store_code_uc"))
               )
  
    
