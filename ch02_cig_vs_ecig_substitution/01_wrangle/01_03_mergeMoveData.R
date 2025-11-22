#This file produces 

if(exists("addToTobDataBase") == FALSE){
  source("code/01_wrangle/01_func_mergeMoveData.R")
}


#Merge 7460 Movement Files
mergeMoveData(tob, productID = prodID$tcigte, year = 2006:2020,
              overwrite = TRUE)

#Merge 7467 Movement Files
mergeMoveData(tob, productID = prodID$ecigte, year = 2013:2020,
              overwrite = TRUE)
