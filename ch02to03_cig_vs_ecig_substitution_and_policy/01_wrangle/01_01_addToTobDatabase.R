#This script is for adding data to the Tobacco Database. The function used
#is setup so files that are already in the database are not re-added. Only
#files that are determined to not be in the database will be added.

#Note to Project Members: DO NOT USE THIS TO BUILD THE DATABASE YOURSELF
#FROM SCRATCH. USE THE ALREADY CREATED DATABASE. ADDITONALLY, PLEASE
#DO NOT ADD ANYTHING TO THE MAIN DATABASE STORED AT X WITHOUT EXPLICT
#TEAM PERMISSION AND AGREEMENT. IN THE EVENT IT IS AGREED TO ADD A FILE,
#YOU SHOULD STILL GET PERMISSION BEFORE OVERWRITING THE MAIN DATABASE FILE.

if(exists("addToTobDataBase") == FALSE){
  source("code/00_setup/00_func_addToTobDatabase.R")
}

#Add RMS Data to the Database
addToTobDataBase(niq.rms, "tsv", "rms_")

#Add HMS Data to the Database
addToTobDataBase(niq.hms, "tsv", "hms_")