#Setup--------------------------------------------------------------------------
library(tidyverse)
library(DBI)
library(RSQLite)
library(fredr)
library(reshape2)
library(psych)
library(glmnet)
library(gridExtra)
library(ivreg)
library(lmtest)
library(lfe)
library(fixest)
library(did)
library(CausalImpact)
library(plm)

#Functions----------------------------------------------------------------------
source("functions_utility.R")
source("functions_plots.R")
source("functions_tables.R")

#Connect to the IADB------------------------------------------------------------
if(file.exists(read_file(".iadb_path"))){
  iadb  <-dbConnect(SQLite(), dbname = read_file("iadb_path"))
}

iaan <- dbConnect(SQLite(), dbname = "iaan.db")

if(exists("iadb")){
  if("DATA.DIM_LOC" %in% dbListTables(iaan)){
    
  } else {
    dbWriteTable(iaan, "DATA.DIM_LOC", dbReadTable(iadb, "META.D_LOCATION"))
  }
}

#Get CPI Data-------------------------------------------------------------------
#Connect to FRED
fredr_set_key(read_file(".fred_key"))
if(!("DATA.CPI" %in% dbListTables(iaan))){

  cpi <- fredr("CPIAUCNS",
               observation_start = as.Date("2014-01-01"),
               observation_end = as.Date("2024-12-31")) %>%
    select(date, value) %>%
    dplyr::rename(cpi = value) %>%
    mutate(year = as.character(year(date)),
           month = month(date)) %>%
    mutate(month = ifelse(month <= 9, paste0("0", month), month)) %>%
    select(year, month, cpi) %>%
    group_by(year) %>%
    dplyr::mutate(mean_cpi = mean(cpi)) %>%
    ungroup() %>%
    dplyr::mutate(
      new_base_cpi = max(ifelse(year == 2015, mean_cpi, NA), na.rm = T)) %>%
    mutate(cpi_b2015 = (cpi / new_base_cpi)*100) %>%
    select(year, month, cpi, cpi_b2015)
  
  dbWriteTable(iaan,
               "DATA.CPI",
               cpi)
} else {
  cpi <- dbReadTable(iaan, "DATA.CPI")
}
