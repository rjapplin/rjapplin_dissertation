#Setup--------------------------------------------------------------------------
library(tidyverse)
library(DBI)
library(RSQLite)
library(RSelenium)
library(rvest)
library(fuzzyjoin)
library(tools)
library(stringdist)
library(cluster)
library(stringr)
library(tm)
library(udpipe)
library(hunspell)


#Source Functions---------------------------------------------------------------
source("iadb_engineer/functions.R")

#Connect to Database------------------------------------------------------------
iadb <- dbConnect(SQLite(), dbname = "ia.db")









