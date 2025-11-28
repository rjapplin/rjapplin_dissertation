#Required Packages
library(tidyverse)
library(DBI)
library(RSQLite)

#Connect to Database
iadb <- dbConnect(SQLite(), dbname = "ia.db")

#List Tables and Views
dbListTables(iadb)

#View Metadata About Raw CSV File Source
dbReadTable(iadb, "META.V_FILE_INFO")

#Integrated Data Dictionary
dbReadTable(iadb, "META.M_FIELD_DICTIONARY")

#View Fields in Main Table of Interest
dbListFields(iadb, "CURATED.TAB_LISTING_ANALYTICS")

#Count Rows in Main Table of Interest
dbGetQuery(iadb, "SELECT COUNT(*) FROM [CURATED.TAB_LISTING_ANALYTICS]")

#Select a Row of Data Out of CURATED.TAB_LISTING_ANALYTICS
dbGetQuery(iadb, "SELECT * FROM [CURATED.TAB_LISTING_ANALYTICS] LIMIT 1")

#Select all data for January 2020 Asheville NC
dbGetQuery(iadb,
           "SELECT * 
           FROM [CURATED.TAB_LISTING_ANALYTICS] AS A
           INNER JOIN (SELECT LOC_ID FROM [META.D_LOCATION] WHERE LOC = 'Asheville') AS B
           ON A.CITY_ID = B.LOC_ID
           WHERE CAST(DATE AS INT) >= 20200101 AND CAST(DATE AS INT) < 20200201 
           ")

#Find Average Price by Year
dbGetQuery(iadb,
           "SELECT AVG(price), SUBSTRING(DATE, 1, 4) AS year
           FROM [CURATED.TAB_LISTING_ANALYTICS]
           GROUP BY SUBSTRING(DATE, 1, 4)
           ORDER BY year")

#Find Average Pricy by Year for Asheville NC
dbGetQuery(iadb,
           "SELECT AVG(price), SUBSTRING(DATE, 1, 4) AS year
           FROM [CURATED.TAB_LISTING_ANALYTICS]
           WHERE CITY_ID = '10'
           GROUP BY SUBSTRING(DATE, 1, 4)
           ORDER BY year")

