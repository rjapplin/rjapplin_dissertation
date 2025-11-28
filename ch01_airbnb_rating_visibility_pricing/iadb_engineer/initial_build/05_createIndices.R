#STAGE.KEY_MASTER---------------------------------------------------------------

#Following asssures no duplicates to enable creation of unique index on
#KEY_LIST_ID, KEY_FILE, KEY_HOST_ID

#Add in a row counter column
dbExecute(iadb,
          "ALTER TABLE [STAGE.KEY_MASTER] ADD COLUMN RN INTEGER")

#Populate the row number column with the row number by KEY_LIST_ID and KEY_FILE
#If there are duplicate KEY_LIST_ID, KEY_FILE pairs, then there will exist rows
#where RN > 1
dbExecute(iadb,
          "WITH NUMROW AS (
            SELECT ROWID, ROW_NUMBER() OVER (PARTITION BY KEY_LIST_ID, KEY_HOST_ID, KEY_FILE ORDER BY KEY_LIST_ID) AS RN
            FROM [STAGE.KEY_MASTER]
           )
           UPDATE [STAGE.KEY_MASTER]
            SET RN = (SELECT RN FROM NUMROW WHERE NUMROW.ROWID = [STAGE.KEY_MASTER].ROWID)")

#Delete duplicate rows
dbExecute(iadb,
          "DELETE FROM [STAGE.KEY_MASTER] WHERE RN > 1")

#Assure no more duplicates exist
dbGetQuery(iadb,
           "SELECT DISTINCT 
  ROW_NUMBER() OVER (PARTITION BY KEY_LIST_ID, KEY_FILE ORDER BY KEY_LIST_ID) AS RN 
  FROM [STAGE.KEY_MASTER]"
)

#Remove Row Counter column
dbExecute(iadb,
          "ALTER TABLE [STAGE.KEY_MASTER] DROP RN")


#Create UNIQUE INDEX on KEY_LIST_ID, KEY_FILE
dbExecute(iadb,
          "CREATE UNIQUE INDEX KM_LIST_FILE_INDEX ON [STAGE.KEY_MASTER] (KEY_LIST_ID, KEY_FILE)")

#Create index on KEY_LIST_ID and KEY_FILE
dbExecute(iadb,
          "CREATE INDEX KM_LIST_INDEX ON [STAGE.KEY_MASTER] (KEY_LIST_ID)")

dbExecute(iadb,
          "CREATE INDEX KM_FILE_INDEX ON [STAGE.KEY_MASTER] (KEY_FILE)")

#STAGE.F_LIST-------------------------------------------------------------------

#Following assures no duplicates to enable creation of unique index on
#KEY_LIST_ID, KEY_FILE

#Add in a row counter column
dbExecute(iadb,
          "ALTER TABLE [STAGE.F_LIST] ADD COLUMN RN INTEGER")

#Populate the row number column with the row number by KEY_LIST_ID and KEY_FILE
#If there are duplicate KEY_LIST_ID, KEY_FILE pairs, then there will exist rows
#where RN > 1
dbExecute(iadb,
          "WITH NUMROW AS (
            SELECT ROWID, ROW_NUMBER() OVER (PARTITION BY KEY_LIST_ID, KEY_FILE ORDER BY KEY_LIST_ID) AS RN
            FROM [STAGE.F_LIST]
           )
           UPDATE [STAGE.F_LIST]
            SET RN = (SELECT RN FROM NUMROW WHERE NUMROW.ROWID = [STAGE.F_LIST].ROWID)")

#Delete duplicate rows
dbExecute(iadb,
          "DELETE FROM [STAGE.F_LIST] WHERE RN > 1")

#Assure no more duplicates exist
dbGetQuery(iadb,
  "SELECT DISTINCT 
  ROW_NUMBER() OVER (PARTITION BY KEY_LIST_ID, KEY_FILE ORDER BY KEY_LIST_ID) AS RN 
  FROM [STAGE.F_LIST]"
)

#Remove Row Counter column
dbExecute(iadb,
          "ALTER TABLE [STAGE.F_LIST] DROP RN")

#Create UNIQUE INDEX on KEY_LIST_ID, KEY_FILE
dbExecute(iadb,
          "CREATE UNIQUE INDEX SFL_LIST_FILE_INDEX ON [STAGE.F_LIST] (KEY_LIST_ID, KEY_FILE)")

#Create index on KEY_LIST_ID and KEY_FILE
dbExecute(iadb,
          "CREATE INDEX SFL_LIST_INDEX ON [STAGE.F_LIST] (KEY_LIST_ID)")

dbExecute(iadb,
          "CREATE INDEX SFL_FILE_INDEX ON [STAGE.F_LIST] (KEY_FILE)")

#STAGE.F_HOST-------------------------------------------------------------------

#Following assures no duplicates to enable creation of unique index on
#KEY_LIST_ID, KEY_FILE

#Add in a row counter column
dbExecute(iadb,
          "ALTER TABLE [STAGE.F_HOST] ADD COLUMN RN INTEGER")

#Populate the row number column with the row number by KEY_LIST_ID and KEY_FILE
#If there are duplicate KEY_LIST_ID, KEY_FILE pairs, then there will exist rows
#where RN > 1
dbExecute(iadb,
          "WITH NUMROW AS (
            SELECT ROWID, ROW_NUMBER() OVER (PARTITION BY KEY_HOST_ID, KEY_FILE ORDER BY KEY_HOST_ID) AS RN
            FROM [STAGE.F_HOST]
           )
           UPDATE [STAGE.F_HOST]
            SET RN = (SELECT RN FROM NUMROW WHERE NUMROW.ROWID = [STAGE.F_HOST].ROWID)")

#Delete duplicate rows
dbExecute(iadb,
          "DELETE FROM [STAGE.F_HOST] WHERE RN > 1")

#Assure no more duplicates exist
dbGetQuery(iadb,
           "SELECT DISTINCT 
  ROW_NUMBER() OVER (PARTITION BY KEY_LIST_ID, KEY_FILE ORDER BY KEY_LIST_ID) AS RN 
  FROM [STAGE.F_HOST]"
)

#Remove Row Counter column
dbExecute(iadb,
          "ALTER TABLE [STAGE.F_HOST] DROP RN")

#Create UNIQUE INDEX on KEY_LIST_ID, KEY_FILE
dbExecute(iadb,
          "CREATE UNIQUE INDEX SFH_LIST_FILE_INDEX ON [STAGE.F_HOST] (KEY_HOST_ID, KEY_FILE)")

#Create index on KEY_LIST_ID and KEY_FILE
dbExecute(iadb,
          "CREATE INDEX SFH_LIST_INDEX ON [STAGE.F_HOST] (KEY_HOST_ID)")

dbExecute(iadb,
          "CREATE INDEX SFH_FILE_INDEX ON [STAGE.F_HOST] (KEY_FILE)")

#STAGE.T_LIST-------------------------------------------------------------------

#Following assures no duplicates to enable creation of unique index on
#KE
#STY_LIST_ID, KEY_FILE

#Add in a row counter column
dbExecute(iadb,
          "ALTER TABLE [STAGE.T_HOST] ADD COLUMN RN INTEGER")

#Populate the row number column with the row number by KEY_LIST_ID and KEY_FILE
#If there are duplicate KEY_LIST_ID, KEY_FILE pairs, then there will exist rows
#where RN > 1
dbExecute(iadb,
          "WITH NUMROW AS (
            SELECT ROWID, ROW_NUMBER() OVER (PARTITION BY KEY_HOST_ID, KEY_FILE ORDER BY KEY_HOST_ID) AS RN
            FROM [STAGE.T_HOST]
           )
           UPDATE [STAGE.T_HOST]
            SET RN = (SELECT RN FROM NUMROW WHERE NUMROW.ROWID = [STAGE.T_HOST].ROWID)")

#Delete duplicate rows
dbExecute(iadb,
          "DELETE FROM [STAGE.T_HOST] WHERE RN > 1")

#Assure no more duplicates exist
dbGetQuery(iadb,
           "SELECT DISTINCT 
  ROW_NUMBER() OVER (PARTITION BY KEY_HOST_ID, KEY_FILE ORDER BY KEY_HOST_ID) AS RN 
  FROM [STAGE.T_HOST]"
)

#Remove Row Counter column
dbExecute(iadb,
          "ALTER TABLE [STAGE.T_HOST] DROP RN")

#Create UNIQUE INDEX on KEY_LIST_ID, KEY_FILE
dbExecute(iadb,
          "CREATE UNIQUE INDEX STH_HOST_FILE_INDEX ON [STAGE.T_HOST] (KEY_HOST_ID, KEY_FILE)")

#Create index on KEY_LIST_ID and KEY_FILE
dbExecute(iadb,
          "CREATE INDEX STH_HOST_INDEX ON [STAGE.T_HOST] (KEY_HOST_ID)")

dbExecute(iadb,
          "CREATE INDEX STH_FILE_INDEX ON [STAGE.T_HOST] (KEY_FILE)")

#RAW.F_LIST_AMENITY_INDICATORS--------------------------------------------------

#Following assures no duplicates to enable creation of unique index on
#KEY_LIST_ID, KEY_FILE

#Add in a row counter column
dbExecute(iadb,
          "ALTER TABLE [RAW.F_LIST_AMENITY_INDICATORS] ADD COLUMN RN INTEGER")

#Populate the row number column with the row number by KEY_LIST_ID and KEY_FILE
#If there are duplicate KEY_LIST_ID, KEY_FILE pairs, then there will exist rows
#where RN > 1
dbExecute(iadb,
          "WITH NUMROW AS (
            SELECT ROWID, ROW_NUMBER() OVER (PARTITION BY KEY_LIST_ID, KEY_FILE ORDER BY KEY_LIST_ID) AS RN
            FROM [RAW.F_LIST_AMENITY_INDICATORS]
           )
           UPDATE [RAW.F_LIST_AMENITY_INDICATORS]
            SET RN = (SELECT RN FROM NUMROW WHERE NUMROW.ROWID = [RAW.F_LIST_AMENITY_INDICATORS].ROWID)")

d#Delete duplicate rows
dbExecute(iadb,
          "DELETE FROM [RAW.F_LIST_AMENITY_INDICATORS] WHERE RN > 1")

#Assure no more duplicates exist
dbGetQuery(iadb,
           "SELECT DISTINCT 
  ROW_NUMBER() OVER (PARTITION BY KEY_LIST_ID, KEY_FILE ORDER BY KEY_LIST_ID) AS RN 
  FROM [RAW.F_LIST_AMENITY_INDICATORS]"
)

#Remove Row Counter column
dbExecute(iadb,
          "ALTER TABLE [RAW.F_LIST_AMENITY_INDICATORS] DROP RN")

#Create UNIQUE INDEX on KEY_LIST_ID, KEY_FILE
dbExecute(iadb,
          "CREATE UNIQUE INDEX SFLA_LIST_FILE_INDEX ON [RAW.F_LIST_AMENITY_INDICATORS] (KEY_LIST_ID, KEY_FILE)")

#Create index on KEY_LIST_ID and KEY_FILE
dbExecute(iadb,
          "CREATE INDEX SFLA_LIST_INDEX ON [RAW.F_LIST_AMENITY_INDICATORS] (KEY_LIST_ID)")

dbExecute(iadb,
          "CREATE INDEX SFLA_FILE_INDEX ON [RAW.F_LIST_AMENITY_INDICATORS] (KEY_FILE)")

