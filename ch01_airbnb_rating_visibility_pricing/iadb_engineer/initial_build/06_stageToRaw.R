#Make Current Tables the Raw Tables---------------------------------------------
dbExecute(iadb, "ALTER TABLE [STAGE.F_HOST] RENAME TO [RAW.F_HOST]")
dbExecute(iadb, "ALTER TABLE [STAGE.F_LIST] RENAME TO [RAW.F_LIST]")
dbExecute(iadb, "ALTER TABLE [STAGE.KEY_MASTER] RENAME TO [RAW.KEY_MASTER]")
dbExecute(iadb, "ALTER TABLE [STAGE.M_HOST] RENAME TO [RAW.M_HOST]")
dbExecute(iadb, "ALTER TABLE [STAGE.M_LIST] RENAME TO [RAW.M_LIST]")
dbExecute(iadb, "ALTER TABLE [STAGE.T_HOST] RENAME TO [RAW.T_HOST]")
dbExecute(iadb, "ALTER TABLE [STAGE.T_LIST] RENAME TO [RAW.T_LIST]")

#Form Equivalent Staging Tables-------------------------------------------------
dbExecute(iadb,
          "CREATE TABLE [STAGE.F_HOST] AS 
            SELECT * FROM [RAW.F_HOST] WHERE 0")

dbExecute(iadb,
          "CREATE TABLE [STAGE.F_LIST] AS 
            SELECT * FROM [RAW.F_LIST] WHERE 0")

dbExecute(iadb,
          "CREATE TABLE [STAGE.KEY_MASTER] AS 
            SELECT * FROM [RAW.KEY_MASTER] WHERE 0")

dbExecute(iadb,
          "CREATE TABLE [STAGE.M_HOST] AS 
            SELECT * FROM [RAW.M_HOST] WHERE 0")

dbExecute(iadb,
          "CREATE TABLE [STAGE.M_LIST] AS 
            SELECT * FROM [RAW.M_LIST] WHERE 0")

dbExecute(iadb,
          "CREATE TABLE [STAGE.T_HOST] AS 
            SELECT * FROM [RAW.T_HOST] WHERE 0")

dbExecute(iadb,
          "CREATE TABLE [STAGE.T_LIST] AS 
            SELECT * FROM [RAW.T_LIST] WHERE 0")
