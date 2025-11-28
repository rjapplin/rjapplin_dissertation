#Form Curated Table-------------------------------------------------------------
dbExecute(con, "CREATE TABLE [CURATED.TAB_LISTING_ANALYTICS] AS
                      SELECT *, CURRENT_TIMESTAMP AS TAB_LAST_UPDATED
                      FROM [CURATED.V_LISTING_ANALYTICS]")