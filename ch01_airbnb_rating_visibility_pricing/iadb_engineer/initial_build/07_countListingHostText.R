#Initialize Table [RAW.T_LIST_COUNTS]-------------------------------------------
key_cols <- c("KEY_LIST_ID TEXT", "KEY_FILE TEXT")
numeric_cols <- c("T_LIST_DESCRIP_WORDS NUMERIC", "T_LIST_LOCOVER_WORDS NUMERIC",
                  "T_LIST_DESCRIP_CHAR NUMERIC", "T_LIST_LOCOVER_CHAR NUMERIC")
columns_string <- paste(c(key_cols, numeric_cols), collapse = ", ")
dbExecute(iadb, paste0("CREATE TABLE IF NOT EXISTS [RAW.T_LIST_COUNTS] (",
                       columns_string, ");"))
rm(key_cols, numeric_cols, columns_string)


files <- (dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM [RAW.KEY_MASTER]"))$KEY_FILE
for(f in 1:length(files)){
  
  dbGetQuery(iadb,
             paste0("SELECT
                      KEY_LIST_ID, 
                      KEY_FILE,
                      T_LIST_DESCRIPTION,
                      T_LIST_LOC_OVERVIEW
                    FROM [RAW.T_LIST] WHERE KEY_FILE = ", files[f])
  ) %>%
    group_by(KEY_LIST_ID) %>%
    mutate(
      DESCRIPTION = gsub("[[:punct:][:digit:]]+", "", T_LIST_DESCRIPTION) %>%
        tolower() %>%
        gsub("\\s+", " ", .),
      LOC_OVERVIEW = gsub("[[:punct:][:digit:]]+", "", T_LIST_LOC_OVERVIEW) %>%
        tolower() %>%
        gsub("\\s+", " ", .),
    ) %>%
    select(KEY_LIST_ID, KEY_FILE, DESCRIPTION, LOC_OVERVIEW) %>%
    mutate(T_LIST_DESCRIP_WORDS = length(strsplit(DESCRIPTION, "\\s")[[1]]),
           T_LIST_LOCOVER_WORDS = length(strsplit(LOC_OVERVIEW, "\\s")[[1]]),
    ) %>%
    mutate(T_LIST_DESCRIP_CHAR = nchar(DESCRIPTION),
           T_LIST_LOCOVER_CHAR = nchar(LOC_OVERVIEW),
    ) %>%
    select(KEY_LIST_ID, KEY_FILE,
           T_LIST_DESCRIP_WORDS, T_LIST_LOCOVER_WORDS, T_LIST_DESCRIP_CHAR,
           T_LIST_LOCOVER_CHAR) %>%
    dbWriteTable(iadb, "RAW.T_LIST_COUNTS", ., append = TRUE)
  
  print(f)
  
}

rm(files, f)
#Form Indicies

#Create UNIQUE INDEX on KEY_LIST_ID, KEY_FILE
dbExecute(iadb,
          "CREATE UNIQUE INDEX SFLC_LIST_FILE_INDEX ON [RAW.T_LIST_COUNTS] (KEY_LIST_ID, KEY_FILE)")

#Create index on KEY_LIST_ID and KEY_FILE
dbExecute(iadb,
          "CREATE INDEX SFLC_LIST_INDEX ON [RAW.T_LIST_COUNTS] (KEY_LIST_ID)")

dbExecute(iadb,
          "CREATE INDEX SFLC_FILE_INDEX ON [RAW.T_LIST_COUNTS] (KEY_FILE)")


#Initialize Table [RAW.T_HOST_COUNTS]-------------------------------------------
key_cols <- c("KEY_HOST_ID TEXT", "KEY_FILE TEXT")
numeric_cols <- c("T_HOST_ABOUT_WORDS NUMERIC", "T_HOST_ABOUT_CHAR NUMERIC")
columns_string <- paste(c(key_cols, numeric_cols), collapse = ", ")
dbExecute(iadb, paste0("CREATE TABLE IF NOT EXISTS [RAW.T_HOST_COUNTS] (",
                       columns_string, ");"))
rm(key_cols, numeric_cols, columns_string)


files <- (dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM [RAW.KEY_MASTER]"))$KEY_FILE


for(f in 1:length(files)){
  
  dbGetQuery(iadb,
             paste0("SELECT
                      KEY_HOST_ID, 
                      KEY_FILE,
                      T_HOST_ABOUT
                    FROM [RAW.T_HOST] WHERE KEY_FILE = ", files[f])
  ) %>%
    group_by(KEY_HOST_ID) %>%
    mutate(
      ABOUT = gsub("[[:punct:][:digit:]]+", "", T_HOST_ABOUT) %>%
        tolower() %>%
        gsub("\\s+", " ", .)
    ) %>%
    select(KEY_HOST_ID, KEY_FILE, ABOUT) %>%
    mutate(T_HOST_ABOUT_WORDS = length(strsplit(ABOUT, "\\s")[[1]]),
    ) %>%
    mutate(T_HOST_ABOUT_CHAR = nchar(ABOUT),
    ) %>%
    select(KEY_HOST_ID, KEY_FILE,
           T_HOST_ABOUT_WORDS, T_HOST_ABOUT_CHAR) %>%
    dbWriteTable(iadb, "RAW.T_HOST_COUNTS", ., append = TRUE)
  
  print(f)
  
}

#Form Indicies

#Create UNIQUE INDEX on KEY_LIST_ID, KEY_FILE
dbExecute(iadb,
          "CREATE UNIQUE INDEX SFHC_HOST_FILE_INDEX ON [RAW.T_HOST_COUNTS] (KEY_HOST_ID, KEY_FILE)")

#Create index on KEY_LIST_ID and KEY_FILE
dbExecute(iadb,
          "CREATE INDEX SFHC_HOST_INDEX ON [RAW.T_HOST_COUNTS] (KEY_HOST_ID)")

dbExecute(iadb,
          "CREATE INDEX SFHC_FILE_INDEX ON [RAW.T_HOST_COUNTS] (KEY_FILE)")





  