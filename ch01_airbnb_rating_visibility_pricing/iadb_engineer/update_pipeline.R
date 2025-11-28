#Identify New Data to Stage-----------------------------------------------------

#Files already in RAW
file_keys_in_raw <- dbGetQuery(iadb, 
                               "SELECT DISTINCT KEY_FILE FROM [RAW.KEY_MASTER]"
                               )$KEY_FILE

#All files in Metadata
file_keys_in_meta <- (dbReadTable(iadb, "META.V_FILE_INFO") %>%
  filter(TYPE == "listings" & EXT == "csv") %>%
  mutate(full_path = paste0("raw/", LOC, "/", FILE_NAME)
  ) %>%
  select(FILE_ID))$FILE_ID

#New Files - Set difference
new_files <- file_keys_in_meta[!(file_keys_in_meta %in% file_keys_in_raw)]
rm(file_keys_in_meta, file_keys_in_meta)

files_to_stage <- dbReadTable(iadb, "META.V_FILE_INFO") %>%
  filter(TYPE == "listings" & EXT == "csv") %>%
  mutate(full_path = paste0("raw/", LOC, "/", FILE_NAME)
  ) %>%
  filter(FILE_ID %in% new_files)

all_locs <- unique(files_to_stage$LOC)
raw_to_clean <- dbReadTable(iadb, "META.M_FIELD_DICTIONARY") %>% 
  select(RAW, CLEAN)

#Add to Staging Tables----------------------------------------------------------
for(loc in 1:length(all_locs)){
  
  #Filter to location-----------------------------------------------------------
  loc_data <- files_to_stage %>% filter(LOC == all_locs[loc]) %>%
    filter(ROWS != 0 | is.na(ROWS))
  
  #Test all files exist---------------------------------------------------------
  if(
    all_true(file.exists(loc_data$full_path))
  ){
    
  } else {
    file_check <- data.frame(
      file = loc_files, exists = file.exists(loc_files)
    ) %>% filter(exists == FALSE)
    print(file_check)
    stop("The files listed above do not exist.")
  }
  
  #Standardize column names to prepare for making database tables---------------
  for(file in 1:length(loc_data$full_path)){
    
    
    file_data <- read.csv(loc_data$full_path[file], colClasses = "character") %>%
      mutate(FILE = loc_data$FILE_ID[file]) %>%
      select(-X)
    
    file_data <- file_data %>%
      rename_with(~ raw_to_clean$CLEAN[match(colnames(file_data), 
                                             raw_to_clean$RAW)], everything())
    missing_fields <- raw_to_clean$CLEAN[which(
      raw_to_clean$CLEAN %in% colnames(file_data) == FALSE)]
    missing_fields_df <- data.frame(matrix(NA, nrow = dim(file_data)[1], 
                                           ncol = length(missing_fields)))
    colnames(missing_fields_df) <- missing_fields
    file_data <- cbind(file_data, missing_fields_df)
    rm(missing_fields, missing_fields_df)  
    
    #LIST_HOST_FILE_MAP---------------------------------------------------------
    
    #This table contains a list of all Keys
    list_host_map <- file_data %>% select(KEY_LIST_ID, KEY_HOST_ID, KEY_FILE,
                                          M_SCRAPE_ID, M_SCRAPE_LAST,
                                          M_CAL_LAST_SCRAPE)
    dbWriteTable(conn = iadb,
                 name = "STAGE.KEY_MASTER",
                 value = list_host_map,
                 append = TRUE)
    rm(list_host_map)
    
    #LISTING_FACT---------------------------------------------------------------
    
    #This table contains all fact fields for listings
    list_fact <- file_data %>%
      select(KEY_LIST_ID, KEY_FILE, starts_with("F_LIST"))
    dbWriteTable(conn = iadb,
                 name = "STAGE.F_LIST",
                 value = list_fact,
                 append = TRUE)
    rm(list_fact)
    
    #LISTING_TEXT---------------------------------------------------------------
    
    #This table contains all text heavy fields for listings
    list_text <- file_data %>%
      select(KEY_LIST_ID, KEY_FILE, starts_with("T_LIST"))
    dbWriteTable(conn = iadb,
                 name = "STAGE.T_LIST",
                 value = list_text,
                 append = TRUE)
    rm(list_text)
    
    #LISTING_META---------------------------------------------------------------
    
    #This table contains all metadata fields for listings
    list_meta <- file_data %>%
      select(KEY_LIST_ID, KEY_FILE, starts_with("M_LIST"))
    dbWriteTable(conn = iadb,
                 name = "STAGE.M_LIST",
                 value = list_meta,
                 append = TRUE)
    rm(list_meta)
    
    #HOST FACT------------------------------------------------------------------
    
    #This table contains all fact fields for hosts
    host_fact <- file_data %>%
      select(KEY_HOST_ID, KEY_FILE, starts_with("F_HOST")) %>%
      distinct() %>%
      group_by(KEY_HOST_ID, KEY_FILE) %>%
      mutate(ROWN = row_number()) %>%
      filter(ROWN == 1) %>%
      select(-ROWN)
    dbWriteTable(conn = iadb,
                 name = "STAGE.F_HOST",
                 value = host_fact,
                 append = TRUE)
    rm(host_fact)
    
    #HOST_TEXT---------------------------------------------------------------
    
    #This table contains all text heavy fields for hosts
    host_text <- file_data %>%
      select(KEY_HOST_ID, KEY_FILE, starts_with("T_HOST")) %>%
      distinct() %>%
      group_by(KEY_HOST_ID, KEY_FILE) %>%
      mutate(ROWN = row_number()) %>%
      filter(ROWN == 1) %>%
      select(-ROWN)
    dbWriteTable(conn = iadb,
                 name = "STAGE.T_HOST",
                 value = host_text,
                 append = TRUE)
    rm(host_text)
    
    #HOST_META------------------------------------------------------------------
    
    #This table contains all metadata fields for hosts
    host_meta <- file_data %>%
      select(KEY_HOST_ID, KEY_FILE, starts_with("M_HOST")) %>%
      distinct() %>%
      group_by(KEY_HOST_ID, KEY_FILE) %>%
      mutate(ROWN = row_number()) %>%
      filter(ROWN == 1) %>%
      select(-ROWN)
    dbWriteTable(conn = iadb,
                 name = "STAGE.M_HOST",
                 value = host_meta,
                 append = TRUE)
    rm(host_meta)
    
    print(paste0("Inner Progress: ", round((file/length(loc_data$full_path))*100, 2)))
    
  }
  
  print(paste0("Outer Progress: ", round((loc/length(all_locs))*100, 2)))
  if(loc == length(all_locs)){
    system("curl -d 'All done!!!' ntfy.sh/applin_airbnb_r_code")
  }
  
}

#Intermediate Cleanup
rm(file_keys_in_raw, file_keys_in_meta, new_files, files_to_stage, all_locs,
   raw_to_clean)


#Process Amenity Data-----------------------------------------------------------

#Get Amenities data from Staging Table STAGE.T_LIST and parse amenities
amen <- dbGetQuery(iadb, "SELECT KEY_LIST_ID, KEY_FILE, T_LIST_AMENITIES FROM [STAGE.T_LIST]") %>%
  mutate(amenities_list = lapply(T_LIST_AMENITIES, parse_amenities)) %>%
  select(-T_LIST_AMENITIES)

#Identify all unique amenities
all_amen <- unique(unlist(amen$amenities_list))

#Clean up all_amen
all_amen <- all_amen %>%
  t() %>% 
  t() %>% as.data.frame() %>%
  dplyr::rename(AMENITY_RAW = V1)

#Convert main amenity table to data.table
amen <- as.data.table(amen)

#Download language model to aid cleaning up amenity text
model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(model$file_model)

#Remove all punctuation and digits, convert to lower case, remove stop words,
#and remove extra spaces
all_amen %>%
  mutate(AMENITY_CLEAN = gsub(
    "[[:punct:][:digit:]]+", "", AMENITY_RAW
  )) %>%
  mutate(AMENITY_CLEAN = tolower(AMENITY_CLEAN)) %>%
  mutate(AMENITY_CLEAN = removeWords(AMENITY_CLEAN, stopwords("en"))) %>%
  mutate(AMENITY_CLEAN = gsub("\\s+", " ", AMENITY_CLEAN)) -> all_amen2
dbWriteTable(iadb, "STAGE.D_ALL_AMENITIES", all_amen2, overwrite = T)

#Annotate the cleaned up text
anno <- udpipe_annotate(ud_model, x = all_amen2$AMENITY_CLEAN)

#Identify nouns and correct misspellings
anno <- anno %>%
  as.data.frame() %>%
  filter(upos == "NOUN") %>%
  select(sentence, lemma) %>% 
  distinct()

anno <- anno %>% dplyr::rename(AMENITY_CLEAN = sentence)

#Merge in Amenity nouns back in with vector of all amenities
amenity_final <- anno %>%
  select(lemma) %>%
  distinct() %>%
  mutate(AMENITY_CODE = paste0("A", row_number())) %>%
  inner_join(anno, by = "lemma") %>%
  inner_join(all_amen2, by = "AMENITY_CLEAN", relationship = "many-to-many") %>%
  dplyr::rename(AMENITY_LEMMA = lemma)
dbWriteTable(iadb, "STAGE.D_AMENITY_LEMMAS", amenity_final, overwrite = T)

#Read in human labeled
amenity_human_lab <-
  read.csv("iadb_engineer/int_data/AMENITY_HUMAN_LABELED.csv") %>%
  filter(HUMAN_LABEL != "remove") %>%
  arrange(HUMAN_LABEL) %>%
  mutate(AMENITY_CLEAN_CODE = paste0("AC", as.numeric(as.factor(HUMAN_LABEL))))

#Finalize raw to clean mapping
amenity_map <- amenity_final %>%
  select(-AMENITY_LEMMA) %>%
  inner_join(amenity_human_lab, by = "AMENITY_CODE") %>%
  select(AMENITY_CODE, AMENITY_CLEAN, AMENITY_RAW, HUMAN_LABEL, AMENITY_CLEAN_CODE) %>%
  mutate(wrong_pool = case_when(
    HUMAN_LABEL == "pool" & str_detect(AMENITY_CLEAN, "world pool") ~ 1,
    HUMAN_LABEL == "pool" & str_detect(AMENITY_CLEAN, "pool table") ~ 1,
    HUMAN_LABEL == "pool" & str_detect(AMENITY_CLEAN, "whirl pool") ~ 1,
    .default = 0)
  ) %>%
  filter(wrong_pool == 0)
dbWriteTable(iadb, "STAGE.D_AMENITY_MAP", amenity_map, overwrite = T)

#Cleanup
rm(amenity_human_lab, amenity_final, anno, all_amen, all_amen2, ud_model, model)

#STAGE.T_AMENITIES
amax <- max((amenity_map %>%
               mutate(numeric_code = as.numeric(gsub("AC", "", AMENITY_CLEAN_CODE))) %>%
               select(numeric_code))$numeric_code)

key_cols <- c("KEY_LIST_ID TEXT", "KEY_FILE TEXT")

#Amenity codes
numeric_cols <- paste0("AC", 1:amax, " NUMERIC")

columns_string <- paste(c(key_cols, 
                          c(" AMENITY_WORD_COUNT NUMERIC", " 
                            AMENITY_COUNT NUMERIC",
                            "AMENITY_CHAR_COUNT NUMERIC", "AC9999 NUMERIC"), 
                          numeric_cols), collapse = ", ")
dbExecute(iadb, paste0("CREATE TABLE IF NOT EXISTS [STAGE.F_LIST_AMENITY_INDICATORS] (",
                       columns_string, ");"))
all_amen_codes <- gsub(" NUMERIC", "", numeric_cols)
rm(amen)

files <- (dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM [STAGE.KEY_MASTER]"))$KEY_FILE

#Create Index on STAGE.T_LIST to speed up read
dbExecute(iadb, "CREATE INDEX FILE_INDEX ON [STAGE.T_LIST] (KEY_FILE ASC)")


for(f in 1:length(files)){
  
  temp <- dbGetQuery(iadb,
                     paste0("SELECT KEY_LIST_ID, T_LIST_AMENITIES FROM [STAGE.T_LIST] WHERE KEY_FILE = ", files[f])
  )
  nlist <- length(unique(temp$KEY_LIST_ID))
  
  temp <- temp %>%
    #Transform amenities to list
    mutate(amenities_list = lapply(T_LIST_AMENITIES, parse_amenities)) %>%
    select(-T_LIST_AMENITIES) %>%
    group_by(KEY_LIST_ID) %>%
    #Compute word count, character count, and unique amenity count
    mutate(AMENITY_WORD_COUNT = length(
      unlist(
        strsplit(
          unlist(amenities_list), 
          "\\s+")
      )
    )
    ) %>%
    dplyr::mutate(AMENITY_CHAR_COUNT = nchar(amenities_list)) %>%
    dplyr::mutate(AMENITY_COUNT = length(amenities_list[[1]])) %>%
    ungroup() %>%
    #Transform each separate amenity to its own row
    unnest(amenities_list, keep_empty = TRUE) %>%
    dplyr::rename(AMENITY_RAW = amenities_list)   %>%
    left_join(amenity_map, by = "AMENITY_RAW") %>%
    select(KEY_LIST_ID, AMENITY_WORD_COUNT, AMENITY_CHAR_COUNT,
           AMENITY_COUNT, AMENITY_CLEAN_CODE) %>%
    #Put all amenities not in the amenity map under code AC9999
    mutate(AMENITY_CLEAN_CODE = ifelse(is.na(AMENITY_CLEAN_CODE), 
                                       "AC9999",
                                       AMENITY_CLEAN_CODE)) %>%
    distinct() %>%
    #For use with pivot_wider next to make dummies
    mutate(value = 1) %>%
    #Pivot out code to create dummy variables indicating precense of amenities
    pivot_wider(names_from = "AMENITY_CLEAN_CODE", 
                values_from = value,
                values_fill = list(value = 0),
                values_fn = list(value = max)) %>%
    mutate(AC9999 = {if(!("AC9999" %in% names(.))) 0 else AC9999})
  
  #Create dataframe of zeroes for amenities that did not appear above
  missing_amen <- all_amen_codes[!(all_amen_codes %in% colnames(temp))] %>%
    t() %>% as.data.frame()
  amen_names <- missing_amen[1,]
  missing_amen[1,] <- 0
  missing_amen <- missing_amen %>%
    as.matrix() %>%
    as.numeric() %>% 
    t() %>%
    as.data.frame()
  colnames(missing_amen) <- amen_names
  
  temp <- cbind(temp, missing_amen) %>%
    mutate(KEY_FILE = files[f])
  
  if(
    length(unique(temp$KEY_LIST_ID)) != nlist
  ) {
    stop(paste0("Lost some listings somehow for file ", files[f]))
  }
  
  dbWriteTable(iadb, "STAGE.F_LIST_AMENITY_INDICATORS", temp, append = TRUE)
  print(f)
  
  
}

#Cleanup
rm(all_amen_codes, amax, columns_string, temp, missing_amen, f, files, key_cols,
   nlist, numeric_cols)

#STAGE.F_LIST to RAW.F_LIST-----------------------------------------------------
  
  #Sanity Check
  stage_file_keys <- dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM 
                                [STAGE.F_LIST]")$KEY_FILE
  raw_file_keys <- dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM
                              [RAW.F_LIST]")$KEY_FILE
  goodToProceed <- !(all_true(stage_file_keys %in% raw_file_keys))
  if(!goodToProceed){
    stop("Some stage data appears to already be in raw data")
  } else {
    
    stage_n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [STAGE.F_LIST]")$C
    raw_n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [RAW.F_LIST]")$C
    union_n <- stage_n + raw_n
    
    #Create temporary table for review
    dbExecute(iadb, "CREATE TEMPORARY TABLE [temp_raw] 
              AS SELECT * FROM [RAW.F_LIST] LIMIT 0")
    
    #Insert [RAW.F_LIST] into temp_raw
    dbExecute(iadb, "INSERT INTO temp_raw SELECT * FROM [RAW.F_LIST]")
    
    #Insert [STAGE.F_LIST] INTO temp_raw
    dbExecute(iadb, "INSERT INTO temp_raw SELECT * FROM [STAGE.F_LIST]")
    
    #Check Rows
    n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM temp_raw")$C
    goodToProceed <- (n == union_n)
    
    if(!(goodToProceed)){
      stop("Rows(STAGE.F_LIST) + Rows(RAW.F_LIST) != Rows(STAGE.F_LIST + RAW.F_LIST)")
    } else {
      dbExecute(iadb, "INSERT INTO [RAW.F_LIST] SELECT * FROM [STAGE.F_LIST]")
    }
    
    #Validate for sanity
    n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [RAW.F_LIST]")$C
    allAppearsGood <- (n == union_n)
    if(!(allAppearsGood)){
      stop("CRITICAL WARNING: EXPECTED NUMBER OF ROWS OF UPDATED [RAW.F_LIST] NOT EQUAL TO ACTUAL NUMBER OF ROWS")
    }
    
  }
  
#STAGE.F_HOST to RAW.F_HOST---------------------------------------------------
  
  #Sanity Check
  stage_file_keys <- dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM 
                                [STAGE.F_HOST]")$KEY_FILE
  raw_file_keys <- dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM
                              [RAW.F_HOST]")$KEY_FILE
  goodToProceed <- !(all_true(stage_file_keys %in% raw_file_keys))
  if(!goodToProceed){
    stop("Some stage data appears to already be in raw data")
  } else {
    
    stage_n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [STAGE.F_HOST]")$C
    raw_n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [RAW.F_HOST]")$C
    union_n <- stage_n + raw_n
    
    #Create temporary table for review
    dbExecute(iadb, "CREATE TEMPORARY TABLE [temp_raw] 
              AS SELECT * FROM [RAW.F_HOST] LIMIT 0")
    
    #Insert [RAW.F_HOST] into temp_raw
    dbExecute(iadb, "INSERT INTO temp_raw SELECT * FROM [RAW.F_HOST]")
    
    #Insert [STAGE.F_HOST] INTO temp_raw
    dbExecute(iadb, "INSERT INTO temp_raw SELECT * FROM [STAGE.F_HOST]")
    
    #Check Rows
    n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM temp_raw")$C
    goodToProceed <- (n == union_n)
    
    if(!(goodToProceed)){
      stop("Rows(STAGE.F_HOST) + Rows(RAW.F_HOST) != Rows(STAGE.F_HOST + RAW.F_HOST)")
    } else {
      dbExecute(iadb, "INSERT INTO [RAW.F_HOST] SELECT * FROM [STAGE.F_HOST]")
    }
    
    #Validate for sanity
    n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [RAW.F_HOST]")$C
    allAppearsGood <- (n == union_n)
    if(!(allAppearsGood)){
      stop("CRITICAL WARNING: EXPECTED NUMBER OF ROWS OF UPDATED [RAW.F_HOST] NOT EQUAL TO ACTUAL NUMBER OF ROWS")
    }
    
  }
  
#STAGE.M_HOST to RAW.M_HOST-----------------------------------------------------

  #Sanity Check
  stage_file_keys <- dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM 
                                [STAGE.M_HOST]")$KEY_FILE
  raw_file_keys <- dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM
                              [RAW.M_HOST]")$KEY_FILE
  goodToProceed <- !(all_true(stage_file_keys %in% raw_file_keys))
  if(!goodToProceed){
    stop("Some stage data appears to already be in raw data")
  } else {
    
    stage_n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [STAGE.M_HOST]")$C
    raw_n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [RAW.M_HOST]")$C
    union_n <- stage_n + raw_n
    
    #Create temporary table for review
    dbExecute(iadb, "DROP TABLE [temp_raw]")
    dbExecute(iadb, "CREATE TEMPORARY TABLE [temp_raw] 
              AS SELECT * FROM [RAW.M_HOST] LIMIT 0")
    
    #Insert [RAW.M_HOST] into temp_raw
    dbExecute(iadb, "INSERT INTO temp_raw SELECT * FROM [RAW.M_HOST]")
    
    #Insert [STAGE.M_HOST] INTO temp_raw
    dbExecute(iadb, "INSERT INTO temp_raw SELECT * FROM [STAGE.M_HOST]")
    
    #Check Rows
    n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM temp_raw")$C
    goodToProceed <- (n == union_n)
    
    if(!(goodToProceed)){
      stop("Rows(STAGE.M_HOST) + Rows(RAW.M_HOST) != Rows(STAGE.M_HOST + RAW.M_HOST)")
    } else {
      dbExecute(iadb, "INSERT INTO [RAW.M_HOST] SELECT * FROM [STAGE.M_HOST]")
    }
    
    #Validate for sanity
    n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [RAW.M_HOST]")$C
    allAppearsGood <- (n == union_n)
    if(!(allAppearsGood)){
      stop("CRITICAL WARNING: EXPECTED NUMBER OF ROWS OF UPDATED [RAW.M_HOST] NOT EQUAL TO ACTUAL NUMBER OF ROWS")
    }
    
  }
  
#STAGE.M_LIST to RAW.M_LIST-----------------------------------------------------

  #Sanity Check
  stage_file_keys <- dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM 
                                [STAGE.M_LIST]")$KEY_FILE
  raw_file_keys <- dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM
                              [RAW.M_LIST]")$KEY_FILE
  goodToProceed <- !(all_true(stage_file_keys %in% raw_file_keys))
  if(!goodToProceed){
    stop("Some stage data appears to already be in raw data")
  } else {
    
    stage_n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [STAGE.M_LIST]")$C
    raw_n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [RAW.M_LIST]")$C
    union_n <- stage_n + raw_n
    
    #Create temporary table for review
    dbExecute(iadb, "DROP TABLE [temp_raw]")
    dbExecute(iadb, "CREATE TEMPORARY TABLE [temp_raw] 
              AS SELECT * FROM [RAW.M_LIST] LIMIT 0")
    
    #Insert [RAW.M_LIST] into temp_raw
    dbExecute(iadb, "INSERT INTO temp_raw SELECT * FROM [RAW.M_LIST]")
    
    #Insert [STAGE.M_LIST] INTO temp_raw
    dbExecute(iadb, "INSERT INTO temp_raw SELECT * FROM [STAGE.M_LIST]")
    
    #Check Rows
    n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM temp_raw")$C
    goodToProceed <- (n == union_n)
    
    if(!(goodToProceed)){
      stop("Rows(STAGE.M_LIST) + Rows(RAW.M_LIST) != Rows(STAGE.M_LIST + RAW.M_LIST)")
    } else {
      dbExecute(iadb, "INSERT INTO [RAW.M_LIST] SELECT * FROM [STAGE.M_LIST]")
    }
    
    #Validate for sanity
    n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [RAW.M_LIST]")$C
    allAppearsGood <- (n == union_n)
    if(!(allAppearsGood)){
      stop("CRITICAL WARNING: EXPECTED NUMBER OF ROWS OF UPDATED [RAW.M_LIST] NOT EQUAL TO ACTUAL NUMBER OF ROWS")
    }
    
  }
  
#STAGE.T_LIST to RAW.T_LIST-----------------------------------------------------
  
  #Sanity Check
  stage_file_keys <- dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM 
                                [STAGE.T_LIST]")$KEY_FILE
  raw_file_keys <- dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM
                              [RAW.T_LIST]")$KEY_FILE
  goodToProceed <- !(all_true(stage_file_keys %in% raw_file_keys))
  if(!goodToProceed){
    stop("Some stage data appears to already be in raw data")
  } else {
    
    stage_n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [STAGE.T_LIST]")$C
    raw_n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [RAW.T_LIST]")$C
    union_n <- stage_n + raw_n
    
    #Create temporary table for review
    dbExecute(iadb, "DROP TABLE [temp_raw]")
    dbExecute(iadb, "CREATE TEMPORARY TABLE [temp_raw] 
              AS SELECT * FROM [RAW.T_LIST] LIMIT 0")
    
    #Insert [RAW.T_LIST] into temp_raw
    dbExecute(iadb, "INSERT INTO temp_raw SELECT * FROM [RAW.T_LIST]")
    
     #Insert [STAGE.T_LIST] INTO temp_raw
    dbBegin(iadb)
    dbExecute(iadb, "INSERT INTO temp_raw SELECT * FROM [STAGE.T_LIST]")
    dbCommit(iadb)
    
    #Check Rows
    n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM temp_raw")$C
    goodToProceed <- (n == union_n)
    
    if(!(goodToProceed)){
      stop("Rows(STAGE.M_LIST) + Rows(RAW.T_LIST) != Rows(STAGE.T_LIST + RAW.T_LIST)")
    } else {
      dbBegin(iadb)
      dbExecute(iadb, "INSERT INTO [RAW.T_LIST] SELECT * FROM [STAGE.T_LIST]")
      dbCommit(iadb)
    }
    
    #Validate for sanity
    n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [RAW.T_LIST]")$C
    allAppearsGood <- (n == union_n)
    if(!(allAppearsGood)){
      stop("CRITICAL WARNING: EXPECTED NUMBER OF ROWS OF UPDATED [RAW.T_LIST] NOT EQUAL TO ACTUAL NUMBER OF ROWS")
    }
    
  }
  
#STAGE.T_HOST to RAW.T_HOST-----------------------------------------------------
  
  #Sanity Check
  stage_file_keys <- dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM 
                                [STAGE.T_HOST]")$KEY_FILE
  raw_file_keys <- dbGetQuery(iadb, "SELECT DISTINCT KEY_FILE FROM
                              [RAW.T_HOST]")$KEY_FILE
  goodToProceed <- !(all_true(stage_file_keys %in% raw_file_keys))
  if(!goodToProceed){
    stop("Some stage data appears to already be in raw data")
  } else {
    
    stage_n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [STAGE.T_HOST]")$C
    raw_n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [RAW.T_HOST]")$C
    union_n <- stage_n + raw_n
    
    #Create temporary table for review
    dbExecute(iadb, "DROP TABLE [temp_raw]")
    dbExecute(iadb, "CREATE TEMPORARY TABLE [temp_raw] 
              AS SELECT * FROM [RAW.T_HOST] LIMIT 0")
    
    #Insert [RAW.T_HOST] into temp_raw
    dbBegin(iadb)
    dbExecute(iadb, "INSERT INTO temp_raw SELECT * FROM [RAW.T_HOST]")
    dbCommit(iadb)
    
    #Insert [STAGE.T_HOST] INTO temp_raw
    dbBegin(iadb)
    dbExecute(iadb, "INSERT INTO temp_raw SELECT * FROM [STAGE.T_HOST]")
    dbCommit(iadb)
    
    #Check Rows
    n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM temp_raw")$C
    goodToProceed <- (n == union_n)
    
    if(!(goodToProceed)){
      stop("Rows(STAGE.T_HOST) + Rows(RAW.T_HOST) != Rows(STAGE.T_HOST + RAW.T_HOST)")
    } else {
      dbBegin(iadb)
      dbExecute(iadb, "INSERT INTO [RAW.T_HOST] SELECT * FROM [STAGE.T_HOST]")
      dbCommit(iadb)
    }
    
    #Validate for sanity
    n <- dbGetQuery(iadb, "SELECT COUNT(*) AS C FROM [RAW.T_HOST]")$C
    allAppearsGood <- (n == union_n)
    if(!(allAppearsGood)){
      stop("CRITICAL WARNING: EXPECTED NUMBER OF ROWS OF UPDATED [RAW.T_HOST] NOT EQUAL TO ACTUAL NUMBER OF ROWS")
    }
    
  }
  
  