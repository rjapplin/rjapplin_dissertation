all_files <- dbReadTable(iadb, "META.V_FILE_INFO") %>%
  filter(TYPE == "listings" & EXT == "csv") %>%
  mutate(full_path = paste0("raw/", LOC, "/", FILE_NAME)
  )
all_locs <- unique(all_files$LOC)
raw_to_clean <- dbReadTable(iadb, "META.M_FIELD_DICTIONARY") %>% 
  select(RAW, CLEAN)
startFresh <- FALSE
if(TRUE %in% (c("STAGE.F_HOST", "STAGE.T_HOST", "STAGE.M_HOST", "STAGE.F_LIST",
                "STAGE.T_LIST", "STAGE.M_LIST", 
                "STAGE.KEY_MASTER") %in% dbListTables(iadb))
){
  if(startFresh == TRUE){
    dbRemoveTable(iadb, "STAGE.KEY_MASTER")
    dbRemoveTable(iadb, "STAGE.M_LIST")
    dbRemoveTable(iadb, "STAGE.T_LIST")
    dbRemoveTable(iadb, "STAGE.F_LIST")
    dbRemoveTable(iadb, "STAGE.M_HOST")
    dbRemoveTable(iadb, "STAGE.T_HOST")
    dbRemoveTable(iadb, "STAGE.F_HOST")
  } else {
    warning("Continuing to append.")
  }
}

for(loc in 1:length(all_locs)){
  
  #Filter to location-----------------------------------------------------------
  loc_data <- all_files %>% filter(LOC == all_locs[loc]) 
  
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

