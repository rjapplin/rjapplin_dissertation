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
dbWriteTable(iadb, "RAW.D_ALL_AMENITIES", all_amen2)

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
dbWriteTable(iadb, "RAW.D_AMENITY_LEMMAS", amenity_final)

#Write to csv for human labeling
amenity_final %>%
  select(AMENITY_LEMMA, AMENITY_CODE) %>%
  distinct() %>%
  write.csv("iadb_engineer/int_data/AMENITY_HUMAN_LABELED.csv")

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
dbWriteTable(iadb, "RAW.D_AMENITY_MAP", amenity_map)

rm(amenity_human_lab, amenity_final, anno, all_amen, all_amen2, ud_model, model)

#Initialize PREPARED.T_AMENITIES
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
dbExecute(iadb, paste0("CREATE TABLE IF NOT EXISTS [RAW.F_LIST_AMENITY_INDICATORS] (",
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
    
    dbWriteTable(iadb, "RAW.F_LIST_AMENITY_INDICATORS", temp, append = TRUE)
    print(f)
  
  
}

dbExecute(iadb, "DROP INDEX FILE_INDEX")

#Clean up    
rm(temp, all_amen_codes, amax, amenity_map, columns_string, f, files, key_cols,
   nlist, numeric_cols, missing_amen, query, table_cols)
