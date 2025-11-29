#Utility Functions--------------------------------------------------------------

#For getting row count of a file------------------------------------------------
get_row_count <- function(file_path) {
  if (file.exists(file_path)) {
    row_count <- length(count.fields(file_path, skip = 1))
    return(row_count)
  } else {
    warning(paste("File", file_path, "not found."))
    return(NA)
  }
}

#For determining number of unique listings in a file----------------------------
get_unique_ids <- function(file_path){
  
  if(file.exists(file_path)) {
    d <- read.csv(file_path)
    if(!is.null(d$id)){
      return(length(unique(d$id)))
    } else {
      return(length(unique(d$listing_id)))
    }
    
  } else {
    warning(paste("File", file_path, "not found."))
    return(NA)
  }
  
}

#For getting hyperlinks to download from IABNB website--------------------------
getDownLoadLinks <- function(){
  
  #List to pass to rsDriver to access web in headless mode
  eCaps <- list(
    "moz:firefoxOptions" = list(
      args = list('--headless')
    )
  )
  
  #Start server and browser
  rD <- rsDriver(browser="firefox", port = netstat::free_port(), verbose=F, 
                 chromever = NULL,
                 extraCapabilities = eCaps,
                 geckover = "latest",
                 phantomver = NULL)
  remDr <- rD$client
  
  #Navigate to Inside Airbnb Data Page
  remDr$navigate("https://insideairbnb.com/get-the-data/")
  
  #Find all instances where there is "expand table option"
  show_buttons <- remDr$findElements(using = "css selector", 
                                     ".showArchivedData")
  
  #Click on each expand table ("show Archived Data") button. This expands
  #a table of links to files that we will grab. For some reason,
  #need to click twice - and do a pause of about 1 second between the two
  #clicks for this to work
  for(i in seq_along(show_buttons)){
    show_buttons[[i]]$clickElement()
    Sys.sleep(1) 
    show_buttons[[i]]$clickElement()
    print(i)
  }
  
  #Now get page source and read the html
  page_source <- remDr$getPageSource()[[1]]
  page <- read_html(page_source)
  
  #Grab all hyperlinks in all tables on the page
  links_html <- page %>% html_elements("table a")
  
  #Keep only those that are for the U.S. - and only the .csv.gz file types
  #which correspond to listings, calendar, and reviews data
  hrefs <- as.character(links_html) %>% 
    grep("united-states", . ,value = TRUE) %>% 
    grep(".csv.gz", ., value = TRUE)
  
  #Extract out hyperlinks
  pattern <- 'href=\\"(https://[^\\"]+)\\"'
  hrefs <- regmatches(hrefs, regexpr(pattern, hrefs))
  hrefs <- sub('href=\\"', '', hrefs)
  hrefs <- sub('\\"$', '', hrefs)
  
  #Close out web process
  remDr$close()
  rD$server$stop()
  
  return(hrefs)
  
}

#For Extracting Metadata from Hyperlinks----------------------------------------
extractLinkMetadata <- function(links){
  
  #Take links returned from getDownLoadLinks() and extract sand clean metadata
  #contained in link
  m <- data.frame(
    LINK = links,
    LOC = sub(".*united-states/[^/]+/([^/]+)/.*", "\\1", links),
    DATE = sub(".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", links),
    TYPE = sub("\\.csv$", "", sub(".*/([^/]+)\\.[^/]+$", "\\1", links))
  ) %>%
    mutate(LOC = gsub("-", " ", LOC)) %>%
    mutate(
      LOC = case_when(
        LOC == "clark county nv" ~ "clark county, nv",
        LOC == "salem or" ~ "salem, or",
        LOC == "washington dc" ~ "washington, d.c.",
        .default = LOC
      )
    ) %>%
    dplyr::rename(loc = LOC) %>%
    left_join(dbReadTable(iadb, "META.D_LOCATION") %>%
                mutate(loc = tolower(LOC)),
              by = "loc")
  
  return(m)
  
}

#Determine which will be new files----------------------------------------------
determineIfNew <- function(metadata){
  
  #Take metadata returned from extractLinkMetadata() and verify that the
  #link references a file not in the raw data folder yet. Accomplish this
  #by merging in dimensions (LOC_ID, EXT_ID, and TYPE_ID) and creating Date
  #IDs to construct corresponding file key. Create indicator for if key is 
  #in the metadata schema.
  m <- metadata %>%
    inner_join(dbReadTable(iadb, "META.D_TYPE")) %>%
    mutate(EXT = "csv") %>%
    inner_join(dbReadTable(iadb, "META.D_EXT")) %>%
    mutate(
      YEAR_ID = year(DATE),
      MONTH_ID = month(DATE),
      DAY_ID = day(DATE)
    ) %>%
    mutate(MONTH_ID = ifelse(MONTH_ID < 10, paste0("0", MONTH_ID), MONTH_ID),
           DAY_ID = ifelse(DAY_ID < 10, paste0("0", DAY_ID), DAY_ID)) %>%
    select(LINK, loc, LOC, TYPE, LOC_ID, TYPE_ID, EXT_ID, YEAR_ID, MONTH_ID, DAY_ID) %>%
    mutate(key = paste0(LOC_ID, TYPE_ID, EXT_ID, YEAR_ID, 
                        MONTH_ID, DAY_ID)) %>%
    mutate(fullname = paste0("raw/", LOC, "/", LOC, "_", YEAR_ID, "-", MONTH_ID,
                             "-", DAY_ID, "_", TYPE, ".csv")) %>%
    mutate(FILE_EXISTS = file.exists(fullname)) %>%
    select(-fullname) %>%
    mutate(
      METADATA_EXISTS = ifelse(key %in% dbReadTable(iadb, "META.D_FILE_NAME")$FILE_ID, 
                               TRUE, FALSE),
      IS_NEW_CITY = grepl("NA", key)
    )
  
  return(m)
  
}

#Download new files-------------------------------------------------------------
downloadIAData <- function(metadata){
  
  #Keep only links that will result in a new file being downloaded using
  #return from determineIfNew()
  m <- metadata %>% 
    mutate(DOWNLOAD_FILE = (FILE_EXISTS == FALSE)) %>%
    filter(DOWNLOAD_FILE == TRUE)
  
  if(dim(m)[1] == 0){
    warning("No new files to download.")
    m <- metadata %>% filter(METADATA_EXISTS == FALSE)
    if(dim(m)[1] == 0){
      return(NULL)
    } else {
      return(m)
    }
  }
  
  for(f in 1:dim(m)[1]){
    
    #Extract out information needed to create file path
    link <- m$LINK[f]
    if(m$IS_NEW_CITY[f] == FALSE){
      city = m$LOC[f]
    } else {
      city = toTitleCase(metadata$loc[f])
    }
    date = paste0(m$YEAR_ID[f], "-", m$MONTH_ID[f], "-", m$DAY_ID[f])
    type = m$TYPE[f]
    pathToSave = paste0(
      "raw/", city, "/", city, "_", date, "_", type, ".csv"
    )
    
    #Download the file - if necessary, for new locations not present,
    #create corresponding directory
    if(file.exists(paste0("raw/", city))){
      dl <- try(write.csv(data.table::fread(link), pathToSave))
    } else {
      dir.create(paste0("raw/", city), recursive = TRUE)
      dl <- try(write.csv(data.table::fread(link), pathToSave))
    }
    
    if(class(dl) == "try-error"){
      m$download_successful[f] = FALSE
    } else {
      m$download_successful[f] = TRUE
    }
    
    print(f)
  
  }
  
  return(m)
  
}

#Updated metadata in the meta schema in the iadb database-----------------------
updateMetaData <- function(metadata){
  
  if(is.null(metadata)){
    return("No metadata to update.")
  }
  
  m <- metadata %>% filter(download_successful == TRUE)
  
  m <- m %>%
    mutate(LOC = ifelse(is.na(LOC), toTitleCase(loc), LOC))
  
  m <- m %>%
    mutate(file_name = paste0(LOC, "_", YEAR_ID, "-", MONTH_ID, "-", DAY_ID,
                              "_", TYPE, ".csv"))
  
  #Add metadata for existing locations from new files
  existing <- m %>% filter(IS_NEW_CITY == FALSE) %>%
    select(key, LOC, YEAR_ID, MONTH_ID, DAY_ID, file_name) %>%
    filter(!(key %in% dbReadTable(iadb, "META.D_FILE_NAME")$FILE_ID)) %>%
    dplyr::rename(FILE_ID = key, FILE_NAME = file_name)
  
  dbWriteTable(iadb, 
               "META.D_FILE_NAME",
               existing %>% select(FILE_ID, FILE_NAME),
               append = TRUE)
  
  dbWriteTable(iadb,
               "META.D_DATE",
               existing %>%
                 mutate(DATE = paste0(YEAR_ID, "-", MONTH_ID, "-", DAY_ID)) %>%
                 select(YEAR_ID, MONTH_ID, DAY_ID, DATE) %>%
                 distinct() %>%
                 filter(!(DATE %in% dbReadTable(iadb, "META.D_DATE")$DATE)),
               append = TRUE
  )
  
  existing <- existing %>%
    mutate(fullname = paste0("raw/", LOC, "/", FILE_NAME))
  
  #Get file size contents
  data.frame(
    FILE_ID = existing$FILE_ID,
    SIZE_MB = file.size(paste0(existing$fullname))/1000000,
    ROWS = sapply(existing$fullname, get_row_count),
    UNIQUE_LISTINGS = sapply(existing$fullname, get_unique_ids)
  ) %>%
    filter(!(FILE_ID %in% dbReadTable(iadb, "META.F_FILE_SIZE")$FILE_ID)) %>%
    dbWriteTable(iadb,
                 "META.F_FILE_SIZE",
                 .,
                 append = TRUE)
  
  existing$metadata_updated <- TRUE
  
  #Add metadata from new locations to database
  new <- m %>% filter(IS_NEW_CITY == TRUE)
  
  dbReadTable(iadb, "META.D_LOCATION") %>%
    rbind(new %>% select(LOC_ID, LOC) %>% distinct()) %>%
    mutate(max_id = max(LOC_ID, na.rm = TRUE)) %>%
    rownames_to_column("temp_id") %>%
    mutate(temp_id = as.numeric(temp_id) + 9) %>%
    filter(temp_id > max_id) %>%
    mutate(LOC_ID = temp_id) %>%
    select(LOC_ID, LOC) %>%
    dbWriteTable(iadb, 
                 "META.D_LOCATION",
                 .,
                 append = TRUE)
  
  new <- new %>%
    select(-LOC_ID) %>%
    inner_join(dbReadTable(iadb, "META.D_LOCATION"),
               by = "LOC") %>%
    mutate(key = as.character(
      paste0(LOC_ID, TYPE_ID, EXT_ID, YEAR_ID, MONTH_ID, DAY_ID)))
  
  new <- new %>%
    dplyr::rename(FILE_ID = key,
                  FILE_NAME = file_name)
  
  dbWriteTable(iadb, 
               "META.D_FILE_NAME",
               new %>% select(FILE_ID, FILE_NAME),
               append = TRUE)
  
  dbWriteTable(iadb,
               "META.D_DATE",
               new %>%
                 mutate(DATE = paste0(YEAR_ID, "-", MONTH_ID, "-", DAY_ID)) %>%
                 select(YEAR_ID, MONTH_ID, DAY_ID, DATE) %>%
                 distinct() %>%
                 filter(!(DATE %in% dbReadTable(iadb, "META.D_DATE")$DATE)),
               append = TRUE
  )
  
  new <- new %>%
    mutate(fullname = paste0("raw/", LOC, "/", FILE_NAME))
  
  #Get file size contents
  data.frame(
    FILE_ID = new$FILE_ID,
    SIZE_MB = file.size(paste0(new$fullname))/1000000,
    ROWS = sapply(new$fullname, get_row_count),
    UNIQUE_LISTINGS = sapply(new$fullname, get_unique_ids)
  ) %>%
    filter(!(FILE_ID %in% dbReadTable(iadb, "META.F_FILE_SIZE")$FILE_ID)) %>%
    dbWriteTable(iadb,
                 "META.F_FILE_SIZE",
                 .,
                 append = TRUE)
  
  new$metadata_updated <- TRUE
  
  #For returning information about downloaded and processed files
  existing %>%
    select(FILE_NAME, FILE_ID) %>%
    mutate(processed = "Successful") %>%
    rbind(
      new %>%
        select(FILE_NAME, FILE_ID) %>%
        mutate(processed = Sys.time())
    ) %>%
    return()
  
  
  
}

#Wrapper Function for Automating Scraping---------------------------------------
scrapeIAWrapper <- function(){
  getDownLoadLinks() %>%
    extractLinkMetadata() %>%
    determineIfNew() %>%
    downloadIAData() %>%
    updateMetaData() 
}

#Not run
#scrapeIAWrapper()

#Function for forming file paths------------------------------------------------
pathFormeR <- function(file_name, location){
  return(
    paste0("raw/", location, "/", file_name)
  )
}

#FILE KEY Functions-------------------------------------------------------------

parseKeyDate <- function(KEY){
  paste0(substr(KEY, 7, 10), "-", 
         substr(KEY, 11, 12), "-", 
         substr(KEY, 13, 14)) %>% as.Date()
}

#Unit Test Functions------------------------------------------------------------
all_true <- function(X){
  unique(X)==TRUE
}


parse_amenities <- function(amenities_str) {
  # Remove the curly braces
  amenities_str <- gsub("^\\{|\\}$" , "", amenities_str)
  # Replace quotes with nothing (or handle them differently if needed)
  amenities_str <- gsub('"', '', amenities_str)
  # Split the string by commas
  amenities_list <- unlist(strsplit(amenities_str, ","))
  # Trim whitespace (if any)
  amenities_list <- trimws(amenities_list)
  # Return as a list
  return(amenities_list)
}


correct_spelling <- function(words){
  correct_words <- sapply(words, function(word){
    if(word %in% ignore || hunspell_check(word)){
      return(word)
    } else {
      suggestions <- hunspell_suggest(word)
      if(length(suggestions[[1]]) > 0){
        return(suggestions[[1]][[1]])
      } else {
        return(NA)
      }
    }
  })
}

#Lookup-------------------------------------------------------------------------
lookupField <- function(field){
  dbGetQuery(iadb,
             paste0(
               "SELECT * FROM [META.M_FIELD_DICTIONARY] WHERE CLEAN = '", 
               field,
               "'")
  )
}

#ReadTableHead------------------------------------------------------------------
dbReadHead <- function(con, table, n){
  
  dbGetQuery(con, 
             paste0("SELECT * FROM [", table, "] LIMIT ", n)
  )
  
}



#Update CURATED.LISTING_ANALYTICS-----------------------------------------------
updateCuratedListAnalytics <- function(con){
  
  if("CURATED.TAB_LISTING_ANALYTICS" %in% dbListTables(con)){
    dbRemoveTable(con, "CURATED.TAB_LISTING_ANALYTICS")
    dbExecute(con, "SELECT *, CURRENT_TIMESTAMP AS TAB_LAST_UPDATED
                      INTO [CURATED.TAB_LISTING_ANALYTICS]
                    FROM [CURATED.V_LISTING_ANALYTICS")
  } else {
    
    dbExecute(con, "CREATE TABLE [CURATED.TAB_LISTING_ANALYTICS] AS
                      SELECT *, CURRENT_TIMESTAMP AS TAB_LAST_UPDATED
                      FROM [CURATED.V_LISTING_ANALYTICS]")
    
  }
  
}

