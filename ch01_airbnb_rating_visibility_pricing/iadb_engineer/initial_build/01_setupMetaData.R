#Meta Data----------------------------------------------------------------------

#Number of Files Associated With Each City--------------------------------------
m_loc_num_files <- data.frame(
  loc = list.files(paste0("raw/"))
) %>%
  inner_join(
    sapply(list.files(paste0("raw/")), 
           function(loc){list.files(paste0("raw/", loc)) %>% 
               length()}) %>% 
      as.data.frame() %>%
      dplyr::rename(num_files = ".") %>%
      rownames_to_column("loc"),
    "loc")

#File Names and Information-----------------------------------------------------

#Extract file name. And then use name to extract date, file extension, and file
#size
m_loc_file_info <- m_loc_num_files$loc %>%
  lapply(
    function(loc){
      data.frame(name = list.files(paste0("raw/", loc)))
    }
  ) %>%
  Reduce(rbind, .) %>%
  mutate(loc = sub("_.*", "", name)) %>%
  mutate(date = sub(".*_(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", name)) %>%
  mutate(type =  sub("^[^_]*_(.*)\\.(csv|tsv)$", "\\1", name)) %>%
  mutate(type = sub("^[^_]*_", "", type)) %>%
  mutate(ext = sub("^[^.]*\\.", "", name)) %>%
  mutate(ext = sub("^[^.]*\\.", "", ext)) %>%
  mutate(ext = sub("^[^.]*\\.", "", ext)) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(day = day(date)) %>%
  mutate(size = file.size(paste0("raw/", loc, "/", name))/1000000)

#Get number of rows of data for listings csv files
m_loc_file_info %>%
  filter(type == "listings" & ext == "csv") %>%
  mutate(temp = paste0("raw/", loc, "/", name)) %>%
  mutate(rows = sapply(temp, get_row_count)) %>%
  select(-temp) %>%
  right_join(
    m_loc_file_info,
    by = c("name", "loc", "date", "type", "ext", "year", "month", "day", "size")
  ) -> m_loc_file_info

#Get number of rows of data for calendar csv files
m_loc_file_info %>%
  filter(type == "calendar" & ext == "csv") %>%
  mutate(temp = paste0("raw/", loc, "/", name)) %>%
  mutate(rows = sapply(temp, get_row_count)) %>%
  select(-temp) %>%
  right_join(
    m_loc_file_info,
    by = c("name", "loc", "date", "type", "ext", "year", "month", "day", "size")
  ) %>%
  mutate(rows = ifelse(type == "calendar", rows.x, rows.y)) %>%
  select(-rows.x, -rows.y) -> m_loc_file_info

#Get Number of Unique Listing IDs
m_loc_file_info <- m_loc_file_info %>%
  filter(type %in% c("calendar", "listings") & ext == "csv") %>%
  mutate(temp = paste0("raw/", loc, "/", name)) %>%
  mutate(unique_listings = sapply(temp, get_unique_ids)) %>%
  select(-temp) %>%
  right_join(m_loc_file_info,
             by = c("name", "loc", "date", "type", "ext", "year", "month",
                    "day", "size", "rows"))

#Create location ID
d_loc_id <- m_loc_num_files %>%
  select(loc) %>%
  rownames_to_column("LOC_ID") %>%
  mutate(LOC_ID = as.numeric(LOC_ID) + 9)

#Merge in LOC_ID into m_loc_num_files
m_loc_num_files <- m_loc_num_files %>%
  inner_join(d_loc_id, "loc") %>%
  select(-loc) %>%
  select(LOC_ID, num_files) 

#Merge in LOC_ID into m_loc_file_info
m_loc_file_info <- m_loc_file_info %>%
  inner_join(d_loc_id, "loc") %>%
  select(-loc) %>%
  select(LOC_ID, name, date, type, ext, year, month, day, size, rows,
         unique_listings)

#Type ID
d_type_id <- m_loc_file_info %>%
  select(type) %>%
  distinct() %>%
  rownames_to_column("TYPE_ID") %>%
  mutate(TYPE_ID = as.numeric(TYPE_ID) + 9)
  
#Merge in Type ID
m_loc_file_info <- m_loc_file_info %>%
  inner_join(d_type_id, "type") %>%
  select(-type) %>%
  select(LOC_ID, TYPE_ID, name, date, ext, year, month, day, size, rows,
         unique_listings)

#Extension ID
d_ext_id <- m_loc_file_info %>%
  select(ext) %>%
  distinct() %>%
  rownames_to_column("EXT_ID") %>%
  mutate(EXT_ID = as.numeric(EXT_ID) + 9)

#Merge in Extension ID
m_loc_file_info <- m_loc_file_info %>%
  inner_join(d_ext_id, "ext") %>%
  select(-ext) %>%
  select(LOC_ID, TYPE_ID, EXT_ID, name, YEAR_ID, MONTH_ID, DAY_ID, size, rows,
         unique_listings)

#Rename year, month, day to ID
m_loc_file_info <- m_loc_file_info %>%
  dplyr::rename(
    YEAR_ID = year,
    MONTH_ID = month,
    DAY_ID = day
  ) %>%
  select(LOC_ID, TYPE_ID, EXT_ID, YEAR_ID, MONTH_ID, DAY_ID, name, size, rows,
         unique_listings)

#Add Leading Zero to Month and Day
m_loc_file_info <- m_loc_file_info %>%
  mutate(MONTH_ID = ifelse(MONTH_ID < 10, paste0("0", MONTH_ID), MONTH_ID),
         DAY_ID = ifelse(DAY_ID < 10, paste0("0", DAY_ID), DAY_ID))

#Create Unique File Identifier
m_loc_file_info <- m_loc_file_info %>%
  mutate(
    KEY_FILE = paste0(LOC_ID, TYPE_ID, EXT_ID, YEAR_ID, 
                      MONTH_ID, DAY_ID) %>%
      as.character()
  ) %>%
  select(KEY_FILE, LOC_ID, TYPE_ID, EXT_ID, YEAR_ID, MONTH_ID, DAY_ID, name,
         size, rows, unique_listings)

m_file_key <- m_loc_file_info %>%
  select(KEY_FILE, name)

m_file_contents <- m_loc_file_info %>%
  select(KEY_FILE, size, rows, unique_listings)

m_key_ids <- m_loc_file_info %>%
  select(KEY_FILE, LOC_ID, TYPE_ID, EXT_ID, YEAR_ID, MONTH_ID, DAY_ID)

d_date_id <- m_loc_file_info %>%
  select(YEAR_ID, MONTH_ID, DAY_ID) %>%
  distinct()

#Create/Add To Database---------------------------------------------------------

if(!file.exists("ia.db")){
iadb <- dbConnect(SQLite(), dbname = "ia.db")
}

#Write m_file_key, m_file_contents, m_key_ids into database
if(!("META.D_FILE_NAME" %in% dbListTables(iadb))){
dbWriteTable(iadb,
             "META.D_FILE_NAME",
             m_file_key %>% dplyr::rename(FILE_ID = KEY_FILE, FILE_NAME = name),
             overwrite = TRUE)
}

if(!("META.V_KEY_ID_MAPPINGS" %in% dbListTables(iadb))){
dbExecute(iadb, "CREATE VIEW [META.V_KEY_ID_MAPPINGS] AS
            SELECT
            FILE_ID,
            SUBSTRING(FILE_ID, 1, 2) AS LOC_ID,
            SUBSTRING(FILE_ID, 3, 2) AS TYPE_ID,
            SUBSTRING(FILE_ID, 5, 2) AS EXT_ID, 
            SUBSTRING(FILE_ID, 7, 4) AS YEAR_ID,
            SUBSTRING(FILE_ID, 11, 2) AS MONTH_ID,
            SUBSTRING(FILE_ID, 13, 2) AS DAY_ID FROM [META.D_FILE_NAME]")
}

if(!("META.F_FILE_SIZE" %in% dbListTables(iadb))){
dbWriteTable(iadb, 
             "META.F_FILE_SIZE",
             m_file_contents %>% dplyr::rename(FILE_ID = KEY_FILE, SIZE_MB = size,
                                               ROWS = rows, 
                                               UNIQUE_LISTINGS = unique_listings),
             overwrite = TRUE)
}

#Write Dimension Tables into Database
if(!("META.D_LOCATION" %in% dbListTables(iadb))){
dbWriteTable(iadb, 
             "META.D_LOCATION",
             d_loc_id %>% dplyr::rename(LOC = loc),
             overwrite = TRUE)
}

if(!("META.D_EXT" %in% dbListTables(iadb))){
dbWriteTable(iadb,
             "META.D_EXT",
             d_ext_id %>% dplyr::rename(EXT = ext),
             overwrite = TRUE)
}

if(!("META.D_TYPE" %in% dbListTables(iadb))){
dbWriteTable(iadb,
             "META.D_TYPE",
             d_type_id %>% dplyr::rename(TYPE = type),
             overwrite = TRUE)
}

if(!("META.D_DATE" %in% dbListTables(iadb))){
dbWriteTable(iadb,
             "META.D_DATE",
             d_date_id %>% mutate(
               DATE = paste0(YEAR_ID, "-", MONTH_ID, "-", DAY_ID)
             ),
             overwrite = TRUE)
}

if(!("META.V_FILE_INFO" %in% dbListTables(iadb))){
#Add View with file info (human readable)
dbExecute(iadb,
           "CREATE VIEW [META.V_FILE_INFO] AS
           SELECT B.FILE_ID, FILE_NAME, LOC, TYPE, EXT, DATE, SIZE_MB, ROWS, 
           UNIQUE_LISTINGS
           FROM
           [META.D_FILE_NAME] AS A
           INNER JOIN [META.V_KEY_ID_MAPPINGS] AS B ON A.FILE_ID = B.FILE_ID
           INNER JOIN [META.D_LOCATION] AS C ON C.LOC_ID = B.LOC_ID
           INNER JOIN [META.D_TYPE] AS D ON D.TYPE_ID = B.TYPE_ID
           INNER JOIN [META.D_EXT] AS E ON E.EXT_ID = B.EXT_ID
           INNER JOIN [META.D_DATE] AS F ON F.YEAR_ID = B.YEAR_ID AND
           F.MONTH_ID = B.MONTH_ID AND F.DAY_ID = B.DAY_ID
           INNER JOIN [META.F_FILE_SIZE] AS G ON G.FILE_ID = A.FILE_ID")
}

#Clean up-----------------------------------------------------------------------
rm(m_loc_num_files, m_loc_file_info, d_date_id, d_ext_id, d_loc_id, d_type_id,
   m_file_key, m_file_contents, m_key_ids, m_loc_id)

