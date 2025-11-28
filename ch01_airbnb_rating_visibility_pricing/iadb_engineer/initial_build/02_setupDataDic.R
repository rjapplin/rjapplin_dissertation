all_files <- dbReadTable(iadb, "META.V_FILE_INFO") %>%
  filter(TYPE == "listings" & EXT == "csv") %>%
  mutate(full_path = paste0("raw/", LOC, "/", FILE_NAME)
  )

#Form Variable History----------------------------------------------------------

#Create dataframe where each row represents a file in the data and all
#other columns are variable names. Each value indicates whether the variable
#is in the associated file.
for(i in 1:dim(all_files)[1]){
  
  if(i == 1){
    data_hist <- read.csv(all_files$full_path[i]) %>% select(-X) %>% colnames()
    data_hist <- as.data.frame(t(data_hist))
    colnames(data_hist) <- data_hist[1,]
    data_hist[1,] <- 1
    data_hist[] <- lapply(data_hist, as.numeric)
    data_hist$FILE <- all_files$FILE_ID[i]
  } else {
    
    temp <- read.csv(all_files$full_path[i]) %>% select(-X) %>% colnames()
    temp <- as.data.frame(t(temp))
    colnames(temp) <- temp[1,]
    temp[1,] <- 1
    temp[] <- lapply(temp, as.numeric)
    temp$FILE <- all_files$FILE_ID[i]
    
    data_hist <- bind_rows(data_hist, temp)
    
  }
  
  progress <- ((i/dim(all_files)[1])*100) %>% round(1)
  whole_percent <- ((progress %% 1) == 0)
  if(whole_percent == TRUE){
    print(progress)
  }
  
}
rm(progress, whole_percent, i)

#Condense into distinct rows for unique date-indicator combinations
data_hist_cond <- data_hist %>%
  mutate(DATE = parseKeyDate(FILE)) %>%
  select(-c(FILE)) %>%
  distinct() %>%
  arrange(DATE)


#Use Data History Object to form a data dictionary------------------------------
data_dic <- data.frame(
  #Raw name from the data
  RAW = colnames(data_hist),
  #Clean name to give field in wrangling
  CLEAN = c("KEY_LIST_ID",
    "M_LIST_URL",
    "M_SCRAPE_ID",
    "M_SCRAPE_LAST",
    "T_LIST_NAME",
    "T_LIST_SUMMARY",
    "T_LIST_SPACE",
    "T_LIST_DESCRIPTION",
    "T_LIST_EXPERIENCES",
    "T_LIST_LOC_OVERVIEW",
    "T_LIST_NOTES",
    "T_LIST_TRANSIT",
    "T_LIST_ACCESS",
    "T_LIST_INTERACTION",
    "T_LIST_RULES",
    "M_LIST_THUMBNAIL_URL",
    "M_LIST_MEDIUM_URL",
    "M_LIST_PICTURE_URL",
    "M_LIST_XL_PICTURE_URL",
    "KEY_HOST_ID",
    "M_HOST_URL",
    "T_HOST_NAME",
    "F_HOST_SINCE",
    "T_HOST_LOCATION",
    "T_HOST_ABOUT",
    "F_HOST_RESP_TIME",
    "F_HOST_RESP_RATE",
    "F_HOST_ACPT_RATE",
    "F_HOST_SUPERHOST",
    "M_HOST_THUMBNAIL_URL",
    "M_HOST_PICTURE_URL",
    "F_HOST_AREA",
    "F_HOST_LIST_COUNT",
    "F_HOST_DUP_LIST_COUNT",
    "F_HOST_VERIFICATIONS",
    "F_HOST_PROFILE_PIC",
    "F_HOST_VERIFIED",
    "F_LIST_STREET",
    "T_LIST_AREA",
    "F_LIST_GEOCODE",
    "T_LIST_AREA_GROUP",
    "F_LIST_CITY",
    "F_LIST_STATE",
    "F_LIST_ZIP",
    "F_LIST_MARKET",
    "F_LIST_SMART_LOC",
    "F_LIST_COUNTRY_CODE",
    "F_LIST_COUNTRY",
    "F_LIST_LAT",
    "F_LIST_LONG",
    "F_LIST_EXACT_LOC",
    "F_LIST_TYPE",
    "F_LIST_ROOM_TYPE",
    "F_LIST_CAPACITY",
    "F_LIST_BATHROOMS",
    "F_LIST_BEDROOMS",
    "F_LIST_BEDS",
    "F_LIST_BED_TYPE",
    "T_LIST_AMENITIES",
    "F_LIST_SQUARE_FT",
    "F_LIST_PRICE_D",
    "F_LIST_PRICE_W",
    "F_LIST_PRICE_M",
    "F_LIST_SEC_DEP",
    "F_LIST_CLEAN_FEE",
    "F_LIST_GUEST_IN_PRICE",
    "F_LIST_EXTRA_PRICE",
    "F_LIST_MIN_NIGHT",
    "F_LIST_MAX_NIGHT",
    "F_LIST_CAL_UPDATED",
    "F_LIST_HAS_AVAIL",
    "F_LIST_HAS_AVAIL_30",
    "F_LIST_HAS_AVAIL_60",
    "F_LIST_HAS_AVAIL_90",
    "F_LIST_HAS_AVAIL_365",
    "M_CAL_LAST_SCRAPE",
    "F_LIST_NUM_REV",
    "F_LIST_FIRST_REV_DT",
    "F_LIST_LAST_REV_DT",
    "F_LIST_RATE_OVERALL",
    "F_LIST_RATE_ACC",
    "F_LIST_RATE_CLEAN",
    "F_LIST_RATE_CHECKIN",
    "F_LIST_RATE_COM",
    "F_LIST_RATE_LOC",
    "F_LIST_RATE_VAL",
    "F_LIST_LICENSE_REQ",
    "T_LIST_LICENSE",
    "T_LIST_JURIS_NAME",
    "F_LIST_INSTANT_BOOK",
    "F_LIST_CANCEL_POL",
    "F_LIST_GUEST_PIC_REQ",
    "F_LIST_GUEST_VER_REQ",
    "F_HOST_CALC_LIST_COUNT",
    "F_LIST_REV_PER_M",
    "KEY_FILE",
    "F_LIST_BUS_READY",
    "F_LIST_MIN_MIN_NIGHT",
    "F_LIST_MAX_MIN_NIGHT",
    "F_LIST_MIN_MAX_NIGHT",
    "F_LIST_MAX_MAX_NIGHT",
    "F_LIST_AVG_MIN_NIGHT",
    "F_LIST_AVG_MAX_NIGHT",
    "F_LIST_NUM_REV_REV_LAST_Y",
    "F_HOST_CALC_HOME_COUNT",
    "F_HOST_CALC_PRIV_COUNT",
    "F_HOST_CALC_SHAR_COUNT",
    "T_LIST_BATHROOMS_TEXT",
    "F_LIST_NUM_REV_LAST_M",
    "F_LIST_SOURCE",
    "F_HOST_DDUP_LIST_COUNT"
    )
) %>%
  #Determine first appearance of each variable
  inner_join(
    data_hist %>%
      mutate(DATE = parseKeyDate(FILE)) %>%
      mutate(
        across(-DATE, ~ as.Date(ifelse(is.na(.), NA, DATE)))
      ) %>%
      mutate(
        across(-DATE, ~ min(., na.rm = TRUE))
      ) %>%
      select(-DATE) %>%
      distinct() %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column(var = "RAW") %>%
      dplyr::rename(FIRST_APPEAR = V1) %>%
      #Determine last appearance of each variable
      inner_join(
        (
          data_hist %>%
            mutate(DATE = parseKeyDate(FILE)) %>%
            mutate(
              across(-DATE, ~ as.Date(ifelse(is.na(.), NA, DATE)))
            ) %>%
            mutate(
              across(-DATE, ~ max(., na.rm = TRUE))
            ) %>%
            select(-DATE) %>%
            distinct() %>%
            t() %>%
            as.data.frame() %>%
            rownames_to_column(var = "RAW") %>%
            dplyr::rename(LAST_APPEAR = V1)
        )
      ),
    by = "RAW"
  ) %>%
  #Indicate whether variable is a dimension, metadata, text, or fact
  mutate(
    TYPE = case_when(
      substr(CLEAN, 1, 1) == "M" ~ "Metadata",
      substr(CLEAN, 1, 1) == "T" ~ "Text",
      substr(CLEAN, 1, 1) == "F" ~ "Fact",
      substr(CLEAN, 1, 3) == "KEY" ~ "Key"
    )
  ) %>%
  #Indicate unit of the variable
  mutate(
    UNIT = case_when(
      substr(CLEAN, 1,6) %in% 
        c("F_LIST", "M_LIST", "T_LIST", "KEY_LI") ~ "Listing",
      substr(CLEAN, 1,6) %in%
        c("F_HOST", "M_HOST", "T_HOST", "KEY_HO") ~ "Host",
      substr(CLEAN, 1, 6) == "KEY_FI" ~ "File",
      substr(CLEAN, 1, 6) == "M_SCRA" ~ "Scrape",
      substr(CLEAN, 1, 6) == "M_CAL_" ~ "Calendar"
    )
  ) %>%
  #Variable definitions
  mutate(
    DEF = c(
      "Unique listing identifier",
      "Airbnb url for the listing",
      "Identifier for the Inside Airbnb scrape a file is associated with",
      "Date when the data was scraped for a listing",
      "Listing name on Airbnb",
      "A summary of the listing",
      "A description of the space for rent",
      "A detailed description of the listing",
      "A description of experiences offered",
      "Host's description of the neighborhood",
      "Any other notes on the listing",
      "Information on transportation",
      "Information about accessing the property",
      "Information about interacting with the host/and or other property residents and/or guests",
      "Information about property rules",
      "URL for Airbnb thumbnail for the listing",
      "URL for a medium sized picture for the listing",
      "URL for a picture for the listing",
      "URL for a large picture for the listing",
      "Unique host identifier",
      "URL for Airbnb host profile",
      "Host name",
      "Date the host registered on Airbnb",
      "Host's self-reported location",
      "Host's description of themselves",
      "Average time to respond",
      "Rate at which host responds to booking inquiries",
      "Rate at which host accepts bookings",
      "Indicates whether host is a superhost",
      "URL for host thumbnail",
      "URL for host picture",
      "Location description for host",
      "Total number of listings a host has as calculated by Airbnb",
      "Total number of listings a host has as calculated by Airbnb",
      "Verified contact information for the host",
      "Indicator for whether host has a profile picture",
      "Indicator for whether host has identitiy verified",
      "Street of listing",
      "Location description for listing",
      "Geocode. For U.S. listings, zipcode",
      "Cleaner text description of location",
      "City of the listing",
      "State of the listing",
      "Zipcode of the listing",
      "Market of the listing",
      "Cleaned up location by Inside Airbnb",
      "Country code of the listing",
      "Country of the listing",
      "Latitude of the listing",
      "Longitude of the listing",
      "Indicator for whether location is exact",
      "Self-selected property type of the listing",
      "All properties are grouped into one of three types: Private room, Entire home/apt, or Shared room",
      "How many guests the listing accomodates",
      "The number of bathrooms",
      "The number of bedrooms",
      "The number of beds",
      "The type of beds",
      "List of amentities offered",
      "Listing square feet",
      "Daily price",
      "Weekly price",
      "Monthly price",
      "Security deposit",
      "Cleaning fee",
      "How many guests are included in the price",
      "Rate for extra people beyond number of guests included",
      "Minimum nights listing must be rented for",
      "Maximum nights listing can be rented for",
      "When listing availability calendar was last updated",
      "Indicator for whether listing has currently has availability",
      "Number of days of availability for the next 30 days",
      "Number of days of availability for the next 60 days",
      "Number of days of availability for the next 90 days",
      "Number of days of availability for the next 365 days",
      "Date calendar was last scraped",
      "Number of reviews the listing has",
      "Date of the first review",
      "Date of the last review",
      "Overall rating for the listing",
      "Average rating for accuracy of listing description",
      "Average rating for listing cleanliness",
      "Average rating for checkin experience",
      "Average rating for communication",
      "Average rating for location",
      "Average rating for value",
      "Indicator for whether hosts need a license to host their listing",
      "Text with license number/description",
      "Name of Jurisdiction",
      "Indicator for whether the listing is instant bookable",
      "Description of how strict cancellation policy is",
      "Indicator for whether guest profile picture is required",
      "Indicator for whether guest phone verification is required",
      "Inside Airbnb estimate of total number of listings a host has",
      "The number of reviews per month a listing has averaged",
      "Points to the raw datafile data is associated with",
      "Indicator for whether the listing is suitable for business travel",
      "Minimum of the listed minimum nights over the next 365 days",
      "Maximum of the listed minimum nights over the next 365 days",
      "Minimum of the listed maximum nights over the next 365 days",
      "Maximum of the listed maximum nights over the next 365 days",
      "Average listed minimum nights over next 365 days",
      "Average listed maximum nights over next 365 days",
      "Number of reviews over the last 365 days",
      "Inside Airbnb's count of the host's number of entire home listings",
      "Inside Airbnb's count of the host's number of private room listings",
      "Inside Airbnb's count of the host's number of hsared room listings",
      "Number of bathrooms (textual)",
      "Number of reviews over the last 30 days",
      "Indicates whether listing was found by searching the city or from being part of a previous scrape in the last 65 days",
      "Count of number of listings a host has"
    )
  )

#Write information to the iadb database-----------------------------------------
dbWriteTable(iadb, "META.M_FIELD_FILE_HIST", data_hist)
dbWriteTable(iadb, "META.M_FIELD_FILE_HIST_COND", data_hist_cond)
dbWriteTable(iadb, "META.M_FIELD_DICTIONARY", data_dic, overwrite = TRUE)
rm(data_dic, data_hist, data_hist_cond)
