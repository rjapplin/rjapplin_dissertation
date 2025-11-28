#Curated Listing Analytics View-------------------------------------------------
dbExecute(iadb,
           "CREATE VIEW [CURATED.V_LISTING_ANALYTICS] AS
            SELECT
              KEY_FILE,
              KEY_LIST_ID,
              KEY_HOST_ID,
              SUBSTR(KEY_FILE, 1, 2) AS CITY_ID,
              SUBSTR(KEY_FILE, 7, 8) AS DATE,
              CAST(REPLACE(F_LIST_PRICE_D, '$', '') AS DECIMAL(10, 2)) AS price,
              F_LIST_GEOCODE AS zip,
              F_LIST_TYPE AS listing_type,
              F_LIST_ROOM_TYPE AS room_type,
              CAST(F_LIST_CAPACITY AS INT ) AS capacity,
              CAST(F_LIST_BATHROOM AS NUMERIC) AS bathrooms,
              CAST(F_LIST_BEDROOMS AS NUMERIC) AS bedrooms,
              CAST(F_LIST_BEDS AS NUMERIC) AS beds,
              CAST(F_LIST_MIN_NIGHT AS NUMERIC) AS min_night,
              CAST(F_LIST_MAX_NIGHT AS NUMERIC) as max_night,
              CASE
                WHEN F_LIST_HAS_AVAIL = 't' THEN 1
                WHEN F_LIST_HAS_AVAIL = 'f' THEN 0
                ELSE NULL
              END AS available,
              CAST(F_LIST_HAS_AVAIL_30 AS INT) AS available_030,
              CAST(F_LIST_HAS_AVAIL_60 AS INT) AS available_060,
              CAST(F_LIST_HAS_AVAIL_90 AS INT) AS available_090,
              CAST(F_LIST_HAS_AVAIL_365 AS INT) AS available_365,
              CAST(F_LIST_NUM_REV AS INT) AS number_of_reviews,
              F_LIST_FIRST_REV_DT AS first_review_date,
              F_LIST_LAST_REV_DT AS last_review_date,
              CASE
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) >= 20210602 THEN CAST(F_LIST_RATE_OVERALL AS NUMERIC)
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) < 20210602 THEN CAST(F_LIST_RATE_OVERALL AS NUMERIC)/20.0 
              END AS overall_rating,
              CASE
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) >= 20210602 THEN CAST(F_LIST_RATE_ACC AS NUMERIC)
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) < 20210602 THEN CAST(F_LIST_RATE_ACC AS NUMERIC)/2.0 
              END AS accom_rating,
              CASE
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) >= 20210602 THEN CAST(F_LIST_RATE_CLEAN AS NUMERIC)
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) < 20210602 THEN CAST(F_LIST_RATE_CLEAN AS NUMERIC)/2.0 
              END AS clean_rating,
              CASE
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) >= 20210602 THEN CAST(F_LIST_RATE_CHECKIN AS NUMERIC)
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) < 20210602 THEN CAST(F_LIST_RATE_CHECKIN AS NUMERIC)/2.0 
              END AS checkin_rating,
              CASE
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) >= 20210602 THEN CAST(F_LIST_RATE_COM AS NUMERIC)
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) < 20210602 THEN CAST(F_LIST_RATE_COM AS NUMERIC)/2.0 
              END AS com_rating,
              CASE
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) >= 20210602 THEN CAST(F_LIST_RATE_LOC AS NUMERIC)
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) < 20210602 THEN CAST(F_LIST_RATE_LOC AS NUMERIC)/2.0 
              END AS loc_rating,
              CASE
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) >= 20210602 THEN CAST(F_LIST_RATE_val AS NUMERIC)
                WHEN CAST(SUBSTR(KEY_FILE, 7, 8) AS NUMERIC) < 20210602 THEN CAST(F_LIST_RATE_val AS NUMERIC)/2.0 
              END AS value_rating,
              AMENITY_WORD_COUNT AS amenity_word_count,
              AMENITY_COUNT AS amenity_count,
              AMENITY_CHAR_COUNT AS amenity_char_count,
              T_LIST_LOCOVER_WORDS AS loc_overview_word_count,
              T_LIST_DESCRIP_WORDS AS descrip_word_count,
              T_LIST_LOCOVER_CHAR AS loc_overview_char_count,
              T_LIST_DESCRIP_CHAR AS descrip_char_count,
              F_HOST_SINCE AS host_since,
              F_HOST_RESP_TIME AS host_response_time,
              CAST(REPLACE(F_HOST_RESP_RATE, '%', '') AS NUMERIC)/100.0 AS host_response_rate,
              CAST(REPLACE(F_HOST_ACPT_RATE, '%', '') AS NUMERIC)/100.0 AS host_accept_rate,
              CASE
                WHEN F_HOST_SUPERHOST = 't' THEN 1
                WHEN F_HOST_SUPERHOST = 'f' THEN 0
                ELSE NULL
              END AS host_is_superhost,
              CAST(F_HOST_LISTINGS_COUNT AS NUMERIC) AS host_listing_count,
              CASE
                WHEN F_HOST_PROFILE_PIC = 't' THEN 1
                WHEN F_HOST_PROFILE_PIC = 'f' THEN 0
                ELSE NULL
              END AS host_has_pic,
              CASE
                WHEN F_HOST_VERIFIED = 't' THEN 1
                WHEN F_HOST_VERIFIED = 'f' THEN 0
                ELSE NULL
              END AS host_verified,
              T_HOST_ABOUT_WORDS AS host_about_word_count,
              T_HOST_ABOUT_CHAR AS host_about_char_count,
              AC1,
              AC2,
              AC3,
              AC4,
              AC5,
              AC6,
              AC7,
              AC8,
              AC9,
              AC10,
              AC11,
              AC12,
              AC13,
              AC14,
              AC15,
              AC16,
              AC17,
              AC18,
              AC19,
              AC20,
              AC21,
              AC22,
              AC23,
              AC24,
              AC25,
              AC26,
              AC27,
              AC28,
              AC29,
              AC30,
              AC31,
              AC32,
              AC33,
              AC34,
              AC35,
              AC36,
              AC37,
              AC38,
              AC39,
              AC40,
              AC41,
              AC42,
              AC43,
              AC44,
              AC45,
              AC46,
              AC47,
              AC48,
              AC49,
              AC50,
              AC51,
              AC52,
              AC53,
              AC54,
              AC55,
              AC56,
              AC57,
              AC58,
              AC59,
              AC60,
              AC61,
              AC62,
              AC63,
              AC64,
              AC65,
              AC66,
              AC67,
              AC68,
              AC69,
              AC70,
              AC71,
              AC72,
              AC73,
              AC74,
              AC75,
              AC76,
              AC77,
              AC78,
              AC79,
              AC80,
              AC81,
              AC82
            FROM [PREPARED.V_F_LIST_HOST]")
