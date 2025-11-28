#PREPARED.V_F_LIST--------------------------------------------------------------
dbExecute(iadb,
           "CREATE VIEW [PREPARED.V_F_LIST] AS
            SELECT
              F_LIST_PRICE_D,
              F_LIST_GEOCODE,
              F_LIST_TYPE,
              F_LIST_ROOM_TYPE,
              F_LIST_CAPACITY,
              CASE
                WHEN F_LIST_BATHROOMS IS NOT NULL THEN F_LIST_BATHROOMS
                WHEN F_LIST_BATHROOMS IS NULL AND T_LIST_BATHROOMS_TEXT IS NOT NULL THEN T_LIST_BATHROOMS_TEXT
                ELSE NULL
                END AS F_LIST_BATHROOM,
              F_LIST_BEDROOMS,
              F_LIST_BEDS,
              F_LIST_MIN_NIGHT,
              F_LIST_MAX_NIGHT,
              F_LIST_HAS_AVAIL,
              F_LIST_HAS_AVAIL_30,
              F_LIST_HAS_AVAIL_60,
              F_LIST_HAS_AVAIL_90,
              F_LIST_HAS_AVAIL_365,
              F_LIST_NUM_REV,
              F_LIST_FIRST_REV_DT,
              F_LIST_LAST_REV_DT,
              F_LIST_RATE_OVERALL,
              F_LIST_RATE_ACC,
              F_LIST_RATE_CLEAN,
              F_LIST_RATE_CHECKIN,
              F_LIST_RATE_COM,
              F_LIST_RATE_LOC,
              F_LIST_RATE_VAL,
              C.*,
              T_LIST_DESCRIP_WORDS,
              T_LIST_LOCOVER_WORDS,
              T_LIST_DESCRIP_CHAR,
              T_LIST_LOCOVER_CHAR
            FROM
              [RAW.F_LIST] AS A 
            INNER JOIN
              (SELECT KEY_LIST_ID, KEY_FILE, T_LIST_BATHROOMS_TEXT FROM [RAW.T_LIST]) AS B
              ON A.KEY_LIST_ID = B.KEY_LIST_ID AND A.KEY_FILE = B.KEY_FILE
            INNER JOIN
              [RAW.F_LIST_AMENITY_INDICATORS] AS C
              ON A.KEY_LIST_ID = C.KEY_LIST_ID AND A.KEY_FILE = C.KEY_FILE
            INNER JOIN
              [RAW.T_LIST_COUNTS] AS D
              ON A.KEY_LIST_ID = D.KEY_LIST_ID AND A.KEY_FILE = D.KEY_FILE
            ")

#PREPARED.V_F_HOST--------------------------------------------------------------
dbExecute(iadb,
           "CREATE VIEW [PREPARED.V_F_HOST] AS
            SELECT
              A.KEY_FILE,
              A.KEY_HOST_ID,
              F_HOST_SINCE,
              F_HOST_RESP_TIME,
              F_HOST_RESP_RATE,
              F_HOST_ACPT_RATE,
              F_HOST_SUPERHOST,
              CASE 
                WHEN F_HOST_LIST_COUNT IS NOT NULL THEN F_HOST_LIST_COUNT
                WHEN F_HOST_LIST_COUNT IS NULL AND F_HOST_DUP_LIST_COUNT IS NOT NULL THEN F_HOST_DUP_LIST_COUNT
                ELSE NULL
              END AS F_HOST_LISTINGS_COUNT,
              F_HOST_PROFILE_PIC,
              F_HOST_VERIFIED,
              T_HOST_ABOUT_WORDS,
              T_HOST_ABOUT_CHAR
            FROM 
              [RAW.F_HOST] AS A
            INNER JOIN
              [RAW.T_HOST_COUNTS] AS B
            ON A.KEY_HOST_ID = B.KEY_HOST_ID AND A.KEY_FILE = B.KEY_FILE
          ")

#PREPARED.V_F_LIST_WITH_HOSTID--------------------------------------------------
dbExecute(iadb,
          "CREATE VIEW [PREPARED.V_F_LIST_WITH_HOSTID] AS
            SELECT
            A.*,
            B.KEY_HOST_ID
            FROM
              [PREPARED.V_F_LIST] AS A
            INNER JOIN
              [RAW.KEY_MASTER] AS B
            ON A.KEY_LIST_ID = B.KEY_LIST_ID AND A.KEY_FILE = B.KEY_FILE
          ")

#PREPARED.V_F_LIST_HOST----------------------------------------------------
dbExecute(iadb,
          "CREATE VIEW [PREPARED.V_F_LIST_HOST] AS
            SELECT
              A.*,
              F_HOST_SINCE,
              F_HOST_RESP_TIME,
              F_HOST_RESP_RATE,
              F_HOST_ACPT_RATE,
              F_HOST_SUPERHOST,
              F_HOST_LISTINGS_COUNT,
              F_HOST_PROFILE_PIC,
              F_HOST_VERIFIED,
              T_HOST_ABOUT_WORDS,
              T_HOST_ABOUT_CHAR
            FROM
              [PREPARED.V_F_LIST_WITH_HOSTID] AS A
            INNER JOIN
              [PREPARED.V_F_HOST] AS B
            ON A.KEY_HOST_ID = B.KEY_HOST_ID AND A.KEY_FILE = B.KEY_FILE")
