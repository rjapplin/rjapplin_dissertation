#Number of Listings by City-----------------------------------------------------
dbGetQuery(iadb, 
           "SELECT 
              SUBSTR(DATE, 1, 4) AS year,
              CITY_ID,
              COUNT(DISTINCT KEY_LIST_ID) AS listing_count
            FROM [CURATED.TAB_LISTING_ANALYTICS]
            GROUP BY SUBSTR(DATE, 1, 4), CITY_ID") %>%
  inner_join(
    dbReadTable(iadb, "META.D_LOCATION") %>%
      dplyr::rename(CITY_ID = LOC_ID) %>%
      mutate(CITY_ID = as.character(CITY_ID)),
    by = "CITY_ID"
  ) %>%
  select(-CITY_ID) %>%
  dbWriteTable(iaan, "FIGURE.LISTINGS_BY_LOC", .)
#Number of Listings Per Year for In-Sample Locations----------------------------
dbGetQuery(iadb,
           "SELECT 
              SUBSTR(DATE, 1, 4) AS year,
              COUNT(DISTINCT KEY_LIST_ID) AS listing_count
            FROM [CURATED.TAB_LISTING_ANALYTICS]
            WHERE 
              CAST(CITY_ID AS NUMERIC) IN (11, 12, 15, 23, 24, 25, 26, 28, 30, 33, 34, 38, 40)
              AND CAST(SUBSTR(DATE, 1, 4) AS NUMERIC) > 2014
              AND CAST(SUBSTR(DATE, 1, 4) AS NUMERIC) < 2024
            GROUP BY SUBSTR(DATE, 1, 4)") %>%
  dbWriteTable(iaan, "FIGURE.LISTINGS_BY_YEAR_INSAMP", .)

viz <- viz %>%
  ggplot(aes(x = year, y = listing_count/1000)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    xlab("Year") +
    ylab("Listing Count (Thousands)") +
    geom_text(
      data = viz[viz$listing_count == max(viz$listing_count),],
      aes(label = listing_count),
      vjust = -0.5
    ) +
    geom_text(
      data = viz[viz$listing_count == min(viz$listing_count),],
      aes(label = listing_count),
      vjust = -0.5
    ) 

#Number of Hosts Per Year for in-Sample Location--------------------------------
dbGetQuery(iadb,
                  "SELECT 
                    SUBSTR(DATE, 1, 4) AS year,
                    COUNT(DISTINCT KEY_HOST_ID) AS host_count
                  FROM [CURATED.TAB_LISTING_ANALYTICS]
                  WHERE 
                    CAST(CITY_ID AS NUMERIC) IN (11, 12, 15, 23, 24, 25, 26, 28, 30, 33, 34, 38, 40)
                    AND CAST(SUBSTR(DATE, 1, 4) AS NUMERIC) > 2014
                    AND CAST(SUBSTR(DATE, 1, 4) AS NUMERIC) < 2024
                  GROUP BY SUBSTR(DATE, 1, 4)") %>%
  dbWriteTable(iaan, "FIGURE.HOSTS_BY_YEAR_INSAMP", .)

#Listing to Host Ratio for In-Sample Locations----------------------------------
dbGetQuery(iadb,
                  "SELECT 
                    SUBSTR(DATE, 1, 4) AS year,
                    COUNT(DISTINCT KEY_LIST_ID)*1.0 / (COUNT(DISTINCT KEY_HOST_ID)) AS lhr
                  FROM [CURATED.TAB_LISTING_ANALYTICS]
                  WHERE 
                    CAST(CITY_ID AS NUMERIC) IN (11, 12, 15, 23, 24, 25, 26, 28, 30, 33, 34, 38, 40)
                    AND CAST(SUBSTR(DATE, 1, 4) AS NUMERIC) > 2014
                    AND CAST(SUBSTR(DATE, 1, 4) AS NUMERIC) < 2024
                  GROUP BY SUBSTR(DATE, 1, 4)") %>%
  dbWriteTable(iaan, "FIGURE.LIST_HOST_RATIO_BY_YEAR_INSAMP", .)

viz <- viz %>%
  mutate(lhr = round(lhr, 2))
  
viz <- viz %>%
  ggplot(aes(x = year, y = lhr)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("Year") +
  ylab("Listing to Host Ratio") +
  geom_text(
    data = viz[viz$lhr == max(viz$lhr),],
    aes(label = lhr),
    vjust = -0.5
  ) +
  geom_text(
    data = viz[viz$lhr == min(viz$lhr),],
    aes(label = lhr),
    vjust = -0.5
  )

#Real Prices by Year and Location for In-Sample Locations-----------------------
d <- dbGetQuery(iadb,
                  "SELECT 
                    SUBSTR(DATE, 1, 4) AS year,
                    CITY_ID,
                    price
                  FROM [CURATED.TAB_LISTING_ANALYTICS]
                  WHERE 
                    CAST(CITY_ID AS NUMERIC) IN (11, 12, 15, 23, 24, 25, 26, 28, 30, 33, 34, 38, 40)")

d %>%
  group_by(CITY_ID, year) %>%
  inner_join(
    cpi %>% group_by(year) %>% dplyr::summarize(cpi = mean(cpi_b2015)),
    by = "year"
  ) %>%
  mutate(price = (price/cpi)*100) %>%
  dplyr::summarize(
    Average = mean(price, na.rm = T),
    Median = median(price, na.rm = T),
    `25th Percentile` = quantile(price, probs = 0.25, na.rm = T),
    `75th Percentile` = quantile(price, probs = 0.75, na.rm = T),
    Max = max(price, na.rm = T),
    Min = min(price, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(Average, Median, `25th Percentile`, `75th Percentile`,
             Max, Min)
  ) %>%
  inner_join(
    dbReadTable(iadb, "META.D_LOCATION") %>%
      dplyr::rename(CITY_ID = LOC_ID) %>%
      mutate(CITY_ID = as.character(CITY_ID)),
    by = "CITY_ID"
  ) %>%
  ungroup() %>%
  select(-CITY_ID) %>%
  dplyr::rename(Location = LOC) %>%
  dplyr::rename(Statistic = name) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_STATS_BY_LOC_INSAMP", .)

#Nominal Prices by Year and Location for In-Sample Locations-------------------- 
d %>%
  group_by(CITY_ID, year) %>%
  dplyr::summarize(
    Average = mean(price, na.rm = T),
    Median = median(price, na.rm = T),
    `25th Percentile` = quantile(price, probs = 0.25, na.rm = T),
    `75th Percentile` = quantile(price, probs = 0.75, na.rm = T),
    Max = max(price, na.rm = T),
    Min = min(price, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(Average, Median, `25th Percentile`, `75th Percentile`,
             Max, Min)
  ) %>%
  inner_join(
    dbReadTable(iadb, "META.D_LOCATION") %>%
      dplyr::rename(CITY_ID = LOC_ID) %>%
      mutate(CITY_ID = as.character(CITY_ID)),
    by = "CITY_ID"
  ) %>%
  ungroup() %>%
  select(-CITY_ID) %>%
  dplyr::rename(Location = LOC) %>%
  dplyr::rename(Statistic = name) %>%
  dbWriteTable(iaan, "FIGURE.NOM_PRICE_STATS_BY_LOC_INSAMP", .)

  ggplot(aes(x = year, y = value, color = Statistic)) +
  geom_line(aes(group = Statistic, linetype = Statistic), linewidth = 1.2) +
  facet_wrap(~Location) +
  scale_color_manual(
    values = c("#FFD580", "#FF8C00", "#006400", "#8B0000", "#90EE90", "#FF7F7F")
  ) + 
  scale_linetype_manual(values = c("dashed", "dashed", "solid", "dotted", "solid", "dotted")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Year") +
  ylab("Nominal Price") -> viz2

#Real Price distribution Histogram by City--------------------------------------
d %>%
  inner_join(
    dbReadTable(iadb, "META.D_LOCATION") %>%
      dplyr::rename(CITY_ID = LOC_ID) %>%
      mutate(CITY_ID = as.character(CITY_ID)),
    by = "CITY_ID"
  ) %>%
  inner_join(
    cpi %>% group_by(year) %>% dplyr::summarize(cpi = mean(cpi_b2015)),
    by = "year"
  ) %>%
  mutate(price = (price/cpi)*100) %>%
  select(-CITY_ID, -year) %>%
  dbWriteTable(iaan, "FIGURE.ALL_PRICE_BY_CITY", .)  

#Nominal Price distribution Histogram by City--------------------------------------
d %>%
  inner_join(
    dbReadTable(iadb, "META.D_LOCATION") %>%
      dplyr::rename(CITY_ID = LOC_ID) %>%
      mutate(CITY_ID = as.character(CITY_ID)),
    by = "CITY_ID"
  ) %>%
  select(-CITY_ID, -year) %>%
  dbWriteTable(iaan, "FIGURE.ALL_NOM_PRICE_BY_CITY", .)  
    
  ggplot(aes(x = price, after_stat(density))) +
  geom_histogram() +
  facet_wrap(~LOC) +
  theme_bw() +
  ylab("Density") +
  xlab("Nominal Price") -> viz2

viz2 %>%
  ggsave(
    "paper/figures/price_by_location_hist_sample.png", plot = .,
    width = 22, height = 8.1)
rm(viz2)

#Real Prices by Year------------------------------------------------
d %>%
  inner_join(
    cpi %>% group_by(year) %>% dplyr::summarize(cpi = mean(cpi_b2015)),
    by = "year"
  ) %>%
  mutate(price = (price/cpi)*100) %>%
  group_by(year) %>%
  dplyr::summarize(
    Average = mean(price, na.rm = T),
    Median = median(price, na.rm = T),
    `25th Percentile` = quantile(price, probs = 0.25, na.rm = T),
    `75th Percentile` = quantile(price, probs = 0.75, na.rm = T),
    Max = max(price, na.rm = T),
    Min = min(price, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(Average, Median, `25th Percentile`, `75th Percentile`,
             Max, Min)
  ) %>%
  dplyr::rename(Statistic = name) %>%
  dbWriteTable(iaan, "FIGURE.PRICE_STATS_BY_YEAR",)

#Nominal Prices by Year------------------------------------------------
d %>%
  group_by(year) %>%
  dplyr::summarize(
    Average = mean(price, na.rm = T),
    Median = median(price, na.rm = T),
    `25th Percentile` = quantile(price, probs = 0.25, na.rm = T),
    `75th Percentile` = quantile(price, probs = 0.75, na.rm = T),
    Max = max(price, na.rm = T),
    Min = min(price, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(Average, Median, `25th Percentile`, `75th Percentile`,
             Max, Min)
  ) %>%
  dplyr::rename(Statistic = name) %>%
  dbWriteTable(iaan, "FIGURE.NOM_PRICE_STATS_BY_YEAR", .)

#Rating Distribution by City and Year for in-sample locations-------------------
d <- dbGetQuery(iadb,
                  "SELECT 
                    SUBSTR(DATE, 1, 4) AS year,
                    CITY_ID,
                    overall_rating
                  FROM [CURATED.TAB_LISTING_ANALYTICS]
                  WHERE 
                    CAST(CITY_ID AS NUMERIC) IN (11, 12, 15, 23, 24, 25, 26, 28, 30, 33, 34, 38, 40)")

d %>%
  group_by(CITY_ID, year) %>%
  dplyr::summarize(
    Average = mean(overall_rating, na.rm = T),
    Median = median(overall_rating, na.rm = T),
    `25th Percentile` = quantile(overall_rating, probs = 0.25, na.rm = T),
    `75th Percentile` = quantile(overall_rating, probs = 0.75, na.rm = T),
    Max = max(overall_rating, na.rm = T),
    Min = min(overall_rating, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(Average, Median, `25th Percentile`, `75th Percentile`,
             Max, Min)
  ) %>%
  inner_join(
    dbReadTable(iadb, "META.D_LOCATION") %>%
      dplyr::rename(CITY_ID = LOC_ID) %>%
      mutate(CITY_ID = as.character(CITY_ID)),
    by = "CITY_ID"
  ) %>%
  ungroup() %>%
  select(-CITY_ID) %>%
  dplyr::rename(Location = LOC) %>%
  dplyr::rename(Statistic = name) %>%
  dbWriteTable(iaan, "FIGURE.RATING_BY_YEAR_LOC_INSAMPL", .)
  
#Rating Distribution by City - Histogram--------------------------------
d %>%
  inner_join(
    dbReadTable(iadb, "META.D_LOCATION") %>%
      dplyr::rename(CITY_ID = LOC_ID) %>%
      mutate(CITY_ID = as.character(CITY_ID)),
    by = "CITY_ID"
  ) %>%
  select(-CITY_ID, -year) %>%
  dbWriteTable(iaan, "ALL_RATE_BY_YEAR_LOC_INSAMP", .)

#Rating by Year---------------------------------------------------------
d %>%
  group_by(year) %>%
  dplyr::summarize(
    Average = mean(overall_rating, na.rm = T),
    Median = median(overall_rating, na.rm = T),
    `25th Percentile` = quantile(overall_rating, probs = 0.25, na.rm = T),
    `75th Percentile` = quantile(overall_rating, probs = 0.75, na.rm = T),
    Max = max(overall_rating, na.rm = T),
    Min = min(overall_rating, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c(Average, Median, `25th Percentile`, `75th Percentile`,
             Max, Min)
  ) %>%
  dplyr::rename(Statistic = name) %>%
  dbWriteTable(iaan, "FIGURE.RATE_BY_YEAR_INSAMP", .)
  
#Rating Distribution - Histogram------------------------------------------------
viz %>%
  ggplot(aes(x = overall_rating, after_stat(density))) +
  geom_histogram() +
  theme_bw() +
  ylab("Density") +
  xlab("Overall Rating") -> viz2

viz2 %>%
  ggsave(
    "paper/figures/rating_hist_sample.png", plot = .,
    width = 22, height = 8.1)
rm(viz2)

