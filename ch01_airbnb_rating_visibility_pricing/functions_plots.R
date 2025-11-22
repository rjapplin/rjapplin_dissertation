#Master Function----------------------------------------------------------------
getPlotMaster <- function(plot, ...){
  
  #Aggregate Visualizations-----------------------------------------------------
  plotListByLoc <- function(){
    
    if("FIGURE.LISTINGS_BY_LOC" %in% dbListTables(iaan)){
      g <- dbReadTable(iaan, "FIGURE.LISTINGS_BY_LOC") %>%
        ggplot(aes(x = year, y = log(listing_count))) +
          geom_line(aes(group = 1)) +
          facet_wrap(~LOC) +
          theme_bw() +
          xlab("Year") +
          ylab("Log(Listing Count)")
    } else {
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
      g <- dbReadTable(iaan, "FIGURE.LISTINGS_BY_LOC")
    }
    return(g)
    
  }
  
  plotListByYearInSamp <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.LISTINGS_BY_YEAR_INSAMP")
    
    g <- d %>%
      ggplot(aes(x = year, y = listing_count/1000)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      xlab("Year") +
      ylab("Listing Count (Thousands)") +
      geom_text(
        data = d[d$listing_count == max(d$listing_count),],
        aes(label = listing_count),
        vjust = -0.5
      ) +
      geom_text(
        data = d[d$listing_count == min(d$listing_count),],
        aes(label = listing_count),
        vjust = -0.5
      ) 
    
    return(g)
    
  }
  
  plotHostByYearInSamp <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.HOSTS_BY_YEAR_INSAMP")
    
    g <- d %>%
      ggplot(aes(x = year, y = host_count/1000)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      xlab("Year") +
      ylab("Host Count (Thousands)") +
      geom_text(
        data = d[d$host_count == max(d$host_count),],
        aes(label = host_count),
        vjust = -0.5
      ) +
      geom_text(
        data = d[d$host_count == min(d$host_count),],
        aes(label = host_count),
        vjust = -0.5
      )
    
    return(g)
    
  }
  
  plotLHRatioByYearInSamp <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.LIST_HOST_RATIO_BY_YEAR_INSAMP") %>%
      mutate(lhr = round(lhr, 2))
    
    g <- d %>%
      ggplot(aes(x = year, y = lhr)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      xlab("Year") +
      ylab("Listing to Host Ratio") +
      geom_text(
        data = d[d$lhr == max(d$lhr),],
        aes(label = lhr),
        vjust = -0.5
      ) +
      geom_text(
        data = d[d$lhr == min(d$lhr),],
        aes(label = lhr),
        vjust = -0.5
      )
    
    return(g)
    
  }
  
  plotRealPriceStatsByYearCityInSamp <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICE_STATS_BY_LOC_INSAMP")
    
    g <- d %>%
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
      ylab("Real Price")
    
    return(g)
    
  }
  
  plotNomPriceStatsByYearCityInSamp <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.NOM_PRICE_STATS_BY_LOC_INSAMP")
    
    g <- d %>%
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
      ylab("Nominal Price")
    
    return(g)
    
  }
  
  plotRealPriceDistByCityInSamp <- function(){
    
    g <- dbReadTable(iaan, "FIGURE.ALL_PRICE_BY_CITY") %>%
      ggplot(aes(x = price, after_stat(density))) +
      geom_histogram() +
      facet_wrap(~LOC) +
      theme_bw() +
      ylab("Density")
    
    return(g)
    
  }
  
  plotNomPriceDistByCityInSamp <- function(){
    
    g <- dbReadTable(iaan, "FIGURE.ALL_NOM_PRICE_BY_CITY") %>%
      ggplot(aes(x = price, after_stat(density))) +
      geom_histogram() +
      facet_wrap(~LOC) +
      theme_bw() +
      ylab("Density") +
      xlab("Nominal Price")
    
    return(g)
    
  }
  
  plotRealPriceStatsByYearInSamp <- function(){
    
    g <- dbReadTable(iaan, "FIGURE.PRICE_STATS_BY_YEAR") %>%
      ggplot(aes(x = year, y = value, color = Statistic)) +
      geom_line(aes(group = Statistic, linetype = Statistic), linewidth = 1.2) +
      scale_color_manual(
        values = c("#FFD580", "#FF8C00", "#006400", "#8B0000", "#90EE90", "#FF7F7F")
      ) + 
      scale_linetype_manual(values = c("dashed", "dashed", "solid", "dotted", "solid", "dotted")) +
      theme_bw() +
      ylab("Real Price") + 
      xlab("Year") +
      theme(legend.position = "bottom")
    
    return(g)
    
  }
  
  plotNomPriceStatsByYearInSamp <- function(){
    
    g <- dbReadTable(iaan, "FIGURE.NOM_PRICE_STATS_BY_YEAR") %>%
      ggplot(aes(x = year, y = value, color = Statistic)) +
      geom_line(aes(group = Statistic, linetype = Statistic), linewidth = 1.2) +
      scale_color_manual(
        values = c("#FFD580", "#FF8C00", "#006400", "#8B0000", "#90EE90", "#FF7F7F")
      ) + 
      scale_linetype_manual(values = c("dashed", "dashed", "solid", "dotted", "solid", "dotted")) +
      theme_bw() +
      theme(legend.position = "bottom") +
      ylab("Nominal Price") +
      xlab("Year")
    
    return(g)
    
  }
  
  plotRealPriceDistInSamp <- function(){
    
    g <- dbReadTable(iaan, "FIGURE.ALL_PRICE_BY_CITY") %>%
      ggplot(aes(x = price, after_stat(density))) +
      geom_histogram() +
      theme_bw() +
      ylab("Density") +
      xlab("Real Price")
    
    return(g)
    
  }
  
  plotNomPriceDistInSamp <- function(){
    
    g <- dbReadTable(iaan, "FIGURE.ALL_NOM_PRICE_BY_CITY") %>%
      ggplot(aes(x = price, after_stat(density))) +
      geom_histogram() +
      theme_bw() +
      ylab("Density") +
      xlab("Nominal Price")
    
    return(g)
    
  }
  
  plotRateStatsByYearCityInSamp <- function(){
    
    g <- dbReadTable(iaan, "FIGURE.RATING_BY_YEAR_LOC_INSAMPL") %>%
      ggplot(aes(x = year, y = value, color = Statistic)) +
      geom_line(aes(group = Statistic, linetype = Statistic), linewidth = 1.2) +
      facet_wrap(~Location) +
      scale_color_manual(
        values = c("#FFD580", "#FF8C00", "#006400", "#8B0000", "#90EE90", "#FF7F7F")
      ) + 
      scale_linetype_manual(values = c("dashed", "dashed", "solid", "dotted", "solid", "dotted")) +
      theme_bw() +
      theme(legend.position = "bottom") +
      ylab("Overall Rating") +
      xlab("Year")
    
    return(g)
    
    
  }
  
  plotRateDistByCityInSamp <- function(){
    
    g <- dbReadTable(iaan, "ALL_RATE_BY_YEAR_LOC_INSAMP") %>%
      ggplot(aes(x = overall_rating, after_stat(density))) +
      geom_histogram() +
      facet_wrap(~LOC) +
      theme_bw() +
      ylab("Density") +
      xlab("Overall Rating")
    
    return(g)
    
  }
  
  plotRateStatsByYearInSamp <- function(){
    
    g <- dbReadTable(iaan, "FIGURE.RATE_BY_YEAR_INSAMP") %>%
      ggplot(aes(x = year, y = value, color = Statistic)) +
      geom_line(aes(group = Statistic, linetype = Statistic), linewidth = 1.2) +
      scale_color_manual(
        values = c("#FFD580", "#FF8C00", "#006400", "#8B0000", "#90EE90", "#FF7F7F")
      ) + 
      scale_linetype_manual(values = c("dashed", "dashed", "solid", "dotted", "solid", "dotted")) +
      theme_bw() +
      theme(legend.position = "bottom") +
      ylab("Overall Rating") +
      xlab("Year")
    
    return(g)
    
  }
  
  plotRateDistInSamp <- function(){
    
    g <- dbReadTable(iaan, "ALL_RATE_BY_YEAR_LOC_INSAMP") %>%
      ggplot(aes(x = overall_rating, after_stat(density))) +
      geom_histogram() +
      theme_bw() +
      ylab("Density") +
      xlab("Overall Rating")
    
    return(g)
    
  }
  
  #Variance of Features---------------------------------------------------------
  plotFeatVar <- function(){
    if("FEAT_VAR" %in% dbListTables(iaan)){
      d <- dbReadTable(iaan, "FEAT_VAR")
    } else {
      d <- dbGetQuery(iaan,
                      "SELECT KEY_LIST_ID, KEY_HOST_ID,
                      STDEV(price) price, STDEV(available_030) Q,
                      STDEV(bathrooms) bathrooms, STDEV(bedrooms) bedrooms,
                      STDEV(capacity) capacity,
                      STDEV(AC7) AC7, STDEV(AC72) AC72,
                      STDEV(AC51) AC51, STDEV(overall_rating) overall_rating,
                      STDEV(amenity_word_count) amenity_word_count,
                      STDEV(amenity_char_count) amenity_char_count,
                      STDEV(descrip_word_count) descrip_word_count,
                      STDEV(loc_overview_word_count) loc_overview_word_count,
                      STDEV(loc_overview_char_count) loc_overview_char_count,
                      STDEV(host_about_word_count) host_about_word_count,
                      STDEV(host_about_char_count) host_about_char_count,
                      STDEV(host_accept_rate) host_accept_rate,
                      STDEV(host_response_rate) host_response_rate,
                      STDEV(host_verified) host_verified,
                      STDEV(host_has_pic) host_has_pic,
                      STDEV(host_is_superhost) host_is_superhost
                      FROM [DATA.ANALYSIS_SAMPLE]
                      GROUP BY KEY_LIST_ID, KEY_HOST_ID")
      d <- d %>%
        select(-KEY_LIST_ID, -KEY_HOST_ID) %>%
        summary() %>%
        as.data.frame() %>%
        filter(grepl("Mean", Freq)) %>%
        mutate(Freq = gsub(":", "", Freq)) %>%
        mutate(Freq = gsub("Mean", "", Freq)) %>%
        mutate(Freq = as.numeric(Freq)) %>%
        select(Var2, Freq) %>%
        dplyr::rename(
          Feature = Var2,
          `Average SD` = Freq
        )
      dbWriteTable(iaan, "FEAT_VAR", d, overwrite = T)
      d <- dbReadTable(iaan, "FEAT_VAR")
    }
    
    g <- d %>%
      dplyr::rename(`Average SD` = `Average.SD`) %>%
      mutate(Feature = as.character(Feature)) %>%
      mutate(Limited = ifelse(
        Feature %in% c("host_verified", "host_response_rate",
                       "host_is_superhost", "host_accept_rate",
                       " host_has_pic", "bathrooms", "bedrooms",
                       "overall_rating") | grepl("AC", Feature),
        1, 0
      )) %>%
      mutate(
        Feature = ifelse(grepl("Q", Feature),
                         "booked_nights",
                         Feature)
      ) %>%
      arrange(Feature) %>%
      ggplot(aes(x = Feature, y = `Average SD`,
                 fill = `Average SD`)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      facet_wrap(~Limited, nrow = 2, scales = "free") +
      theme_bw()
    
    return(g)

  }
  #Correlation Plots------------------------------------------------------------
  plotNonAmenCorMat <- function(){
    
    cordata <- dbReadTable(iaan, "FIGURE.NON_AMEN_COR_MATRIX") 
    var_names <- unique(cordata$Var1)
    
    cordata %>%
      mutate(Var1 = factor(Var1, levels = var_names),
             variable = factor(variable, levels = var_names)) %>%
      ggplot(aes(x = Var1, y = variable, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                           midpoint = 0) +
      theme_bw() +
      xlab("") +
      ylab("") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) -> g
    
    return(g)
    
  }
  
  plotAmenCorMat <- function(){
    
    cordata <- dbReadTable(iaan, "FIGURE.AMEN_COR_MATRIX") 
    var_names <- unique(cordata$Var1)
    
    cordata %>%
      mutate(Var1 = factor(Var1, levels = var_names),
             variable = factor(variable, levels = var_names)) %>%
      ggplot(aes(x = Var1, y = variable, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                           midpoint = 0) +
      theme_bw() +
      xlab("") +
      ylab("") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) -> g
    
    return(g)
    
    
  }
  
  
  
  
  #Number of Observation and Event Time Statistics------------------------------
  plotMaxNoObs <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.MAX_OBS_PER_LIST")
    
    g1 <- d %>%
      ggplot(aes(x = as.factor(as.numeric(MaxObs)), y = Freq, fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Maximum Number of Observations") +
      ylab("Frequency") +
      ggtitle("Level")
    
    g2 <- d %>%
      ggplot(aes(x = as.factor(as.numeric(MaxObs)), y = log(Freq), fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Maximum Number of Observations") +
      ylab("Log(Frequency)") +
      ggtitle("Log")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  plotTimeOfEventFreq <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.TIME_OF_EVENT_FREQ") 
    
    d %>%
      ggplot(aes(as.factor(time_of_event), y = Freq)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Time of Treatment (Number of Obs. Until Treated)") +
      ylab("Log(Frequency)") +
      ggtitle("Level") -> g1
    
    d %>%
      ggplot(aes(as.factor(time_of_event), y = log(Freq))) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Time of Treatment (Number of Obs. Until Treated)") +
      ylab("Log(Frequency)") +
      ggtitle("Log") -> g2
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  plotNomEventTime <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.EVENT_TIME_FREQ")
    
    g1 <- d %>%
      ggplot(aes(as.factor(event_time), y = Freq)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Nominal Event Time (Number of Observations Before/After Time of Treatment)") +
      ylab("Frequency") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
      geom_vline(xintercept = which(d$event_time == "0"), color = "red") +
      geom_vline(xintercept = which(d$event_time == "-12"), color = "blue") +
      geom_vline(xintercept = which(d$event_time == "12"), color = "blue") +
      ggtitle("Level")
    
    g2 <- d %>%
      ggplot(aes(as.factor(event_time), y = log(Freq))) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Nominal Event Time (Number of Observations Before/After Time of Treatment)") +
      ylab("Log(Frequency)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
      geom_vline(xintercept = which(d$event_time == "0"), color = "red") +
      geom_vline(xintercept = which(d$event_time == "-12"), color = "blue") +
      geom_vline(xintercept = which(d$event_time == "12"), color = "blue") +
      ggtitle("Log(Level)")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  plotNomMinEventTime <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.MIN_EVENT_TIME_FREQ")
    
    g1 <- d %>%
      ggplot(aes(as.factor(min_event_time), y = Freq)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Minimum Nominal Event Time (Number of Observations Before/After Time of Treatment)") +
      ylab("Frequency") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_vline(xintercept = which(as.factor(d$min_event_time) == "0"), color = "red") +
      geom_vline(xintercept = which(as.factor(d$min_event_time) == "-12"), color = "blue") +
      ggtitle("Level")
    
    g2 <- d %>%
      ggplot(aes(as.factor(min_event_time), y = log(Freq))) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Minimum Nominal Event Time (Number of Observations Before/After Time of Treatment)") +
      ylab("Log(Frequency)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_vline(xintercept = -12) +
      geom_vline(xintercept = which(d$min_event_time == "0"), color = "red") +
      geom_vline(xintercept = which(d$min_event_time == "-12"), color = "blue") +
      ggtitle("Log(Level)")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  plotNomMaxEventTime <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.MAX_EVENT_TIME_FREQ")
    
    g1 <- d %>%
      ggplot(aes(as.factor(max_event_time), y = Freq)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Minimum Nominal Event Time (Number of Observations Before/After Time of Treatment)") +
      ylab("Frequency") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_vline(xintercept = which(as.factor(d$max_event_time) == "0"), color = "red") +
      geom_vline(xintercept = which(as.factor(d$max_event_time) == "-12"), color = "blue") +
      ggtitle("Level")
    
    g2 <- d %>%
      ggplot(aes(as.factor(max_event_time), y = log(Freq))) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Minimum Nominal Event Time (Number of Observations Before/After Time of Treatment)") +
      ylab("Log(Frequency)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_vline(xintercept = -12) +
      geom_vline(xintercept = which(d$max_event_time == "0"), color = "red") +
      geom_vline(xintercept = which(d$max_event_time == "12"), color = "blue") +
      ggtitle("Log(Level)")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  plotRealEventTimeM <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.REAL_EVENT_TIME_FREQ") %>%
      filter(real_type == "month") 
    
    g1 <- d %>%
      ggplot(aes(x = as.factor(real_event_time), y = Freq, fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Real Event Time (Number of Months Before/After Time of Treatment)") +
      ylab("Frequency") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) +
      geom_vline(xintercept = "0", color = "red") +
      geom_vline(xintercept = "-12", color = "blue") +
      geom_vline(xintercept = "12", color = "blue") +
      ggtitle("Level")
    
    g2 <- d %>%
      ggplot(aes(as.factor(real_event_time), y = log(Freq), fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Real Event Time (Number of Months Before/After Time of Treatment)") +
      ylab("Log(Frequency)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) +
      geom_vline(xintercept = "0", color = "red") +
      geom_vline(xintercept = "-12", color = "blue") +
      geom_vline(xintercept = "12", color = "blue") +
      ggtitle("Log(Level)")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  plotRealEventTimeQ <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.REAL_EVENT_TIME_FREQ") %>%
      filter(real_type == "quarter")  %>%
      mutate(Freq = as.numeric(Freq))
    
    g1 <- d %>%
      ggplot(aes(x = as.factor(real_event_time), y = Freq, fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Real Event Time (Number of Quarters Before/After Time of Treatment)") +
      ylab("Frequency") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) +
      geom_vline(xintercept = "0", color = "red") +
      geom_vline(xintercept = "-4", color = "blue") +
      geom_vline(xintercept = "4", color = "blue") +
      ggtitle("Level")
    
    g2 <- d %>%
      ggplot(aes(as.factor(real_event_time), y = log(Freq), fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Real Event Time (Number of Quarters Before/After Time of Treatment)") +
      ylab("Log(Frequency)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) +
      geom_vline(xintercept = "0", color = "red") +
      geom_vline(xintercept = "-4", color = "blue") +
      geom_vline(xintercept = "4", color = "blue") +
      ggtitle("Log(Level)")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  plotMinRealEventTimeM <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.MIN_REAL_EVENT_TIME_FREQ") %>%
      filter(real_type == "month") 
    
    g1 <- d %>%
      ggplot(aes(x = as.factor(min_real_event_time), y = Freq, fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Minimum Real Event Time (Number of Months Before/After Time of Treatment)") +
      ylab("Frequency") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) +
      geom_vline(xintercept = "0", color = "red") +
      geom_vline(xintercept = "-12", color = "blue") +
      geom_vline(xintercept = "12", color = "blue") +
      ggtitle("Level")
    
    g2 <- d %>%
      ggplot(aes(as.factor(min_real_event_time), y = log(Freq), fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Minimum Real Event Time (Number of Months Before/After Time of Treatment)") +
      ylab("Log(Frequency)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) +
      geom_vline(xintercept = "0", color = "red") +
      geom_vline(xintercept = "-12", color = "blue") +
      geom_vline(xintercept = "12", color = "blue") +
      ggtitle("Log(Level)")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  plotMaxRealEventTimeM <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.MAX_REAL_EVENT_TIME_FREQ") %>%
      filter(real_type == "month") 
    
    g1 <- d %>%
      ggplot(aes(x = as.factor(max_real_event_time), y = Freq, fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Maximum Real Event Time (Number of Months Before/After Time of Treatment)") +
      ylab("Frequency") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) +
      geom_vline(xintercept = "0", color = "red") +
      geom_vline(xintercept = "-12", color = "blue") +
      geom_vline(xintercept = "12", color = "blue") +
      ggtitle("Level")
    
    g2 <- d %>%
      ggplot(aes(as.factor(max_real_event_time), y = log(Freq), fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Maximum Real Event Time (Number of Months Before/After Time of Treatment)") +
      ylab("Log(Frequency)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) +
      geom_vline(xintercept = "0", color = "red") +
      geom_vline(xintercept = "-12", color = "blue") +
      geom_vline(xintercept = "12", color = "blue") +
      ggtitle("Log(Level)")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  plotMinRealEventTimeQ <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.MIN_REAL_EVENT_TIME_FREQ") %>%
      filter(real_type == "quarter") 
    
    g1 <- d %>%
      ggplot(aes(x = as.factor(min_real_event_time), y = Freq, fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Minimum Real Event Time (Number of Quarters Before/After Time of Treatment)") +
      ylab("Frequency") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) +
      geom_vline(xintercept = "0", color = "red") +
      geom_vline(xintercept = "-12", color = "blue") +
      geom_vline(xintercept = "12", color = "blue") +
      ggtitle("Level")
    
    g2 <- d %>%
      ggplot(aes(as.factor(min_real_event_time), y = log(Freq), fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Minimum Real Event Time (Number of Quarters Before/After Time of Treatment)") +
      ylab("Log(Frequency)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) +
      geom_vline(xintercept = "0", color = "red") +
      geom_vline(xintercept = "-12", color = "blue") +
      geom_vline(xintercept = "12", color = "blue") +
      ggtitle("Log(Level)")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  plotMaxRealEventTimeQ <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.MAX_REAL_EVENT_TIME_FREQ") %>%
      filter(real_type == "quarter") 
    
    g1 <- d %>%
      ggplot(aes(x = as.factor(max_real_event_time), y = Freq, fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Maximum Real Event Time (Number of Quarters Before/After Time of Treatment)") +
      ylab("Frequency") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) +
      geom_vline(xintercept = "0", color = "red") +
      geom_vline(xintercept = "-4", color = "blue") +
      geom_vline(xintercept = "4", color = "blue") +
      ggtitle("Level")
    
    g2 <- d %>%
      ggplot(aes(as.factor(max_real_event_time), y = log(Freq), fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_bw() +
      xlab("Maximum Real Event Time (Number of Quarters Before/After Time of Treatment)") +
      ylab("Log(Frequency)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) +
      geom_vline(xintercept = "0", color = "red") +
      geom_vline(xintercept = "-4", color = "blue") +
      geom_vline(xintercept = "4", color = "blue") +
      ggtitle("Log(Level)")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  
  
  #Price Dynamics by Event Time-------------------------------------------------
  plotPriceByNomEventTime <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICES_BY_NOM_EVENT_TIME")
    
    g <- d %>%
      mutate(name = case_when(
        name == "price" ~ "Nominal Price",
        name == "real_price" ~ "Real Price")
      ) %>%
      dplyr::rename(Series = name) %>%
      ggplot(aes(x = as.factor(event_time), y = value,
                 color = Series, group = Series)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      xlab("Nominal Event Time (Number of Observations Before/After Time of Treatment)") +
      ylab("Price") +
      geom_vline(xintercept = "0", color = "black")
    
    return(g)
    
  }
  
  plotPricesByRealEventTimeM <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICES_BY_REAL_EVENT_TIME_M")
    
    g <- d %>%
      mutate(name = case_when(
        name == "price" ~ "Nominal Price",
        name == "real_price" ~ "Real Price")
      ) %>%
      dplyr::rename(Series = name) %>%
      ggplot(aes(x = as.factor(real_event_time_m), y = value,
                 color = Series, group = Series)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      xlab("Real Event Time (Number of Months Before/After Time of Treatment)") +
      ylab("Price") +
      geom_vline(xintercept = "0", color = "black") +
      facet_wrap(~treatment_group, nrow = 2)
    
  }
  
  plotPricesByRealEventTimeQ <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICES_BY_REAL_EVENT_TIME_Q")
    
    g <- d %>%
      mutate(name = case_when(
        name == "price" ~ "Nominal Price",
        name == "real_price" ~ "Real Price")
      ) %>%
      dplyr::rename(Series = name) %>%
      ggplot(aes(x = as.factor(real_event_time_q), y = value,
                 color = Series, group = Series)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      xlab("Real Event Time (Number of Quarters Before/After Time of Treatment") +
      ylab("Price") +
      geom_vline(xintercept = "0", color = "black") +
      facet_wrap(~treatment_group, nrow = 2)
    
  }
  
  plotPricesByObsNo <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICES_BY_OBS_NO")
    
    d %>%
      mutate(name = case_when(
        name == "price" ~ "Nominal Price",
        name == "real_price" ~ "Real Price")
      ) %>%
      dplyr::rename(Series = name) %>%
      ggplot(aes(x = as.factor(lobs), y = value,
                 color = Series, group = Series)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      xlab("Observation Number") +
      ylab("Price") +
      geom_vline(xintercept = "0", color = "black") +
      facet_wrap(~treatment_group)
    
  }
  
  #Price Lag IC Plots-----------------------------------------------------------
  
  plotPriceLagIC <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICE_LAG_IC") 
    
    g <- d %>%
      dplyr::rename(Series = name) %>%
      mutate(
        Series = case_when(
          Series == "aic" ~ "AIC",
          Series == "bic" ~ "BIC"
        )
      ) %>%
      ggplot(aes(x = as.factor(plag), y = log(value))) +
      geom_line(group = 1) +
      geom_point(group = 1) +
      xlab("Lags in Autoregression") +
      ylab("Log(Information Criteria)") +
      facet_wrap(~Series, nrow = 2) +
      theme_bw()
    
    return(g)
    
  }
  
  plotRealPriceLagIC <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.REAL_PRICE_LAG_IC") 
    
    g <- d %>%
      dplyr::rename(Series = name) %>%
      mutate(
        Series = case_when(
          Series == "aic" ~ "AIC",
          Series == "bic" ~ "BIC"
        )
      ) %>%
      ggplot(aes(x = as.factor(plag), y = log(value))) +
      geom_line(group = 1) +
      geom_point(group = 1) +
      xlab("Lags in Autoregression") +
      ylab("Log(Information Criteria)") +
      facet_wrap(~Series, nrow = 2) +
      theme_bw()
    
    return(g)
    
  }
  
  #Amenity Selection------------------------------------------------------------
  plotPriceAmenImpThresh <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICE_AMENITY_FEAT_IMPORT_THRESHOLD")
    
    g1 <- d %>%
      dplyr::rename(Statistic = Stat) %>%
      filter(Statistic != "fstat") %>%
      mutate(
        Statistic = case_when(
          Statistic == "rsq" ~ "R Squared",
          Statistic == "arsq" ~ "Adj. R Squared"
        )
      ) %>%
      ggplot(aes(x = as.factor(threshold), y = value, color = Statistic,
                 group = Statistic)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      ylim(c(0.0566, 0.0568)) +
      xlab("Importance Threshold") +
      ylab("Statistic") +
      theme(legend.position = "top")
    
    g2 <- d %>%
      dplyr::rename(Statistic = Stat) %>%
      filter(Statistic == "fstat") %>%
      mutate(
        Statistic = "F Statistic"
      ) %>%
      ggplot(aes(x = as.factor(threshold), y = value)) +
      geom_line(group = 1) +
      geom_point() +
      theme_bw() +
      xlab("Importance Threshold") +
      ylab("F-Statistic")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  plotRealPriceAmenImpThresh <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.REAL_PRICE_AMENITY_FEAT_IMPORT_THRESHOLD")
    
    g1 <- d %>%
      dplyr::rename(Statistic = Stat) %>%
      filter(Statistic != "fstat") %>%
      mutate(
        Statistic = case_when(
          Statistic == "rsq" ~ "R Squared",
          Statistic == "arsq" ~ "Adj. R Squared"
        )
      ) %>%
      ggplot(aes(x = as.factor(threshold), y = value, color = Statistic,
                 group = Statistic)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      ylim(c(0.0610, 0.0612)) +
      xlab("Importance Threshold") +
      ylab("Statistic") +
      theme(legend.position = "top")
    
    g2 <- d %>%
      dplyr::rename(Statistic = Stat) %>%
      filter(Statistic == "fstat") %>%
      mutate(
        Statistic = "F Statistic"
      ) %>%
      ggplot(aes(x = as.factor(threshold), y = value)) +
      geom_line(group = 1) +
      geom_point() +
      theme_bw() +
      xlab("Importance Threshold") +
      ylab("F-Statistic")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  #Feature Selection------------------------------------------------------------
  plotPriceFetImpThresh <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICE_FEATURE_IMPORT_THRESHOLD")
    
    g1 <- d %>%
      dplyr::rename(Statistic = Stat) %>%
      filter(Statistic != "fstat") %>%
      mutate(
        Statistic = case_when(
          Statistic == "rsq" ~ "R Squared",
          Statistic == "arsq" ~ "Adj. R Squared"
        )
      ) %>%
      ggplot(aes(x = as.factor(threshold), y = value, color = Statistic,
                 group = Statistic)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      ylim(c(0.95097, 0.951)) +
      xlab("Importance Threshold") +
      ylab("Statistic") +
      theme(legend.position = "top")
    
    g2 <- d %>%
      dplyr::rename(Statistic = Stat) %>%
      filter(Statistic == "fstat") %>%
      mutate(
        Statistic = "F Statistic"
      ) %>%
      ggplot(aes(x = as.factor(threshold), y = value)) +
      geom_line(group = 1) +
      geom_point() +
      theme_bw() +
      xlab("Importance Threshold") +
      ylab("F-Statistic")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  plotRealPriceFetImpThresh <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.REAL_PRICE_FEATURE_IMPORT_THRESHOLD")
    
    g1 <- d %>%
      dplyr::rename(Statistic = Stat) %>%
      filter(Statistic != "fstat") %>%
      mutate(
        Statistic = case_when(
          Statistic == "rsq" ~ "R Squared",
          Statistic == "arsq" ~ "Adj. R Squared"
        )
      ) %>%
      ggplot(aes(x = as.factor(threshold), y = value, color = Statistic,
                 group = Statistic)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      #ylim(c(0.95097, 0.951)) +
      xlab("Importance Threshold") +
      ylab("Statistic") +
      theme(legend.position = "top")
    
    g2 <- d %>%
      dplyr::rename(Statistic = Stat) %>%
      filter(Statistic == "fstat") %>%
      mutate(
        Statistic = "F Statistic"
      ) %>%
      ggplot(aes(x = as.factor(threshold), y = value)) +
      geom_line(group = 1) +
      geom_point() +
      theme_bw() +
      xlab("Importance Threshold") +
      ylab("F-Statistic")
    
    g <- grid.arrange(g1, g2, nrow = 2)
    
    return(g)
    
  }
  
  #IV Candidates----------------------------------------------------------------
  plotHostRespIVCs <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICE_BY_ACPT_RATE") %>%
      mutate(var = "Accept Rate") %>%
      dplyr::rename(bin = host_accept_rate) %>%
      rbind(dbReadTable(iaan, "FIGURE.PRICE_BY_RESP_RATE") %>%
              mutate(var = "Response Rate") %>%
              dplyr::rename(bin = host_response_rate)) %>%
      rbind(dbReadTable(iaan, "FIGURE.PRICE_BY_RESP_TIME") %>%
              mutate(var = "Response Time") %>%
              dplyr::rename(bin = host_response_time)
      ) %>%
      mutate(
        name = ifelse(name == "price", "Price", "Real Price")
      ) %>%
      dplyr::rename(Group = treatment_group) %>%
      dplyr::rename(Price = value)
    
    d %>%
      ggplot(aes(x = bin, y = Price, fill = Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~name+var, scales = "free_x") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("")
    
    
  }
  
  plotHostVerPicSupIVCs <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICE_BY_HOST_VER") %>%
      mutate(var = "Host Verified") %>%
      dplyr::rename(bin = host_verified) %>%
      rbind(dbReadTable(iaan, "FIGURE.PRICE_BY_HOST_PIC") %>%
              mutate(var = "Host Has Picture") %>%
              dplyr::rename(bin = host_has_pic)) %>%
      rbind(dbReadTable(iaan, "FIGURE.PRICE_BY_HOST_SUPER") %>%
              mutate(var = "Superhost") %>%
              dplyr::rename(bin = host_is_superhost)
      ) %>%
      mutate(
        name = ifelse(name == "price", "Price", "Real Price")
      ) %>%
      dplyr::rename(Group = treatment_group) %>%
      dplyr::rename(Price = value)
    
    d %>%
      ggplot(aes(x = as.factor(bin), y = Price, fill = Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~name+var, scales = "free_x") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("")
    
    
  }
  
  plotAmenCounts <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICE_BY_AMEN_WORDS") %>%
      mutate(var = "Amenity Word Count") %>%
      dplyr::rename(bin = amenity_word_count) %>%
      rbind(dbReadTable(iaan, "FIGURE.PRICE_BY_AMEN_CHAR") %>%
              mutate(var = "Amenity Character Count") %>%
              dplyr::rename(bin = amenity_char_count)
      ) %>%
      mutate(
        name = ifelse(name == "price", "Price", "Real Price")
      ) %>%
      dplyr::rename(Group = treatment_group) %>%
      dplyr::rename(Price = value)
    
    d %>%
      ggplot(aes(x = bin, y = Price, fill = Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~name+var, scales = "free_x") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("")
    
    
  }
  
  plotDescripCounts <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICE_BY_DESC_WORDS") %>%
      mutate(var = "Description Word Count") %>%
      dplyr::rename(bin = descrip_word_count) %>%
      rbind(dbReadTable(iaan, "FIGURE.PRICE_BY_DESC_CHAR") %>%
              mutate(var = "Description Character Count") %>%
              dplyr::rename(bin = descrip_char_count)
      ) %>%
      mutate(
        name = ifelse(name == "price", "Price", "Real Price")
      ) %>%
      dplyr::rename(Group = treatment_group) %>%
      dplyr::rename(Price = value)
    
    d %>%
      ggplot(aes(x = bin, y = Price, fill = Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~name+var, scales = "free_x") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("")
    
    
  }
  
  plotAboutCounts <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICE_BY_HOST_WORDS") %>%
      mutate(var = "About Host Word Count") %>%
      dplyr::rename(bin = host_about_word_count) %>%
      rbind(dbReadTable(iaan, "FIGURE.PRICE_BY_HOST_CHAR") %>%
              mutate(var = "About Host Character Count") %>%
              dplyr::rename(bin = host_about_char_count)
      ) %>%
      mutate(
        name = ifelse(name == "price", "Price", "Real Price")
      ) %>%
      dplyr::rename(Group = treatment_group) %>%
      dplyr::rename(Price = value)
    
    d %>%
      ggplot(aes(x = bin, y = Price, fill = Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~name+var, scales = "free_x") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("")
    
    
  }
  
  plotLocCounts <- function(){
    
    d <- dbReadTable(iaan, "FIGURE.PRICE_BY_LOC_WORDS") %>%
      mutate(var = "About Location Word Count") %>%
      dplyr::rename(bin = loc_overview_word_count) %>%
      rbind(dbReadTable(iaan, "FIGURE.PRICE_BY_LOC_CHAR") %>%
              mutate(var = "About Location Character Count") %>%
              dplyr::rename(bin = loc_overview_char_count)
      ) %>%
      mutate(
        name = ifelse(name == "price", "Price", "Real Price")
      ) %>%
      dplyr::rename(Group = treatment_group) %>%
      dplyr::rename(Price = value)
    
    d %>%
      ggplot(aes(x = bin, y = Price, fill = Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~name+var, scales = "free_x") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("")
    
    
  }

  #Standard Event Studies-------------------------------------------------------
  getClassicES <- function(type){
    d <- dbReadTable(iaan, "MODEL.CLASSIC_EVENT_STUDIES") %>%
      mutate(target = ifelse(substr(id, 1, 1) == "n", "Nominal", "Real"),
             et_type = ifelse(substr(id, 2, 2) == "n", "Nominal", "Real"),
             Method = case_when(
               grepl("_ols_", id) ~ "OLS",
               grepl("_fe_", id) ~ "FE"
             )
      ) %>%
      mutate(
        includes = case_when(
          grepl("_all", id) ~ "All",
          grepl("_tnt", id) ~ "Treated and Never Treated",
          .default = "Treated Only"
        )
      )
    
    if(type == "nn"){
      g <- d %>% filter(target == "Nominal" & et_type == "Nominal") %>%
        filter(event_time != "999") %>%
        select(Estimate, `Std..Error`, event_time, includes, Method, n, nl) %>%
        ggplot(aes(x = as.factor(as.numeric(event_time)), y = Estimate, 
                   color = Method,
                   group = Method)) +
          geom_errorbar(width = 0.1, linetype = 2,
            aes(ymin = Estimate - 2.576*`Std..Error`,
                ymax = Estimate + 2.576*`Std..Error`)) +
          geom_line() +
          geom_point() +
          facet_wrap(~includes) +
        theme_bw() +
        xlab("Nominal Event Time (Observations Since Treatment)") +
        ggtitle("Nominal Price") +
        geom_text(aes(x = "-3", y = 2.4, 
                      label = paste0("No. Obs: ", n)),
                  size = 2, color = "black") +
        geom_text(aes(x = "-3", y = 2.2,
                      label = paste0("No. List: ", nl)),
                  size = 2, color = "black") +
        geom_hline(yintercept = 0, color = "black")
    } else if(type == "nr"){
      g <- d %>% filter(target == "Nominal" & et_type == "Real") %>%
        filter(event_time != "999") %>%
        select(Estimate, `Std..Error`, event_time, includes, Method, n, nl) %>%
        ggplot(aes(x = as.factor(as.numeric(event_time)), y = Estimate, 
                   color = Method,
                   group = Method)) +
        geom_errorbar(width = 0.1, linetype = 2,
                      aes(ymin = Estimate - 2.576*`Std..Error`,
                          ymax = Estimate + 2.576*`Std..Error`)) +
        geom_line() +
        geom_point() +
        facet_wrap(~includes) +
        theme_bw() +
        xlab("Real Event Time (Months Since Treatment)") +
        ggtitle("Nominal Price") +
        geom_text(aes(x = "-3", y = 10, 
                      label = paste0("No. Obs: ", n)),
                  size = 2, color = "black") +
        geom_text(aes(x = "-3", y = 8,
                      label = paste0("No. List: ", nl)),
                  size = 2, color = "black") +
        geom_hline(yintercept = 0, color = "black")
    } else if(type == "rn"){
      g <- d %>% filter(target == "Real" & et_type == "Nominal") %>%
        filter(event_time != "999") %>%
        select(Estimate, `Std..Error`, event_time, includes, Method, n, nl) %>%
        ggplot(aes(x = as.factor(as.numeric(event_time)), y = Estimate, 
                   color = Method,
                   group = Method)) +
        geom_errorbar(width = 0.1, linetype = 2,
                      aes(ymin = Estimate - 2.576*`Std..Error`,
                          ymax = Estimate + 2.576*`Std..Error`)) +
        geom_line() +
        geom_point() +
        facet_wrap(~includes) +
        theme_bw() +
        xlab("Nominal Event Time (Observations Since Treatment)") +
        ggtitle("Real Price") +
        geom_text(aes(x = "-3", y = 2.4, 
                      label = paste0("No. Obs: ", n)),
                  size = 2, color = "black") +
        geom_text(aes(x = "-3", y = 2.2,
                      label = paste0("No. List: ", nl)),
                  size = 2, color = "black") +
        geom_hline(yintercept = 0, color = "black") 
    } else if(type == "rr"){
      g <- d %>% filter(target == "Real" & et_type == "Real") %>%
        filter(event_time != "999") %>%
        select(Estimate, `Std..Error`, event_time, includes, Method, n, nl) %>%
        ggplot(aes(x = as.factor(as.numeric(event_time)), y = Estimate, 
                   color = Method,
                   group = Method)) +
        geom_errorbar(width = 0.1, linetype = 2,
                      aes(ymin = Estimate - 2.576*`Std..Error`,
                          ymax = Estimate + 2.576*`Std..Error`)) +
        geom_line() +
        geom_point() +
        facet_wrap(~includes) +
        theme_bw() +
        xlab("Real Event Time (Months Since Treatment)") +
        ggtitle("Real Price") +
        geom_text(aes(x = "-3", y = 10, 
                      label = paste0("No. Obs: ", n)),
                  size = 2, color = "black") +
        geom_text(aes(x = "-3", y = 8,
                      label = paste0("No. List: ", nl)),
                  size = 2, color = "black") +
        geom_hline(yintercept = 0, color = "black")
    }
    
    return(g)
    
    
  }
  
  #Standard Event Studies SH Only Bal-------------------------------------------------------
  getClassicESSH <- function(type){
    d <- dbReadTable(iaan, "MODEL.SH_ONLY_EVENTSTUDIES") %>%
      filter(!grepl("unbal", id)) %>%
      mutate(target = ifelse(substr(id, 1, 1) == "n", "Nominal", "Real"),
             et_type = ifelse(substr(id, 2, 2) == "n", "Nominal", "Real"),
             Method = case_when(
               grepl("_ols_", id) ~ "OLS",
               grepl("_fe_", id) ~ "FE"
             )
      ) %>%
      mutate(
        includes = case_when(
          grepl("_all", id) ~ "All",
          grepl("_tnt", id) ~ "Treated and Never Treated",
          .default = "Treated Only"
        )
      )
  
      g <- d %>% filter(target == "Real" & et_type == "Real") %>%
        filter(event_time != "999") %>%
        select(Estimate, `Std..Error`, event_time, includes, Method, n, nl) %>%
        ggplot(aes(x = as.factor(as.numeric(event_time)), y = Estimate, 
                   color = Method,
                   group = Method)) +
        geom_errorbar(width = 0.1, linetype = 2,
                      aes(ymin = Estimate - 2.576*`Std..Error`,
                          ymax = Estimate + 2.576*`Std..Error`)) +
        geom_line() +
        geom_point() +
        facet_wrap(~includes) +
        theme_bw() +
        xlab("Real Event Time (Months Since Treatment)") +
        ggtitle("Real Price") +
        geom_text(aes(x = "-3", y = 10, 
                      label = paste0("No. Obs: ", n)),
                  size = 2, color = "black") +
        geom_text(aes(x = "-3", y = 8,
                      label = paste0("No. List: ", nl)),
                  size = 2, color = "black") +
        geom_hline(yintercept = 0, color = "black")
    
    return(g)
    
    
  }
  
  #Standard Event Studies Unbalaced---------------------------------------------
  getClassicESUnbal <- function(type){
    d <- dbReadTable(iaan, "MODEL.CLASSIC_EVENT_STUDIES_UNBAL") %>%
      mutate(target = ifelse(substr(id, 1, 1) == "n", "Nominal", "Real"),
             et_type = ifelse(substr(id, 2, 2) == "n", "Nominal", "Real"),
             Method = case_when(
               grepl("_ols_", id) ~ "OLS",
               grepl("_fe_", id) ~ "FE"
             )
      ) %>%
      mutate(
        includes = case_when(
          grepl("_all", id) ~ "All",
          grepl("_tnt", id) ~ "Treated and Never Treated",
          .default = "Treated Only"
        )
      )
    
    if(type == "nn"){
      g <- d %>% filter(target == "Nominal" & et_type == "Nominal") %>%
        filter(event_time != "999") %>%
        select(Estimate, `Std..Error`, event_time, includes, Method, n, nl) %>%
        ggplot(aes(x = as.factor(as.numeric(event_time)), y = Estimate, 
                   color = Method,
                   group = Method)) +
        geom_errorbar(width = 0.1, linetype = 2,
                      aes(ymin = Estimate - 2.576*`Std..Error`,
                          ymax = Estimate + 2.576*`Std..Error`)) +
        geom_line() +
        geom_point() +
        facet_wrap(~includes) +
        theme_bw() +
        xlab("Nominal Event Time (Observations Since Treatment)") +
        ggtitle("Nominal Price") +
        geom_text(aes(x = "-3", y = 2.4, 
                      label = paste0("No. Obs: ", n)),
                  size = 2, color = "black") +
        geom_text(aes(x = "-3", y = 2.2,
                      label = paste0("No. List: ", nl)),
                  size = 2, color = "black") +
        geom_hline(yintercept = 0, color = "black")
    } else if(type == "nr"){
      g <- d %>% filter(target == "Nominal" & et_type == "Real") %>%
        filter(event_time != "999") %>%
        select(Estimate, `Std..Error`, event_time, includes, Method, n, nl) %>%
        ggplot(aes(x = as.factor(as.numeric(event_time)), y = Estimate, 
                   color = Method,
                   group = Method)) +
        geom_errorbar(width = 0.1, linetype = 2,
                      aes(ymin = Estimate - 2.576*`Std..Error`,
                          ymax = Estimate + 2.576*`Std..Error`)) +
        geom_line() +
        geom_point() +
        facet_wrap(~includes) +
        theme_bw() +
        xlab("Real Event Time (Months Since Treatment)") +
        ggtitle("Nominal Price") +
        geom_text(aes(x = "-3", y = 3, 
                      label = paste0("No. Obs: ", n)),
                  size = 2, color = "black") +
        geom_text(aes(x = "-3", y = 2,
                      label = paste0("No. List: ", nl)),
                  size = 2, color = "black") +
        geom_hline(yintercept = 0, color = "black")
    } else if(type == "rn"){
      g <- d %>% filter(target == "Real" & et_type == "Nominal") %>%
        filter(event_time != "999") %>%
        select(Estimate, `Std..Error`, event_time, includes, Method, n, nl) %>%
        ggplot(aes(x = as.factor(as.numeric(event_time)), y = Estimate, 
                   color = Method,
                   group = Method)) +
        geom_errorbar(width = 0.1, linetype = 2,
                      aes(ymin = Estimate - 2.576*`Std..Error`,
                          ymax = Estimate + 2.576*`Std..Error`)) +
        geom_line() +
        geom_point() +
        facet_wrap(~includes) +
        theme_bw() +
        xlab("Nominal Event Time (Observations Since Treatment)") +
        ggtitle("Real Price") +
        geom_text(aes(x = "-3", y = 2.4, 
                      label = paste0("No. Obs: ", n)),
                  size = 2, color = "black") +
        geom_text(aes(x = "-3", y = 2.2,
                      label = paste0("No. List: ", nl)),
                  size = 2, color = "black") +
        geom_hline(yintercept = 0, color = "black") 
    } else if(type == "rr"){
      g <- d %>% filter(target == "Real" & et_type == "Real") %>%
        filter(event_time != "999") %>%
        select(Estimate, `Std..Error`, event_time, includes, Method, n, nl) %>%
        ggplot(aes(x = as.factor(as.numeric(event_time)), y = Estimate, 
                   color = Method,
                   group = Method)) +
        geom_errorbar(width = 0.1, linetype = 2,
                      aes(ymin = Estimate - 2.576*`Std..Error`,
                          ymax = Estimate + 2.576*`Std..Error`)) +
        geom_line() +
        geom_point() +
        facet_wrap(~includes) +
        theme_bw() +
        xlab("Real Event Time (Months Since Treatment)") +
        ggtitle("Real Price") +
        geom_text(aes(x = "-3", y = 3, 
                      label = paste0("No. Obs: ", n)),
                  size = 2, color = "black") +
        geom_text(aes(x = "-3", y = 2,
                      label = paste0("No. List: ", nl)),
                  size = 2, color = "black") +
        geom_hline(yintercept = 0, color = "black")
    }
    
    return(g)
    
    
  }
  #Standard Event Studies UnbalSH-------------------------------------------------
  getClassicESSH <- function(type){
    d <- dbReadTable(iaan, "MODEL.SH_ONLY_EVENTSTUDIES") %>%
      filter(!grepl("unbal", id)) %>%
      mutate(target = ifelse(substr(id, 1, 1) == "n", "Nominal", "Real"),
             et_type = ifelse(substr(id, 2, 2) == "n", "Nominal", "Real"),
             Method = case_when(
               grepl("_ols_", id) ~ "OLS",
               grepl("_fe_", id) ~ "FE"
             )
      ) %>%
      mutate(
        includes = case_when(
          grepl("_all", id) ~ "All",
          grepl("_tnt", id) ~ "Treated and Never Treated",
          .default = "Treated Only"
        )
      )
    
    g <- d %>% filter(target == "Real" & et_type == "Real") %>%
      filter(event_time != "999") %>%
      select(Estimate, `Std..Error`, event_time, includes, Method, n, nl) %>%
      ggplot(aes(x = as.factor(as.numeric(event_time)), y = Estimate, 
                 color = Method,
                 group = Method)) +
      geom_errorbar(width = 0.1, linetype = 2,
                    aes(ymin = Estimate - 2.576*`Std..Error`,
                        ymax = Estimate + 2.576*`Std..Error`)) +
      geom_line() +
      geom_point() +
      facet_wrap(~includes) +
      theme_bw() +
      xlab("Real Event Time (Months Since Treatment)") +
      ggtitle("Real Price") +
      geom_text(aes(x = "-3", y = 10, 
                    label = paste0("No. Obs: ", n)),
                size = 2, color = "black") +
      geom_text(aes(x = "-3", y = 8,
                    label = paste0("No. List: ", nl)),
                size = 2, color = "black") +
      geom_hline(yintercept = 0, color = "black")
    
  }
    
    return(g)
  #CS Event Studies-------------------------------------------------------------
  getCSEs <- function(){
    
    d <- dbReadTable(iaan, "MODEL.CS_EVENT_STUDIES") %>%
      filter(!(is.na(est))) %>%
      group_by(et, control) %>%
      dplyr::mutate(
        est = mean(est, na.rm = T),
        est_se = mean(est_se, na.rm = T),
      ) %>%
      group_by(control) %>%
      dplyr::mutate(overall = mean(overall, na.rm = T),
                    overall_se = mean(overall_se, na.rm = T)) %>%
      select(et, control, est, est_se, overall, overall_se) %>%
      distinct() %>%
      mutate(control = case_when(
        control == "notyettreated" ~ "Not Yet Treated",
        control == "nevertreated" ~ "Never Treated"
      )) %>%
      dplyr::rename(Control = control)
    
    nyt_overall <- d %>%
      filter(Control == "Not Yet Treated") %>%
      ungroup() %>%
      select(overall, overall_se) %>%
      distinct() %>%
      as.matrix() %>%
      as.vector()
    
    nyt_overall <- paste0("Overall ATT Using Not Yet Treated: ",
      round(nyt_overall[1], 3), " +/- ", round(2.576*nyt_overall[2], 3)
    )
    
    nt_overall <- d %>%
      filter(Control == "Never Treated") %>%
      ungroup() %>%
      select(overall, overall_se) %>%
      distinct() %>%
      as.matrix() %>%
      as.vector()
    
    nt_overall <- paste0("Overall ATT Using Never Treated: ",
      round(nt_overall[1], 3), " +/- ", round(2.576*nt_overall[2], 3)
    )
    
    g <- d %>%
      ggplot(
        aes(x = as.factor(et), y = est, color = Control, group = Control)
      ) +
      geom_line() +
      geom_point() +
      geom_errorbar(aes(ymin = est - 2.576*est_se, ymax = est + 2.576*est_se),
                    width = 0.1, linetype = 2) +
      geom_hline(yintercept = 0) +
      theme_bw() +
      ylab("ATT(g, t) Estimate") +
      xlab("Real Event Time (Months Since Treatment)") +
      geom_text(aes(x = "-2", y = 26.75, label = nt_overall),
                size = 3, color = "black") +
      geom_text(aes(x = "-2", y = 23.75, label = nyt_overall),
                size = 3, color = "black")
    
    return(g)
      
    
  }
  #CS SH Only-------------------------------------------------------------------
  getCSSHEs <- function(){
    
    d <- dbReadTable(iaan, "MODEL.CS_SH_ONLY_EVENT_STUDIES") %>%
      filter(!(is.na(est))) %>%
      group_by(et, control) %>%
      dplyr::mutate(
        est = mean(est, na.rm = T),
        est_se = mean(est_se, na.rm = T),
      ) %>%
      group_by(control) %>%
      dplyr::mutate(overall = mean(overall, na.rm = T),
                    overall_se = mean(overall_se, na.rm = T)) %>%
      select(et, control, est, est_se, overall, overall_se) %>%
      distinct() %>%
      mutate(control = case_when(
        control == "notyettreated" ~ "Not Yet Treated",
        control == "nevertreated" ~ "Never Treated"
      )) %>%
      dplyr::rename(Control = control)
    
    nyt_overall <- d %>%
      filter(Control == "Not Yet Treated") %>%
      ungroup() %>%
      select(overall, overall_se) %>%
      distinct() %>%
      as.matrix() %>%
      as.vector()
    
    nyt_overall <- paste0("Overall ATT Using Not Yet Treated: ",
                          round(nyt_overall[1], 3), " +/- ", round(2.576*nyt_overall[2], 3)
    )
    
    nt_overall <- d %>%
      filter(Control == "Never Treated") %>%
      ungroup() %>%
      select(overall, overall_se) %>%
      distinct() %>%
      as.matrix() %>%
      as.vector()
    
    nt_overall <- paste0("Overall ATT Using Never Treated: ",
                         round(nt_overall[1], 3), " +/- ", round(2.576*nt_overall[2], 3)
    )
    
    g <- d %>%
      ggplot(
        aes(x = as.factor(et), y = est, color = Control, group = Control)
      ) +
      geom_line() +
      geom_point() +
      geom_errorbar(aes(ymin = est - 2.576*est_se, ymax = est + 2.576*est_se),
                    width = 0.1, linetype = 2) +
      geom_hline(yintercept = 0) +
      theme_bw() +
      ylab("ATT(g, t) Estimate") +
      xlab("Real Event Time (Months Since Treatment)") +
      geom_text(aes(x = "-2", y = 26.75, label = nt_overall),
                size = 3, color = "black") +
      geom_text(aes(x = "-2", y = 23.75, label = nyt_overall),
                size = 3, color = "black")
    
    return(g)
    
    
  }
  #BSTS Results-----------------------------------------------------------------
  plotAggBSTSES <- function(){
    d <- dbReadTable(iaan, "MODEL.BSTS_IND") %>%
      group_by(et) %>%
      dplyr::summarize(
        observed = mean(observed),
        fitted = mean(fitted),
        fitted_lo = mean(fitted_lo),
        fitted_hi = mean(fitted_hi),
        delta = mean(delta),
        delta_lo = mean(delta_lo),
        delta_hi = mean(delta_hi)
      )
    g1 <- d %>%
      filter(et >= -12 & et <= 12) %>%
      pivot_longer(cols = c(observed, fitted, delta)) %>%
      filter(name %in% c("observed", "fitted")) %>%
      mutate(fitted_lo = ifelse(name == "fitted", fitted_lo, value),
             fitted_hi = ifelse(name == "fitted", fitted_hi, value)) %>%
      dplyr::rename(Average = name) %>%
      ggplot(aes(x = as.factor(et), y = value,
                 group = Average, color = Average)) +
      geom_line(aes(y = value)) +
      geom_point(aes(y = value)) +
      geom_errorbar(aes(ymin = fitted_lo, ymax = fitted_hi),
                    width = .1, linetype = 2) +
      geom_vline(xintercept = "0") +
      theme_bw() +
      xlab("Real Event Time (Months to Treatment)") +
      ylab("Average Price") +
      theme(legend.position = "bottom")
    
    g2 <- d %>%
      filter(et >= -12 & et <= 12) %>%
      pivot_longer(cols = c(observed, fitted, delta)) %>%
      filter(name %in% c("delta")) %>%
      select(et, delta_lo, delta_hi, value) %>%
      ggplot(aes(x = as.factor(et), y = value)) +
      geom_line(aes(y = value), group = 1) +
      geom_errorbar(aes(ymin = delta_lo, ymax = delta_hi),
                    width = .1, linetype = 2) +
      geom_vline(xintercept = "0") +
      geom_point(aes(y = value)) +
      theme_bw() +
      xlab("Real Event Time (Months to Treatment)") +
      ylab("Average Estimated Treatment Effect")
    
    g <- grid.arrange(g1, g2)
    return(g)
    
  }
    
  plotBSTSBPByET <- function(){
    
    d <- dbReadTable(iaan, "MODEL.BSTS_IND") %>%
      filter(et >= -12 & et <= 12) %>%
      select(et, delta, delta_lo, delta_hi)
    
    g <- d %>%
      ggplot(aes(x = as.factor(et), y = delta)) +
      geom_boxplot() +
      theme_bw() +
      xlab("Real Event Time (Months to Treatment)") +
      ylab("Estimated Individual Treatment Effect") +
      geom_hline(yintercept = 0, linetype = 2, linewidth =0.5)
    
    return(g)
    
  }
  
  plotBSTSHetET <- function(){
    
    mr <- (dbReadTable(iaan, "MODEL.BSTS_IND") %>%
      dplyr::summarize(rating = mean(rating)))$rating
    d <- dbReadTable(iaan, "MODEL.BSTS_HET_BY_RATING") %>%
      filter(et != "<NA>") %>%
      mutate(interaction = grepl(":", Feature)) %>%
      select(Feature, Estimate, SE, method, interaction)
    
    d1 <- d %>%
      filter(interaction == TRUE) %>%
      mutate(Feature = gsub("rating:", "", Feature)) %>%
      dplyr::rename(Interaction = Estimate,
                    Interaction_SE = SE) %>%
      select(-interaction) %>%
      mutate(Interaction_mean = Interaction)
    
    d2 <- d %>%
      filter(Feature == "rating") %>%
      dplyr::rename(Int = Estimate,
                    Int_SE = SE) %>%
      select(Int, Int_SE, method)
    
    d <- d %>%
      filter(!(grepl("rating:", Feature))) %>%
      inner_join(d1, by = c("Feature", "method")) %>%
      select(-interaction) %>%
      inner_join(d2, by = c("method")) %>%
      group_by(Feature) %>%
      mutate(Est_SE = estSE(c(SE, Interaction_SE, Int_SE), 1000)) %>%
      mutate(min_est = Estimate + Interaction*0 + 0,
             mean_est = Estimate + Interaction_mean + Int*mr,
             max_est = Estimate + Interaction + Int) %>%
      select(min_est, mean_est, max_est, Feature, method, Est_SE) %>%
      mutate(et = as.numeric(gsub("[^0-9]", "", Feature))) %>%
      select(-Feature)
    
    g <- d %>%
      pivot_longer(cols = c(min_est, mean_est, max_est)) %>%
      mutate(name = case_when(
        name == "min_est" ~ "Estimate at Minimum Rating",
        name == "mean_est" ~ "Estimate at Mean Rating",
        name == "max_est" ~ "Estimate at Max Rating"
      )) %>%
      mutate(method = case_when(
        method == "ES_OLS" ~ "OLS",
        method == "ES_FE" ~ "FE"
      )) %>%
      select(-Feature) %>%
      filter(et <= 10) %>%
      dplyr::rename(Group = name) %>%
      ggplot(aes(x = as.factor(et), y = value, group = Group,
                 color = Group)) +
      geom_line() +
      geom_point() +
      facet_wrap(~method, nrow = 2) +
      geom_errorbar(aes(ymin = value - 2.576*Est_SE,
                    ymax = value + 2.576*Est_SE),
                    width = 0.1, linetype = 2) +
      geom_hline(yintercept = 0) +
      theme_bw() +
      xlab("Real Event Time (Months Since Treatment)") +
      ylab("Estimate")
    
    return(g)
                  
      
  }
  
  plotPlacAggBSTSES <- function(){
    d <- dbReadTable(iaan, "MODEL.BSTS_IND_PLAC") %>%
      group_by(et) %>%
      dplyr::summarize(
        observed = mean(observed),
        fitted = mean(fitted),
        fitted_lo = mean(fitted_lo),
        fitted_hi = mean(fitted_hi),
        delta = mean(delta),
        delta_lo = mean(delta_lo),
        delta_hi = mean(delta_hi)
      )
    g1 <- d %>%
      mutate(et = et + 3) %>%
      filter(et >= -12 & et <= 12) %>%
      pivot_longer(cols = c(observed, fitted, delta)) %>%
      filter(name %in% c("observed", "fitted")) %>%
      mutate(fitted_lo = ifelse(name == "fitted", fitted_lo, value),
             fitted_hi = ifelse(name == "fitted", fitted_hi, value)) %>%
      dplyr::rename(Average = name) %>%
      ggplot(aes(x = as.factor(et), y = value,
                 group = Average, color = Average)) +
      geom_line(aes(y = value)) +
      geom_point(aes(y = value)) +
      geom_errorbar(aes(ymin = fitted_lo, ymax = fitted_hi),
                    width = .1, linetype = 2) +
      geom_vline(xintercept = "0") +
      theme_bw() +
      xlab("Placebo Event Time") +
      ylab("Average Price")
    
    g2 <- d %>%
      mutate(et = et + 3) %>%
      filter(et >= -12 & et <= 12) %>%
      pivot_longer(cols = c(observed, fitted, delta)) %>%
      filter(name %in% c("delta")) %>%
      select(et, delta_lo, delta_hi, value) %>%
      ggplot(aes(x = as.factor(et), y = value)) +
      geom_line(aes(y = value), group = 1) +
      geom_errorbar(aes(ymin = delta_lo, ymax = delta_hi),
                    width = .1, linetype = 2) +
      geom_vline(xintercept = "0") +
      geom_point(aes(y = value)) +
      theme_bw() +
      xlab("Placebo Event Time") +
      ylab("Average Estimated Treatment Effect")
    
    g <- grid.arrange(g1, g2)
    return(g)
    
  }
        

  
  #BSTS SH Only-----------------------------------------------------------------
  plotAggBSTSESSH <- function(){
    d <- dbReadTable(iaan, "MODEL.BSTS_IND") %>%
      inner_join(
        dbGetQuery(iaan, "SELECT DISTINCT id FROM [DATA.BSTS_SAMPLE]
                   WHERE host_is_superhost = 1"),
        by = "id"
      ) %>%
      group_by(et) %>%
      dplyr::summarize(
        observed = mean(observed),
        fitted = mean(fitted),
        fitted_lo = mean(fitted_lo),
        fitted_hi = mean(fitted_hi),
        delta = mean(delta),
        delta_lo = mean(delta_lo),
        delta_hi = mean(delta_hi)
      )
    g1 <- d %>%
      filter(et >= -12 & et <= 12) %>%
      pivot_longer(cols = c(observed, fitted, delta)) %>%
      filter(name %in% c("observed", "fitted")) %>%
      mutate(fitted_lo = ifelse(name == "fitted", fitted_lo, value),
             fitted_hi = ifelse(name == "fitted", fitted_hi, value)) %>%
      dplyr::rename(Average = name) %>%
      ggplot(aes(x = as.factor(et), y = value,
                 group = Average, color = Average)) +
      geom_line(aes(y = value)) +
      geom_point(aes(y = value)) +
      geom_errorbar(aes(ymin = fitted_lo, ymax = fitted_hi),
                    width = .1, linetype = 2) +
      geom_vline(xintercept = "0") +
      theme_bw() +
      xlab("Real Event Time (Months to Treatment)") +
      ylab("Average Price") +
      theme(legend.position = "bottom")
    
    g2 <- d %>%
      filter(et >= -12 & et <= 12) %>%
      pivot_longer(cols = c(observed, fitted, delta)) %>%
      filter(name %in% c("delta")) %>%
      select(et, delta_lo, delta_hi, value) %>%
      ggplot(aes(x = as.factor(et), y = value)) +
      geom_line(aes(y = value), group = 1) +
      geom_errorbar(aes(ymin = delta_lo, ymax = delta_hi),
                    width = .1, linetype = 2) +
      geom_vline(xintercept = "0") +
      geom_point(aes(y = value)) +
      theme_bw() +
      xlab("Real Event Time (Months to Treatment)") +
      ylab("Average Estimated Treatment Effect")
    
    g <- grid.arrange(g1, g2)
    return(g)
    
  }
  
  plotBSTSBPByETSH <- function(){
    
    d <- dbReadTable(iaan, "MODEL.BSTS_IND") %>%
      inner_join(
        dbGetQuery(iaan, "SELECT DISTINCT id FROM [DATA.BSTS_SAMPLE]
                   WHERE host_is_superhost = 1"),
        by = "id"
      ) %>%
      filter(et >= -12 & et <= 12) %>%
      select(et, delta, delta_lo, delta_hi)
    
    g <- d %>%
      ggplot(aes(x = as.factor(et), y = delta)) +
      geom_boxplot() +
      theme_bw() +
      xlab("Real Event Time (Months to Treatment)") +
      ylab("Estimated Individual Treatment Effect") +
      geom_hline(yintercept = 0, linetype = 2, linewidth =0.5)
    
    return(g)
    
  }
  
  #IV Robustness----------------------------------------------------------------
  plotIvRob <- function(){
    
    g1 <- dbReadTable(iaan, "MODEL.IV_ROBUSTNESS_LOOP_RESULTS") %>%
      select(coef, Estimate) %>%
      na.omit() %>%
      mutate(coef = ifelse(coef == "T", "treated", "booked_nights")) %>%
      ggplot(aes(x = Estimate, y = "   Full Sample   ")) +
      geom_boxplot() +
      theme_bw() +
      facet_wrap(~coef, scales = "free_x") +
      ylab("") +
      xlab("")
    
    g2 <- dbReadTable(iaan, "MODEL.IV_ROBUSTNESS_LOOP_RESULTS_SHONLY") %>%
      select(coef, Estimate) %>%
      na.omit() %>%
      mutate(coef = ifelse(coef == "T", "treated", "booked_nights")) %>%
      ggplot(aes(x = Estimate, y = "Superhost Only")) +
      geom_boxplot() +
      theme_bw() +
      facet_wrap(~coef, scales = "free_x") +
      ylab("") +
      xlab("Coefficient Estimate")
    
    g <- grid.arrange(g1, g2)
    
    return(g)
    
  }
  
  plotIvFs <- function(){
    
    fs <- dbReadTable(iaan, "MODEL.iv_pretrend_tests") %>%
      filter(stage == "first_stage")
    ivs <- unique(fs$feature)
    plot_list <- list()
    for(i in 1:length(ivs)){
      
      temp <- fs %>%
        filter(feature == ivs[i]) %>% na.omit()
      ymin = min(-0.001, summary(temp$Estimate)[2])
      ymax = max(0.001, summary(temp$Estimate)[5])
      plot_list[[i]] <- fs %>%
        filter(feature == ivs[i]) %>%
        na.omit() %>%
        mutate(thresh = ifelse(thresh < 1, thresh, round(thresh))) %>%
        mutate(thresh = as.factor(thresh)) %>%
        dplyr::rename(`Exposure Threshold` = thresh) %>%
        ggplot(aes(
          x = as.factor(et), y = Estimate, color = `Exposure Threshold`
        )) +
        geom_point() +
        geom_errorbar(
          aes(ymin = Estimate - 2.576*`Std..Error`,
              ymax = Estimate + 2.576*`Std..Error`)
        ) +
        coord_cartesian(ylim = c(ymin, ymax)) +
        xlab("Event Time") +
        ylab("Estimate") +
        ggtitle(paste0(ivs[i])) +
        theme_bw() +
        geom_hline(yintercept = 0, color = "red")
      
    }
    
    g <- grid.arrange(
      plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
      plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
      plot_list[[9]], plot_list[[10]], plot_list[[11]], plot_list[[12]], ncol = 4
    )
    
    return(g)
  
  }
  
  plotIvSs <- function(){
    
    ss <- dbReadTable(iaan, "MODEL.iv_pretrend_tests") %>%
      filter(stage == "second_stage")
    ivs <- unique(fs$feature)
    plot_list <- list()
    for(i in 1:length(ivs)){
      
      temp <- ss %>%
        filter(feature == ivs[i]) %>% na.omit()
      ymin = min(-0.001, summary(temp$Estimate)[2])
      ymax = max(0.001, summary(temp$Estimate)[5])
      plot_list[[i]] <- temp %>%
        mutate(thresh = ifelse(thresh < 1, thresh, round(thresh))) %>%
        mutate(thresh = as.factor(thresh)) %>%
        dplyr::rename(`Exposure Threshold` = thresh) %>%
        ggplot(aes(
          x = as.factor(et), y = Estimate, color = `Exposure Threshold`
        )) +
        geom_point() +
        geom_errorbar(
          aes(ymin = Estimate - 2.576*`Std..Error`,
              ymax = Estimate + 2.576*`Std..Error`)
        ) +
        coord_cartesian(ylim = c(ymin, ymax)) +
        xlab("Event Time") +
        ylab("Estimate") +
        ggtitle(paste0(ivs[i])) +
        theme_bw() +
        geom_hline(yintercept = 0, color = "red")
      
    }
    
    g <- grid.arrange(
      plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
      plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
      plot_list[[9]], plot_list[[10]], plot_list[[11]], plot_list[[12]], ncol = 4
    )
    
    return(g)
    
  }
  
  #Rolling Reg------------------------------------------------------------------
  plotRollReg <- function(){
    
    df = dbReadTable(iaan, "MODEL.ROLLING_WINDOW_REG") %>%
      mutate(
        date_start = as.Date(paste0(
          substr(window_start, 1, 4), "-",
          substr(window_start, 5, 6), "-",
          substr(window_start, 7, 8)
        )),
        date_end = as.Date(paste0(
          substr(window_end, 1, 4), "-",
          substr(window_end, 5, 6), "-",
          substr(window_end, 7, 8)
        ))
      ) %>%
      mutate(
        date_mid = as.Date(date_start) + (365/2)
      ) %>%
      mutate(
        date_mid = as.Date(paste0(
          substr(date_mid, 1, 4), "-",
          substr(date_mid, 6, 7), "-",
          "01"
        ))
      )
    
    df %>%
      mutate(year = year(date_mid)) %>%
      group_by(year, feature, method) %>%
      dplyr::summarize(
        Estimate = mean(Estimate),
        `Std..Error` = mean(`Std..Error`)
      ) %>%
      ggplot(
        aes(x = as.factor(year), y = Estimate)
      ) +
      geom_point() +
      geom_errorbar(
        aes(ymin = Estimate - 2.567*`Std..Error`,
            ymax = Estimate + 2.567*`Std..Error`)
      ) +
      facet_wrap(~feature+method, ncol=4,
                 scales="free_y") +
      geom_hline(yintercept=0, color = "red")
    
  }
  #Plot-------------------------------------------------------------------------
  
  if(plot == "LISTINGS_BY_LOC"){
    g <- plotListByLoc()
  }
  
  if(plot == "LISTINGS_BY_YEAR_INSAMP"){
    g <- plotListByYearInSamp()
  }
  
  if(plot == "HOSTS_BY_YEAR_INSAMPL"){
    g <- plotHostByYearInSamp()
  }
  
  if(plot == "LIST_HOST_RATIO_BY_YEAR_INSAMP"){
    g <- plotLHRatioByYearInSamp()
  }
  
  if(plot == "PRICE_STATS_BY_LOC_INSAMP"){
    g <- plotRealPriceStatsByYearCityInSamp()
  }
  
  if(plot == "NOM_PRICE_STATS_BY_LOC_INSAMP"){
    g <- plotNomPriceStatsByYearCityInSamp()
  }
  
  if(plot == "REAL_PRICE_DIST_BY_CITY"){
    g <- plotRealPriceDistByCityInSamp()
  }
  
  if(plot == "NOM_PRICE_DIST_BY_CITY"){
    g <- plotNomPriceDistByCityInSamp()
  }
  
  if(plot == "REAL_PRICE_STATS_BY_YEAR"){
    g <- plotRealPriceStatsByYearInSamp()
  }
  
  if(plot == "NOM_PRICE_STATS_BY_YEAR"){
    g <- plotNomPriceStatsByYearInSamp()
  }
  
  if(plot == "REAL_PRICE_DIST"){
    g <- plotRealPriceDistInSamp()
  }
  
  if(plot == "NOM_PRICE_DIST"){
    g <- plotNomPriceDistInSamp()
  }
  
  if(plot == "RATE_STATS_BY_YEAR_LOC_INSAMP"){
    g <- plotRateStatsByYearCityInSamp()
  }
  
  if(plot == "RATE_DIST_BY_CITY_INSAMP"){
    g <- plotRateDistByCityInSamp()
  }
  
  if(plot == "RATE_STATS_BY_YEAR_INSAMP"){
    g <- plotRateStatsByYearInSamp()
  }
  
  if(plot == "RATE_DIST_INSAMP"){
    g <- plotRateDistInSamp()
  }
  
  if(plot == "FEATURE_VAR"){
    g <- plotFeatVar()
  }
  
  if(plot == "NON_AMEN_COR_MATRIX"){
    g <- plotNonAmenCorMat()
  }
  
  if(plot == "AMEN_COR_MATRIX"){
    g <- plotAmenCorMat()
  }
  
  if(plot == "MAX_OBS_PER_LIST"){
    g <- plotMaxNoObs()
  }
  
  if(plot == "TIME_OF_EVENT_FREQ"){
    g <- plotTimeOfEventFreq()
  }
  
  if(plot == "EVENT_TIME_FREQ"){
    g <- plotNomEventTime()
  }
  
  if(plot == "MIN_EVENT_TIME_FREQ"){
    g <- plotNomMinEventTime()
  }
  
  if(plot == "MAX_EVENT_TIME_FREQ"){
    g <- plotNomMaxEventTime()
  }
  
  if(plot == "REAL_EVENT_TIME_FREQ_M"){
    g <- plotRealEventTimeM()
  }
  
  if(plot == "REAL_EVENT_TIME_FREQ_Q"){
    g <- plotRealEventTimeQ()
  }
  
  if(plot == "MIN_REAL_EVENT_TIME_FREQ_M"){
    g <- plotMinRealEventTimeM()
  }
  
  if(plot == "MAX_REAL_EVENT_TIME_FREQ_M"){
    g <- plotMaxRealEventTimeM()
  }
  
  if(plot == "MIN_REAL_EVENT_TIME_FREQ_Q"){
    g <- plotMinRealEventTimeQ()
  }
  
  if(plot == "MAX_REAL_EVENT_TIME_FREQ_Q"){
    g <- plotMaxRealEventTimeQ()
  }
  
  if(plot == "PRICES_BY_NOM_EVENT_TIME"){
    g <- plotPriceByNomEventTime()
  }
  
  if(plot == "PRICES_BY_REAL_EVENT_TIME_M"){
    g <- plotPricesByRealEventTimeM()
  }
  
  if(plot == "PRICES_BY_OBS_NO"){
    g <- plotPricesByObsNo()
  }
  
  if(plot == "PRICE_LAG_IC"){
    g <- plotPriceLagIC()
  }
  
  if(plot == "REAL_PRICE_LAG_IC"){
    g <- plotRealPriceLagIC()
  }
  
  if(plot == "PRICE_AMENITY_FEAT_IMPORT_THRESHOLD"){
    g <- plotPriceAmenImpThresh()
  }
  
  if(plot == "REAL_PRICE_AMENITY_FEAT_IMPORT_THRESHOLD"){
    g <- plotRealPriceAmenImpThresh()
  }
  
  if(plot == "PRICE_FEATURE_IMPORT_THRESHOLD"){
    g <- plotPriceFetImpThresh()
  }
  
  if(plot == "REAL_PRICE_FEATURE_IMPORT_THRESHOLD"){
    g <- plotRealPriceFetImpThresh()
  }
  
  if(plot == "PRICE_BY_HOST_RATES"){
    g <- plotHostRespIVCs()
  }
  
  if(plot == "PRICE_BY_HOST_INFO"){
    g <- plotHostVerPicSupIVCs()
  }
  
  if(plot == "PRICE_BY_AMEN_COUNTS"){
    g <- plotAmenCounts()
  }
  
  if(plot == "PRICE_BY_DESCRIP_COUNTS"){
    g <- plotDescripCounts()
  }
  
  if(plot == "PRICE_BY_ABOUT_COUNTS"){
    g <- plotAboutCounts()
  }
  
  if(plot == "PRICE_BY_LOC_COUNTS"){
    g <- plotLocCounts()
  }
  
  if(plot == "CLASSIC_EVENT_STUDY"){
    g <- getClassicES(...)
  }
  
  if(plot == "CLASSIC_EVENT_STUDY_UNBAL"){
    g <- getClassicESUnbal(...)
  }
  
  if(plot == "CS_EVENT_STUDY"){
    g <- getCSEs()
  }
  
  if(plot == "BSTS_AGG_ES"){
    g <- plotAggBSTSES()
  }
  
  if(plot == "BSTS_BP"){
    g <- plotBSTSBPByET()
  }
  
  if(plot == "BSTS_HET_BY_RATE_ES"){
    g <- plotBSTSHetET()
  }
  
  if(plot == "BSTS_PLAC"){
    g <- plotPlacAggBSTSES()
  }
  
  if(plot == "IVROB"){
    g <- plotIvRob()
  }
  
  if(plot == "SHCS"){
    g <- getCSSHEs()
  }
  
  if(plot == "BSTS_ES_SH"){
    g <- plotAggBSTSESSH()
  }
  
  if(plot == "BSTS_BP_SH"){
    g <- plotBSTSBPByETSH()
  }
  
  return(g)
  
}
