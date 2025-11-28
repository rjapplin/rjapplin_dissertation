#Master Table Function----------------------------------------------------------
getTableMaster <- function(tabn){
  
  #SS No Breakout---------------------------------------------------------------
  getSumStats <- function(){
    
    tab <- dbReadTable(iaan,
                       "TABLE.SUM_STATS")
    return(tab)
    
  }
  
  getSumStatsListType <- function(){
    
    tab <- dbReadTable(iaan,
                       "TABLE.SUM_STATS_LIST_TYPE")
    return(tab)
    
  }
  
  getSumStatsRoomType <- function(){
    
    tab <- dbReadTable(iaan,
                       "TABLE.SUM_STATS_ROOM_TYPE")
    return(tab)
    
  }
  
  #SS By Treat------------------------------------------------------------------
  getSumStatsByTreat <- function(){
    
    tab <- dbReadTable(iaan,
                       "TABLE.SUM_STATS_BY_TREAT") %>%
      dplyr::rename(
        pta = `Treat...Always.p`,
        ptn = `Treat...Never.p`,
        pan = `Always...Never.p`
      )
    return(tab)
    
  }
  
  getSumStatsListTypeByTreat <- function(){
    
    tab <- dbReadTable(iaan,
                       "TABLE.SUM_STATS_LIST_TYPE_BY_TREAT") %>%
      dplyr::rename(
        pta = `Treat...Always.p`,
        ptn = `Treat...Never.p`,
        pan = `Always...Never.p`
      )
    return(tab)
    
  }
  
  getSumStatsRoomTypeByTreat <- function(){
    
    tab <- dbReadTable(iaan,
                       "TABLE.SUM_STATS_ROOM_TYPE_BY_TREAT") %>%
      dplyr::rename(
        pta = `Treat...Always.p`,
        ptn = `Treat...Never.p`,
        pan = `Always...Never.p`
      )
    return(tab)
    
  }
  
  #SS By After------------------------------------------------------------------
  getSumStatsByAfter <- function(){
    
    tab <- dbReadTable(iaan,
                       "TABLE.SUM_STATS_BY_AFTER") %>%
      dplyr::rename(p = `After...Before.p`)
    return(tab)
    
  }
  
  getSumStatsListTypeByAfter <- function(){
    
    tab <- dbReadTable(iaan,
                       "TABLE.SUM_STATS_LIST_TYPE_BY_AFTER") %>%
      dplyr::rename(p = `After...Before.p`)
    return(tab)
    
  }
  
  getSumStatsRoomTypeByAfter <- function(){
    
    tab <- dbReadTable(iaan,
                       "TABLE.SUM_STATS_ROOM_TYPE_BY_AFTER") %>%
      dplyr::rename(p = `After...Before.p`)
    return(tab)
    
  }
  
  #TWFE Regressions-------------------------------------------------------------
  getTwfeRes <- function(){
    
    if(file.exists("paper/tables/twfe.csv")){
      tab <- read.csv("paper/tables.twfe.csv")
    } else {
      ols_p <- dbReadTable(iaan, "MODEL.PRICE_OLS_FORMATTED")
      
      fe_p <- dbReadTable(iaan, "MODEL.PRICE_FE_FORMATTED")
      
      iv_p <- dbReadTable(iaan, "MODEL.PRICE_IV_LOOP_RES") %>%
        group_by(Feature) %>%
        dplyr::summarize(Estimate = mean(Estimate, na.rm = T),
                         SE = mean(SE, na.rm = T),
                         p = mean(p, na.rm = T)) %>%
        mutate(order_n = mapOrder(Feature)) %>%
        arrange(order_n) %>%
        filter(!(is.na(order_n))) %>%
        select(-order_n) %>%
        mutate(sig = ifelse(p < 0.01, "*", "")) %>%
        pivot_longer(cols = c(Estimate, SE, p)) %>%
        mutate(sig = ifelse(name == "Estimate", sig, "")) %>%
        mutate(sig = ifelse(is.na(sig), "", sig)) %>%
        filter(name != "p") %>%
        mutate(value = case_when(
          name == "Estimate" ~ paste0("$", round(value, 3), "^", sig, "$"),
          name == "SE" ~ paste0("$(", round(value, 3), ")$")
        )) %>%
        filter(value != "$(NaN)$" & value != "(NaN)") %>%
        mutate(Feature = ifelse(name == "SE", "", Feature)) %>%
        select(Feature, value) %>%
        dplyr::rename(IV = value)
       
      ivfe_p <- dbReadTable(iaan, "MODEL.PRICE_FEIV_LOOP_RES") %>%
        group_by(Feature) %>%
        dplyr::summarize(Estimate = mean(Estimate, na.rm = T),
                         SE = mean(SE, na.rm = T),
                         p = mean(p, na.rm = T)) %>%
        mutate(order_n = mapOrder(Feature)) %>%
        arrange(order_n) %>%
        filter(!(is.na(order_n))) %>%
        select(-order_n) %>%
        mutate(sig = ifelse(p < 0.01, "*", "")) %>%
        pivot_longer(cols = c(Estimate, SE, p)) %>%
        mutate(sig = ifelse(name == "Estimate", sig, "")) %>%
        mutate(sig = ifelse(is.na(sig), "", sig)) %>%
        filter(name != "p") %>%
        mutate(value = case_when(
          name == "Estimate" ~ paste0("$", round(value, 3), "^", sig, "$"),
          name == "SE" ~ paste0("$(", round(value, 3), ")$")
        )) %>%
        filter(value != "$(NaN)$" & value != "(NaN)") %>%
        mutate(Feature = ifelse(name == "SE", "", Feature)) %>%
        select(Feature, value) %>%
        dplyr::rename(IVFE = value)
      
      ols_rp <- dbReadTable(iaan, "MODEL.REAL_PRICE_OLS_FORMATTED")
      
      fe_rp <- dbReadTable(iaan, "MODEL.REAL_PRICE_FE_FORMATTED")
      
      iv_rp <- dbReadTable(iaan, "MODEL.REAL_PRICE_IV_LOOP_RES") %>%
        group_by(Feature) %>%
        dplyr::summarize(Estimate = mean(Estimate, na.rm = T),
                         SE = mean(SE, na.rm = T),
                         p = mean(p, na.rm = T)) %>%
        mutate(order_n = mapOrder(Feature)) %>%
        arrange(order_n) %>%
        filter(!(is.na(order_n))) %>%
        select(-order_n) %>%
        mutate(sig = ifelse(p < 0.01, "*", "")) %>%
        pivot_longer(cols = c(Estimate, SE, p)) %>%
        mutate(sig = ifelse(name == "Estimate", sig, "")) %>%
        mutate(sig = ifelse(is.na(sig), "", sig)) %>%
        filter(name != "p") %>%
        mutate(value = case_when(
          name == "Estimate" ~ paste0("$", round(value, 3), "^", sig, "$"),
          name == "SE" ~ paste0("$(", round(value, 3), ")$")
        )) %>%
        filter(value != "$(NaN)$" & value != "(NaN)") %>%
        mutate(Feature = ifelse(name == "SE", "", Feature)) %>%
        select(Feature, value) %>%
        dplyr::rename(IV = value)
      
      ivfe_rp <- dbReadTable(iaan, "MODEL.REAL_PRICE_FEIV_LOOP_RES") %>%
        group_by(Feature) %>%
        dplyr::summarize(Estimate = mean(Estimate, na.rm = T),
                         SE = mean(SE, na.rm = T),
                         p = mean(p, na.rm = T)) %>%
        mutate(order_n = mapOrder(Feature)) %>%
        arrange(order_n) %>%
        filter(!(is.na(order_n))) %>%
        select(-order_n) %>%
        mutate(sig = ifelse(p < 0.01, "*", "")) %>%
        pivot_longer(cols = c(Estimate, SE, p)) %>%
        mutate(sig = ifelse(name == "Estimate", sig, "")) %>%
        mutate(sig = ifelse(is.na(sig), "", sig)) %>%
        filter(name != "p") %>%
        mutate(value = case_when(
          name == "Estimate" ~ paste0("$", round(value, 3), "^", sig, "$"),
          name == "SE" ~ paste0("$(", round(value, 3), ")$")
        )) %>%
        filter(value != "$(NaN)$" & value != "(NaN)") %>%
        mutate(Feature = ifelse(name == "SE", "", Feature)) %>%
        select(Feature, value) %>%
        dplyr::rename(IVFE = value)
    
      tab <- cbind.fill(ols_p, fe_p, iv_p, ivfe_p,
                 ols_rp, fe_rp, iv_rp, ivfe_rp)
        write.csv(tab, "paper/tables/twfe.csv")
      
    }
    
    return(tab)
    
  }
  
  
  #TWFE By Rating---------------------------------------------------------------
  getTwfeByRate <- function(){
    
    if(file.exists("paper/tables/twfe_by_rate.csv")){
      tab <- read.csv("paper/tables.twfe_by_rate.csv")
    } else {
      ols_p <- dbReadTable(iaan, "MODEL.PRICE_OLS_BY_RATE_FORMATTED") %>%
        dplyr::mutate(Feature = case_when(
          Feature == "r" ~ "rating",
          Feature == "rating" ~ "treated*rating",
          .default = Feature
        ))
      
      fe_p <- dbReadTable(iaan, "MODEL.PRICE_FE_BY_RATE_FORMATTED") %>%
        dplyr::mutate(Feature = case_when(
          Feature == "r" ~ "rating",
          Feature == "rating" ~ "treated*rating",
          .default = Feature
        ))
      
      iv_p <- dbReadTable(iaan, "MODEL.PRICE_IV_BY_RATE_LOOP_RES")[2701:5400,] %>%
        group_by(Feature) %>%
        dplyr::summarize(Estimate = mean(Estimate, na.rm = T),
                         SE = mean(SE, na.rm = T),
                         p = mean(p, na.rm = T)) %>%
        mutate(Feature = case_when(
          Feature == "rated" ~ "rating",
          Feature == "treated*rated" ~ "treated*rating",
          .default = Feature
        )) %>%
        mutate(order_n = mapOrder(Feature)) %>%
        arrange(order_n) %>%
        filter(!(is.na(order_n))) %>%
        select(-order_n) %>%
        mutate(sig = ifelse(p < 0.01, "*", "")) %>%
        pivot_longer(cols = c(Estimate, SE, p)) %>%
        mutate(sig = ifelse(name == "Estimate", sig, "")) %>%
        mutate(sig = ifelse(is.na(sig), "", sig)) %>%
        filter(name != "p") %>%
        mutate(value = case_when(
          name == "Estimate" ~ paste0("$", round(value, 3), "^", sig, "$"),
          name == "SE" ~ paste0("$(", round(value, 3), ")$")
        )) %>%
        filter(value != "$(NaN)$" & value != "(NaN)") %>%
        mutate(Feature = ifelse(name == "SE", "", Feature)) %>%
        select(Feature, value) %>%
        dplyr::rename(IV = value)
      
      ivfe_p <- dbReadTable(iaan, "MODEL.PRICE_FEIV_BY_RATE_LOOP_RES") %>%
        group_by(Feature) %>%
        dplyr::summarize(Estimate = mean(Estimate, na.rm = T),
                         SE = mean(SE, na.rm = T),
                         p = mean(p, na.rm = T)) %>%
        mutate(Feature = case_when(
          Feature == "rated" ~ "rating",
          Feature == "treated*rated" ~ "treated*rating",
          .default = Feature
        )) %>%
        mutate(order_n = mapOrder(Feature)) %>%
        arrange(order_n) %>%
        filter(!(is.na(order_n))) %>%
        select(-order_n) %>%
        mutate(sig = ifelse(p < 0.01, "*", "")) %>%
        pivot_longer(cols = c(Estimate, SE, p)) %>%
        mutate(sig = ifelse(name == "Estimate", sig, "")) %>%
        mutate(sig = ifelse(is.na(sig), "", sig)) %>%
        filter(name != "p") %>%
        mutate(value = case_when(
          name == "Estimate" ~ paste0("$", round(value, 3), "^", sig, "$"),
          name == "SE" ~ paste0("$(", round(value, 3), ")$")
        )) %>%
        filter(value != "$(NaN)$" & value != "(NaN)") %>%
        mutate(Feature = ifelse(name == "SE", "", Feature)) %>%
        select(Feature, value) %>%
        dplyr::rename(IVFE = value)
      
      ols_rp <- dbReadTable(iaan, "MODEL.REAL_PRICE_OLS_BY_RATE_FORMATTED") %>%
        dplyr::mutate(Feature = case_when(
          Feature == "r" ~ "rating",
          Feature == "rating" ~ "treated*rating",
          .default = Feature
        ))
      
      fe_rp <- dbReadTable(iaan, "MODEL.REAL_PRICE_FE_BY_RATE_FORMATTED") %>%
        dplyr::mutate(Feature = case_when(
          Feature == "r" ~ "rating",
          Feature == "rating" ~ "treated*rating",
          .default = Feature
        ))
      
      iv_rp <- dbReadTable(iaan, "MODEL.REAL_PRICE_IV_BY_RATE_LOOP_RES") %>%
        group_by(Feature) %>%
        dplyr::summarize(Estimate = mean(Estimate, na.rm = T),
                         SE = mean(SE, na.rm = T),
                         p = mean(p, na.rm = T)) %>%
        dplyr::mutate(Feature = case_when(
          Feature == "rated" ~ "rating",
          Feature == "treated*rated" ~ "treated*rating",
          .default = Feature
        )) %>%
        mutate(order_n = mapOrder(Feature)) %>%
        arrange(order_n) %>%
        filter(!(is.na(order_n))) %>%
        select(-order_n) %>%
        mutate(sig = ifelse(p < 0.01, "*", "")) %>%
        pivot_longer(cols = c(Estimate, SE, p)) %>%
        mutate(sig = ifelse(name == "Estimate", sig, "")) %>%
        mutate(sig = ifelse(is.na(sig), "", sig)) %>%
        filter(name != "p") %>%
        mutate(value = case_when(
          name == "Estimate" ~ paste0("$", round(value, 3), "^", sig, "$"),
          name == "SE" ~ paste0("$(", round(value, 3), ")$")
        )) %>%
        filter(value != "$(NaN)$" & value != "(NaN)") %>%
        mutate(Feature = ifelse(name == "SE", "", Feature)) %>%
        select(Feature, value) %>%
        dplyr::rename(IV = value)
      
      ivfe_rp <- dbReadTable(iaan, "MODEL.REAL_PRICE_FEIV_BY_RATE_LOOP_RES") %>%
        group_by(Feature) %>%
        dplyr::summarize(Estimate = mean(Estimate, na.rm = T),
                         SE = mean(SE, na.rm = T),
                         p = mean(p, na.rm = T)) %>%
        dplyr::mutate(Feature = case_when(
          Feature == "rated" ~ "rating",
          Feature == "treated*rated" ~ "treated*rating",
          .default = Feature
        )) %>%
        mutate(order_n = mapOrder(Feature)) %>%
        arrange(order_n) %>%
        filter(!(is.na(order_n))) %>%
        select(-order_n) %>%
        mutate(sig = ifelse(p < 0.01, "*", "")) %>%
        pivot_longer(cols = c(Estimate, SE, p)) %>%
        mutate(sig = ifelse(name == "Estimate", sig, "")) %>%
        mutate(sig = ifelse(is.na(sig), "", sig)) %>%
        filter(name != "p") %>%
        mutate(value = case_when(
          name == "Estimate" ~ paste0("$", round(value, 3), "^", sig, "$"),
          name == "SE" ~ paste0("$(", round(value, 3), ")$")
        )) %>%
        filter(value != "$(NaN)$" & value != "(NaN)") %>%
        mutate(Feature = ifelse(name == "SE", "", Feature)) %>%
        select(Feature, value) %>%
        dplyr::rename(IVFE = value)
      
      tab <- cbind.fill(ols_p, fe_p, iv_p, ivfe_p,
                        ols_rp, fe_rp, iv_rp, ivfe_rp)
      write.csv(tab, "paper/tables/twfe_by_rate.csv")
      
    }
    
    return(tab)
    
  }
  
  #TWFE Treated Only------------------------------------------------------------
  getTwfeTrOnlyRes <- function(){
  
    tab <- dbReadTable(iaan, "MODEL.TWFE_TREATONLY_COI") %>%
      mutate(t = Estimate/SE) %>%
      mutate(p = round(2*(1 - pt(abs(t), 1000000)), 3)) %>%
      select(Feature, Estimate, target, method, rating_interaction, p) %>%
      dplyr::mutate(
        Feature = case_when(
          Feature == "treated_after" ~ "treated",
          Feature == "treated_after*rating" ~ "treated*rating",
          .default = Feature
        )
      ) %>%
      mutate(forder = case_when(
        Feature == "treated" ~ 1,
        Feature == "treated*rating" ~ 2,
        Feature == "rating" ~ 3,
        Feature == "booked_nights" ~ 4
      )) %>%
      mutate(morder = case_when(
        method == "OLS" ~ 1,
        method == "FE" ~ 2,
        method == "IV" ~ 3,
        method == "FEIV" ~ 4
      )) %>%
      dplyr::arrange(forder, morder, target) %>%
      select(Feature, method, target, rating_interaction, Estimate, p) %>%
      dplyr::rename(
        Method = method,
        Target = target,
        `Includes Rating` = rating_interaction,
        `p-value` = p
      ) %>%
      mutate(sig = ifelse(`p-value` <= 0.01, "*", "")) %>%
      mutate(Estimate = ifelse(sig == "", paste0("$", Estimate, "$"),
                               paste0("$", Estimate, "^", sig, "$"))) %>%
      select(-sig)
    
    return(tab)
    
  }
  
  #TWFE Short Event Window------------------------------------------------------
  getTwfeShortWin <- function(){
    tab <- dbReadTable(iaan, "MODEL.TWFE_SHORTWIN") %>%
      mutate(p = round(p, 3)) %>%
      mutate(forder = case_when(
        Feature == "treated" ~ 1,
        Feature == "treated*rating" ~ 2,
        Feature == "rating" ~ 3,
        Feature == "booked_nights" ~ 4
      )) %>%
      mutate(morder = case_when(
        Method == "OLS" ~ 1,
        Method == "FE" ~ 2,
        Method == "IV" ~ 3,
        Method == "FEIV" ~ 4
      )) %>%
      dplyr::arrange(forder, morder, Target) %>%
      dplyr::rename(
        `p-value` = p
      ) %>%
      select(-forder, -morder)
    
    return(tab)
    
  }
  
  
  #First Stage Regressions------------------------------------------------------
  getFirstStageRes <- function(){
    tp <- dbReadTable(iaan, "MODEL.PRICE_FIRST_STAGE") %>%
      mutate(sig = ifelse(p < 0.01, "^*", "")) %>%
      select(-p) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(-sig) %>%
      dplyr::rename(FS1 = Estimate)
    
    trp <- dbReadTable(iaan, "MODEL.REAL_PRICE_FIRST_STAGE") %>%
      mutate(sig = ifelse(p < 0.01, "^*", "")) %>%
      select(-p) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(-sig) %>%
      dplyr::rename(FS2 = Estimate)
    
    qp <- dbReadTable(iaan, "MODEL.PRICE_FIRST_STAGE_QUANTITY") %>%
      mutate(sig = ifelse(p < 0.01, "^*", "")) %>%
      select(-p) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(-sig) %>%
      dplyr::rename(FS3 = Estimate)
    
    qrp <- dbReadTable(iaan, "MODEL.REAL_PRICE_FIRST_STAGE_QUANTITY") %>%
      mutate(sig = ifelse(p < 0.01, "^*", "")) %>%
      select(-p) %>%
      mutate(Estimate = paste0("$", Estimate, sig, "$")) %>%
      select(-sig) %>%
      dplyr::rename(FS4 = Estimate)
    
    tab <- tp %>%
      inner_join(trp, by = "Feature") %>%
      inner_join(qp, by = "Feature") %>%
      inner_join(qrp, by = "Feature")
    
    return(tab)
    
  }
  
  #Tables-----------------------------------------------------------------------
  
  if(tabn == "SUM_STATS"){
    tabl <- getSumStats()
  }
  
  if(tabn == "SUM_STATS_LIST_TYPE"){
    tabl <- getSumStatsListType()
  }
  
  if(tabn == "SUM_STATS_ROOM_TYPE"){
    tabl <- getSumStatsRoomType()
  }
  
  if(tabn == "SUM_STATS_BY_TREAT"){
    tabl <- getSumStatsByTreat()
  }
  
  if(tabn == "SUM_STATS_LIST_TYPE_BY_TREAT"){
    tabl <- getSumStatsListTypeByTreat()
  }
  
  if(tabn == "SUM_STATS_ROOM_TYPE_BY_TREAT"){
    tabl <- getSumStatsRoomTypeByTreat()
  }
  
  if(tabn == "SUM_STATS_BY_AFTER"){
    tabl <- getSumStatsByAfter()
  }
  
  if(tabn == "SUM_STATS_LIST_TYPE_BY_AFTER"){
    tabl <- getSumStatsListTypeByAfter()
  }
  
  if(tabn == "SUM_STATS_ROOM_TYPE_BY_AFTER"){
    tabl <- getSumStatsRoomTypeByAfter()
  }
  
  if(tabn == "TWFE_RES"){
    tabl <- getTwfeRes(TWFE_RES)
  }
  
  if(tabn == "TWFE_BY_RATE_RES"){
    tabl <- getTwfeByRate()
  }

  if(tabn == "TWFE_TREATONLY_RES"){
    tabl <- getTwfeTrOnlyRes()
  }
  
  if(tabn == "TWFE_SHORTWIN"){
    tabl <- getTwfeShortWin()
  }
  
  return(tabl)
  
}


