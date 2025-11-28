#Function to Convert to Real----------------------------------------------------

toReal <- function(value, cpi){
  return(value/(cpi)*100)
}

#Read in Data-------------------------------------------------------------------
raw_weekly <- direcs$dat.der %>%
  paste0("nielsen/aggregate/oneway/weekly/ow_wk_master.tsv") %>%
  read_tsv() %>%
  mutate(year = as.numeric(substr(week, 1, 4)),
         product_id = as.character(product_id),
         week = ymd(week),
         lsales = log(total_sales),
         lprice_u = log(avg_unit_price),
         lprice_sum = log(avg_sum_price),
         month = month(week),
         month2 = as.Date(paste0(year, "-", month, "-1")),
         quarter = quarter(week),
         quarter2 = as.Date(paste0(year, "-", quarter*3, "-1")),
         year2 = as.Date(paste0(year, "-1-1")),
         ) %>%
  inner_join(
    read_tsv(
      paste0(direcs$dat.ext, "fred/cpi_master.tsv")
    ), c("month", "year")
  ) %>%
  dplyr::mutate(rtotal_sales = toReal(total_sales, cpi/2.7),
                lrtotal_sales = log(rtotal_sales),
                ravg_price = toReal(avg_price, cpi/2.7),
                lravg_price = log(ravg_price),
                ravg_sum_price = toReal(avg_sum_price, cpi/2.7),
                lravg_sum_price = log(ravg_sum_price))

#-SALES-------------------------------------------------------------------------  

  #-Cigarettes Log Sales by Year--------------------------------------------------
  raw_weekly %>%
    filter(product_id == 7460) %>%
    ggplot(
      aes(week,
          lsales
    )) +
    geom_line() +
    facet_wrap(vars(year), scales = "free") +
    scale_x_date(date_breaks = "4 months", date_labels = "%b") +
    theme_economist_white(gray_bg = FALSE) +
    xlab("Week") + ylab("Log Dollar Sales") -> g
  
  ggsave(
    paste0(direcs$pap.fig,
           "nielsen/raw_aggregate/weekly_nom_sales_by_year_7460.png"),
    g
  )
  
  #-E-Cigarettes Log Sales by Year------------------------------------------------
    raw_weekly %>%
      filter(product_id == 7467) %>%
      ggplot(
        aes(week,
            lsales
        )) +
      geom_line() +
      facet_wrap(vars(year), scales = "free") +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Week") + ylab("Log Dollar Sales") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/weekly_nom_sales_by_year_7467.png"),
      g
    )
    
  #-Cigarette Sales by Month------------------------------------------------------
    raw_weekly %>%
      filter(product_id == 7460) %>%
      group_by(year, month) %>%
      dplyr::mutate(msales = sum(total_sales)) %>%
      ggplot(
        aes(month2,
            log(msales)
        )) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(year), scales = "free") +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Month") + ylab("Log Dollar Sales") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/monthly_nom_sales_by_year_7460.png"),
      g
    )
    
  #-E-Cigarette Sales by Month----------------------------------------------------
    raw_weekly %>%
      filter(product_id == 7467) %>%
      group_by(year, month) %>%
      dplyr::mutate(msales = sum(total_sales)) %>%
      ggplot(
        aes(month2,
            log(msales)
        )) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(year), scales = "free") +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Month") + ylab("Log Dollar Sales") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/monthly_nom_sales_by_year_7467.png"),
      g
    )
  
  #-Cigarette Sales by Quarter----------------------------------------------------  
    raw_weekly %>%
      filter(product_id == 7460) %>%
      group_by(year, quarter.x) %>%
      dplyr::mutate(qsales = sum(total_sales)) %>%
      ggplot(
        aes(quarter.x,
            log(qsales)
        )) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(year), scales = "free") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Log Dollar Sales") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_nom_sales_by_year_7460.png"),
      g
    )
    
  #-E-Cigarette Sales by Quarter--------------------------------------------------  
    raw_weekly %>%
      filter(product_id == 7467) %>%
      group_by(year, quarter.x) %>%
      dplyr::mutate(qsales = sum(total_sales)) %>%
      ggplot(
        aes(quarter.x,
            log(qsales)
        )) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(year), scales = "free") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Log Dollar Sales") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_nom_sales_by_year_7467.png"),
      g
    )
    
  #-Cigarette Sales by Quarter (Entire Time Period)-------------------------------
    raw_weekly %>%
      filter(product_id == 7460) %>%
      group_by(year, quarter.x) %>%
      dplyr::mutate(qsales = sum(total_sales)) %>%
      ggplot(
        aes(quarter2,
            log(qsales)
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "36 months", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Log Dollar Sales") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 color = color_pal$sunset, size = 1, linetype = "dashed") ->g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_nom_sales_7460.png"),
      g
    )
    
  #E-Cigarette Sales by Quarter (Entire Time Period)------------------------------
    raw_weekly %>%
      filter(product_id == 7467) %>%
      group_by(year, quarter.x) %>%
      dplyr::mutate(qsales = sum(total_sales)) %>%
      ggplot(
        aes(quarter2,
            log(qsales)
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "24 months", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Log Dollar Sales") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 color = color_pal$sunset, size = 1, linetype = "dashed") ->g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_nom_sales_7467.png"),
      g
    )
    
  #-Cigarette Sales Yearly--------------------------------------------------------
    raw_weekly %>%
      filter(product_id == 7460) %>%
      group_by(year) %>%
      dplyr::mutate(ysales = sum(total_sales)) %>%
      ggplot(
        aes(year2,
            log(ysales)
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "3 years", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Year") + ylab("Log Dollar Sales") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 color = color_pal$sunset, size = 1, linetype = "dashed") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/yearly_nom_sales_7460.png"),
      g
    )
    
  #E-Cigarette Sales Yearly-------------------------------------------------------
    raw_weekly %>%
      filter(product_id == 7467) %>%
      group_by(year) %>%
      dplyr::mutate(ysales = sum(total_sales)) %>%
      ggplot(
        aes(year2,
            log(ysales)
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "3 years", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Year") + ylab("Log Dollar Sales") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 color = color_pal$sunset, size = 1, linetype = "dashed") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/yearly_nom_sales_7467.png"),
      g
    )
  
    
    
#-REAL SALES--------------------------------------------------------------------
  
  #-Cigarettes Log Sales by Year REAL---------------------------------------------
    raw_weekly %>%
      filter(product_id == 7460) %>%
      ggplot(
        aes(week,
            lrtotal_sales
        )) +
      geom_line() +
      facet_wrap(vars(year), scales = "free") +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Week") + ylab("Log Dollar Sales - Constant 2021 Dollars") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/weekly_real_sales_by_year_7460.png"),
      g
    )
    
  #-E-Cigarettes Log Sales by Year REAL-------------------------------------------
    raw_weekly %>%
      filter(product_id == 7467) %>%
      ggplot(
        aes(week,
            lrtotal_sales
        )) +
      geom_line() +
      facet_wrap(vars(year), scales = "free") +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Week") + ylab("Log Dollar Sales - Constant 2021 Dollars") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/weekly_real_sales_by_year_7467.png"),
      g
    )
    
  #-Cigarette Sales by Month REAL-------------------------------------------------
    raw_weekly %>%
      filter(product_id == 7460) %>%
      group_by(year, month) %>%
      dplyr::mutate(msales = sum(rtotal_sales)) %>%
      ggplot(
        aes(month2,
            log(msales)
        )) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(year), scales = "free") +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Month") + ylab("Log Dollar Sales - Constant 2021 Dollars") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/monthly_real_sales_by_year_7460.png"),
      g
    )
    
  #-E-Cigarette Sales by Month REAL-----------------------------------------------
    raw_weekly %>%
      filter(product_id == 7467) %>%
      group_by(year, month) %>%
      dplyr::mutate(msales = sum(rtotal_sales)) %>%
      ggplot(
        aes(month2,
            log(msales)
        )) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(year), scales = "free") +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Month") + ylab("Log Dollar Sales - Constant 2021 Dollars") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/monthly_real_sales_by_year_7467.png"),
      g
    )
    
  #-Cigarette Sales by Quarter REAL----------------------------------------------- 
    raw_weekly %>%
      filter(product_id == 7460) %>%
      group_by(year, quarter.x) %>%
      dplyr::mutate(qsales = sum(rtotal_sales)) %>%
      ggplot(
        aes(quarter.x,
            log(qsales)
        )) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(year), scales = "free") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Log Dollar Sales - Constant 2021 Dollars") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_real_sales_by_year_7460.png"),
      g
    )
    
  #-E-Cigarette Sales by Quarter REAL---------------------------------------------
    raw_weekly %>%
      filter(product_id == 7467) %>%
      group_by(year, quarter.x) %>%
      dplyr::mutate(qsales = sum(rtotal_sales)) %>%
      ggplot(
        aes(quarter.x,
            log(qsales)
        )) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(year), scales = "free") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Log Dollar Sales - Constant 2021 Dollars") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_real_sales_by_year_7467.png"),
      g
    )
    
  #-Cigarette Sales by Quarter (Entire Time Period) REAL--------------------------
    raw_weekly %>%
      filter(product_id == 7460) %>%
      group_by(year, quarter.x) %>%
      dplyr::mutate(qsales = sum(rtotal_sales)) %>%
      ggplot(
        aes(quarter2,
            log(qsales)
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "36 months", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Log Dollar Sales - Constant 2021 Dollars") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 color = color_pal$sunset, size = 1, linetype = "dashed") ->g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_real_sales_7460.png"),
      g
    )
    
  #E-Cigarette Sales by Quarter (Entire Time Period) REAL-------------------------
    raw_weekly %>%
      filter(product_id == 7467) %>%
      group_by(year, quarter.x) %>%
      dplyr::mutate(qsales = sum(rtotal_sales)) %>%
      ggplot(
        aes(quarter2,
            log(qsales)
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "24 months", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Log Dollar Sales - Constant 2021 Dollars") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 color = color_pal$sunset, size = 1, linetype = "dashed") ->g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_real_sales_7467.png"),
      g
    )
    
  #-Cigarette Sales Yearly REAL---------------------------------------------------
    raw_weekly %>%
      filter(product_id == 7460) %>%
      group_by(year) %>%
      dplyr::mutate(ysales = sum(rtotal_sales)) %>%
      ggplot(
        aes(year2,
            log(ysales)
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "3 years", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Year") + ylab("Log Dollar Sales - Constant 2021 Dollars") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 color = color_pal$sunset, size = 1, linetype = "dashed") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/yearly_real_sales_7460.png"),
      g
    )
    
  #E-Cigarette Sales Yearly-------------------------------------------------------
    raw_weekly %>%
      filter(product_id == 7467) %>%
      group_by(year) %>%
      dplyr::mutate(ysales = sum(rtotal_sales)) %>%
      ggplot(
        aes(year2,
            log(ysales)
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "3 years", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Year") + ylab("Log Dollar Sales - Constant 2021 Dollars") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 color = color_pal$sunset, size = 1, linetype = "dashed") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/yearly_real_sales_7467.png"),
      g
    )
    
    
    
#-Volume------------------------------------------------------------------------
    
  #-Cigarettes Volume by Year---------------------------------------------------
  raw_weekly %>%
    filter(product_id == 7460) %>%
    ggplot(
      aes(week,
          log(total_volume)
      )) +
    geom_line() +
    facet_wrap(vars(year), scales = "free") +
    scale_x_date(date_breaks = "4 months", date_labels = "%b") +
    theme_economist_white(gray_bg = FALSE) +
    xlab("Week") + ylab("Log Volume") -> g
  
  ggsave(
    paste0(direcs$pap.fig,
           "nielsen/raw_aggregate/weekly_volume_by_year_7460.png"),
    g
  )
    
  #-E-Cigarettes Volume by Year------------------------------------------------
  raw_weekly %>%
    filter(product_id == 7467) %>%
    ggplot(
      aes(week,
          log(total_volume)
      )) +
    geom_line() +
    facet_wrap(vars(year), scales = "free") +
    scale_x_date(date_breaks = "4 months", date_labels = "%b") +
    theme_economist_white(gray_bg = FALSE) +
    xlab("Week") + ylab("Log Volume") -> g
  
  ggsave(
    paste0(direcs$pap.fig,
           "nielsen/raw_aggregate/weekly_volume_by_year_7467.png"),
    g
  )
  
  #-Cigarette Volume by Month------------------------------------------------------
  raw_weekly %>%
    filter(product_id == 7460) %>%
    group_by(year, month) %>%
    dplyr::mutate(mvol = sum(total_volume)) %>%
    ggplot(
      aes(month2,
          log(mvol)
      )) +
    geom_line() +
    geom_point() +
    facet_wrap(vars(year), scales = "free") +
    scale_x_date(date_breaks = "4 months", date_labels = "%b") +
    theme_economist_white(gray_bg = FALSE) +
    xlab("Month") + ylab("Log Volume") -> g
  
  ggsave(
    paste0(direcs$pap.fig,
           "nielsen/raw_aggregate/monthly_volume_by_year_7460.png"),
    g
  )
  
  #-E-Cigarette Volume by Month----------------------------------------------------
  raw_weekly %>%
    filter(product_id == 7467) %>%
    group_by(year, month) %>%
    dplyr::mutate(mvol = sum(total_volume)) %>%
    ggplot(
      aes(month2,
          log(mvol)
      )) +
    geom_line() +
    geom_point() +
    facet_wrap(vars(year), scales = "free") +
    scale_x_date(date_breaks = "4 months", date_labels = "%b") +
    theme_economist_white(gray_bg = FALSE) +
    xlab("Month") + ylab("Log Volume") -> g
  
  ggsave(
    paste0(direcs$pap.fig,
           "nielsen/raw_aggregate/monthly_volume_by_year_7467.png"),
    g
  )
  
  #-Cigarette Volume by Quarter----------------------------------------------------  
  raw_weekly %>%
    filter(product_id == 7460) %>%
    group_by(year, quarter.x) %>%
    dplyr::mutate(qvol = sum(total_volume)) %>%
    ggplot(
      aes(quarter.x,
          log(qvol)
      )) +
    geom_line() +
    geom_point() +
    facet_wrap(vars(year), scales = "free") +
    theme_economist_white(gray_bg = FALSE) +
    xlab("Quarter") + ylab("Log Volume") -> g
  
  ggsave(
    paste0(direcs$pap.fig,
           "nielsen/raw_aggregate/quarterly_volume_by_year_7460.png"),
    g
  )
  
  #-E-Cigarette Volume by Quarter--------------------------------------------------  
  raw_weekly %>%
    filter(product_id == 7467) %>%
    group_by(year, quarter.x) %>%
    dplyr::mutate(qvol = sum(total_volume)) %>%
    ggplot(
      aes(quarter.x,
          log(qvol)
      )) +
    geom_line() +
    geom_point() +
    facet_wrap(vars(year), scales = "free") +
    theme_economist_white(gray_bg = FALSE) +
    xlab("Quarter") + ylab("Log Volume") -> g
  
  ggsave(
    paste0(direcs$pap.fig,
           "nielsen/raw_aggregate/quarterly_volume_by_year_7467.png"),
    g
  )
  
  #-Cigarette Volume by Quarter (Entire Time Period)-------------------------------
  raw_weekly %>%
    filter(product_id == 7460) %>%
    group_by(year, quarter.x) %>%
    dplyr::mutate(qvol = sum(total_volume)) %>%
    ggplot(
      aes(quarter2,
          log(qvol)
      )) +
    geom_line(size = 1.5) +
    geom_point(size = 2.5) +
    scale_x_date(breaks = "36 months", date_labels = "%Y") +
    theme_economist_white(gray_bg = FALSE) +
    xlab("Quarter") + ylab("Log Volume") +
    geom_vline(xintercept = as.Date("2018-01-01"),
               color = color_pal$sunset, size = 1, linetype = "dashed") ->g
  
  ggsave(
    paste0(direcs$pap.fig,
           "nielsen/raw_aggregate/quarterly_volume_7460.png"),
    g
  )
  
  #E-Cigarette Volume by Quarter (Entire Time Period)------------------------------
    raw_weekly %>%
      filter(product_id == 7467) %>%
      group_by(year, quarter.x) %>%
      dplyr::mutate(qvol = sum(total_volume)) %>%
      ggplot(
        aes(quarter2,
            log(qvol)
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "24 months", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Log Volume") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 color = color_pal$sunset, size = 1, linetype = "dashed") ->g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_volume_7467.png"),
      g
    )
    
  #-Cigarette Volume Yearly----------------------------------------------------
    raw_weekly %>%
      filter(product_id == 7460) %>%
      group_by(year) %>%
      dplyr::mutate(yvol = sum(total_volume)) %>%
      ggplot(
        aes(year2,
            log(yvol)
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "3 years", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Year") + ylab("Log Volume") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 color = color_pal$sunset, size = 1, linetype = "dashed") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/yearly_volume_7460.png"),
      g
    )
    
  #E-Cigarette Sales Yearly-------------------------------------------------------
    raw_weekly %>%
      filter(product_id == 7467) %>%
      group_by(year) %>%
      dplyr::mutate(yvol = sum(total_volume)) %>%
      ggplot(
        aes(year2,
            log(yvol)
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "3 years", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Year") + ylab("Log Volume") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 color = color_pal$sunset, size = 1, linetype = "dashed") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/yearly_volume_7467.png"),
      g
    )

    
    
    
#-Prices------------------------------------------------------------------------    

#-Prices by Year--------------------------------------------------
    raw_weekly %>%
      ggplot(
        aes(week,
            avg_price, , color = product_id
        )) +
      geom_line() +
      facet_wrap(vars(year), scales = "free") +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Week") + ylab("Average Price") +
      scale_color_manual(values = c(color_pal$wildcat_blue,
                                    color_pal$sunset),
                         labels = c("Cigarette Products",
                                   "Electronic Cigarette Products"),
                         name = ""
                         ) +
      theme(legend.position = "bottom")-> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/weekly_avg_prices_by_year.png"),
      g
    )
    
#-Prices by Month------------------------------------------------------
    raw_weekly %>%
      group_by(year, month, product_id) %>%
      dplyr::mutate(price = mean(avg_price)) %>%
      ggplot(
        aes(month2,
            price, , color = product_id
        )) +
      geom_line() +
      facet_wrap(vars(year), scales = "free") +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Month") + ylab("Average Price") +
      scale_color_manual(values = c(color_pal$wildcat_blue,
                                    color_pal$sunset),
                         labels = c("Cigarette Products",
                                    "Electronic Cigarette Products"),
                         name = ""
      ) +
      theme(legend.position = "bottom")-> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/monthly_avg_prices_by_year.png"),
      g
    )
    

    
#-Prices by Quarter----------------------------------------------------  
    raw_weekly %>%
      group_by(year, quarter.x, product_id) %>%
      dplyr::mutate(price = mean(avg_price)) %>%
      ggplot(
        aes(quarter.x,
            price, , color = product_id
        )) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(year), scales = "free") +
      #scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Average Price") +
      scale_color_manual(values = c(color_pal$wildcat_blue,
                                    color_pal$sunset),
                         labels = c("Cigarette Products",
                                    "Electronic Cigarette Products"),
                         name = ""
      ) +
      theme(legend.position = "bottom")-> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_avg_prices_by_year.png"),
      g
    )
    
    
#-Prices by Quarter (Entire Time Period)----------------------------------------
    raw_weekly %>%
      group_by(year, quarter.x, product_id) %>%
      dplyr::mutate(price = mean(avg_price)) %>%
      ggplot(
        aes(quarter2,
            price, color = product_id
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "36 months", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Average Price") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 size = 1, linetype = "dashed") +
      scale_color_manual(values = c(color_pal$wildcat_blue,
                                    color_pal$sunset),
                         labels = c("Cigarette Products",
                                    "Electronic Cigarette Products"),
                         name = ""
      ) +
      theme(legend.position = "bottom") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_avg_prices.png"),
      g
    )
    
#-Prices Yearly--------------------------------------------------------
    raw_weekly %>%
      group_by(year, product_id) %>%
      dplyr::mutate(price = mean(avg_price)) %>%
      ggplot(
        aes(year2,
            price, color = product_id
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "36 months", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Average Price") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 size = 1, linetype = "dashed") +
      scale_color_manual(values = c(color_pal$wildcat_blue,
                                    color_pal$sunset),
                         labels = c("Cigarette Products",
                                    "Electronic Cigarette Products"),
                         name = ""
      ) +
      theme(legend.position = "bottom") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/yearly_avg_prices.png"),
      g
    )
    
    
#-Prices REAL-------------------------------------------------------------------

#-Prices by Year REAL-----------------------------------------------------------
    raw_weekly %>%
      ggplot(
        aes(week,
            toReal(avg_price, cpi/2.7) , color = product_id
        )) +
      geom_line() +
      facet_wrap(vars(year), scales = "free") +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Week") + ylab("Average Price - Constant 2021 Dollars") +
      scale_color_manual(values = c(color_pal$wildcat_blue,
                                    color_pal$sunset),
                         labels = c("Cigarette Products",
                                    "Electronic Cigarette Products"),
                         name = ""
      ) +
      theme(legend.position = "bottom")-> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/weekly_real_avg_prices_by_year.png"),
      g
    )
    
#-Prices by Month---------------------------------------------------------------
    raw_weekly %>%
      group_by(year, month, product_id) %>%
      dplyr::mutate(price = mean(toReal(avg_price, cpi/2.7))) %>%
      ggplot(
        aes(month2,
            price , color = product_id
        )) +
      geom_line() +
      facet_wrap(vars(year), scales = "free") +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Month") + ylab("Average Price - Constant 2021 Dollars") +
      scale_color_manual(values = c(color_pal$wildcat_blue,
                                    color_pal$sunset),
                         labels = c("Cigarette Products",
                                    "Electronic Cigarette Products"),
                         name = ""
      ) +
      theme(legend.position = "bottom")-> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/monthly_real_avg_prices_by_year.png"),
      g
    )
    
    
    
#-Prices by Quarter----------------------------------------------------  
    raw_weekly %>%
      group_by(year, quarter.x, product_id) %>%
      dplyr::mutate(price = mean(toReal(avg_price, cpi/2.7))) %>%
      ggplot(
        aes(quarter.x,
            price , color = product_id
        )) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(year), scales = "free") +
      #scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Average Price - Constant 2021 Dollars") +
      scale_color_manual(values = c(color_pal$wildcat_blue,
                                    color_pal$sunset),
                         labels = c("Cigarette Products",
                                    "Electronic Cigarette Products"),
                         name = ""
      ) +
      theme(legend.position = "bottom")-> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_real_avg_prices_by_year.png"),
      g
    )
    
    
#-Prices by Quarter (Entire Time Period) REAL-----------------------------------
    raw_weekly %>%
      group_by(year, quarter.x, product_id) %>%
      dplyr::mutate(price = mean(toReal(avg_price, cpi/2.7))) %>%
      ggplot(
        aes(quarter2,
            price, color = product_id
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "36 months", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Quarter") + ylab("Average Price - Constant 2021 Dollars") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 size = 1, linetype = "dashed") +
      scale_color_manual(values = c(color_pal$wildcat_blue,
                                    color_pal$sunset),
                         labels = c("Cigarette Products",
                                    "Electronic Cigarette Products"),
                         name = ""
      ) +
      theme(legend.position = "bottom") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/quarterly_real_avg_prices.png"),
      g
    )
    
#-Prices Yearly--------------------------------------------------------
    raw_weekly %>%
      group_by(year, product_id) %>%
      dplyr::mutate(price = mean(toReal(avg_price, cpi/2.7))) %>%
      ggplot(
        aes(year2,
            price, color = product_id
        )) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      scale_x_date(breaks = "36 months", date_labels = "%Y") +
      theme_economist_white(gray_bg = FALSE) +
      xlab("Year") + ylab("Average Price - Constant 2021 Dollars") +
      geom_vline(xintercept = as.Date("2018-01-01"),
                 size = 1, linetype = "dashed") +
      scale_color_manual(values = c(color_pal$wildcat_blue,
                                    color_pal$sunset),
                         labels = c("Cigarette Products",
                                    "Electronic Cigarette Products"),
                         name = ""
      ) +
      theme(legend.position = "bottom") -> g
    
    ggsave(
      paste0(direcs$pap.fig,
             "nielsen/raw_aggregate/yearly_real_avg_prices.png"),
      g
    )

#-Ratios------------------------------------------------------------------------
    
raw_week_grouped <- raw_weekly %>%
      group_by(product_id) %>%
      group_split() 
    
prod7460 <- raw_week_grouped[[1]]
prod7467 <- raw_week_grouped[[2]]
prods <- inner_join(prod7460, prod7467, by = "week") %>%
  dplyr::mutate(sales_ratio = total_sales.y/total_sales.x,
                volume_ratio = total_volume.y/total_volume.x,
                price_ratio = avg_price.y/avg_price.x) %>%
  dplyr::select(week, sales_ratio, volume_ratio, price_ratio, quarter2.x,
                year2.x)

#-Sales Ratio By Quarter Entire Time Period
prods %>%
  group_by(year2.x, quarter2.x) %>%
  dplyr::mutate(sales = mean(sales_ratio)) %>%
  ggplot(
    aes(
      quarter2.x,
      sales
    )
  ) +
  geom_line(size = 1.5) +
  geom_point(size = 2.5) +
  scale_x_date(breaks = "24 months", date_labels = "%Y") +
  theme_economist_white(gray_bg = FALSE) +
  xlab("Quarter") + 
  ylab("Electronic Cigarette Product Sales Over Cigarette Product Sales") +
  geom_vline(xintercept = as.Date("2018-01-01"),
             size = 1, color = color_pal$sunset, linetype = "dashed") -> g
  
  ggsave(
    paste0(direcs$pap.fig,
           "nielsen/raw_aggregate/quarterly_sales_ratio.png"),
    g
  )
  
#-Quarterly Volume Ratio--------------------------------------------------------
  
  prods %>%
    group_by(year2.x, quarter2.x) %>%
    dplyr::mutate(sales = mean(volume_ratio)) %>%
    ggplot(
      aes(
        quarter2.x,
        sales
      )
    ) +
    geom_line(size = 1.5) +
    geom_point(size = 2.5) +
    scale_x_date(breaks = "24 months", date_labels = "%Y") +
    theme_economist_white(gray_bg = FALSE) +
    xlab("Quarter") + 
    ylab("Electronic Cigarette Product Volume Over Cigarette Product Volume") +
    geom_vline(xintercept = as.Date("2018-01-01"),
               size = 1, color = color_pal$sunset, linetype = "dashed") -> g
  
  ggsave(
    paste0(direcs$pap.fig,
           "nielsen/raw_aggregate/quarterly_volume_ratio.png"),
    g
  )
  
#-Quarterly Price Ratio---------------------------------------------------------
  
  prods %>%
    group_by(year2.x, quarter2.x) %>%
    dplyr::mutate(sales = mean(price_ratio)) %>%
    ggplot(
      aes(
        quarter2.x,
        sales
      )
    ) +
    geom_line(size = 1.5) +
    geom_point(size = 2.5) +
    scale_x_date(breaks = "24 months", date_labels = "%Y") +
    theme_economist_white(gray_bg = FALSE) +
    xlab("Quarter") + 
    ylab("Electronic Cigarette Product Average Price Over Cigarette Product Average Price") +
    geom_vline(xintercept = as.Date("2018-01-01"),
               size = 1, color = color_pal$sunset, linetype = "dashed") -> g
  
  ggsave(
    paste0(direcs$pap.fig,
           "nielsen/raw_aggregate/quarterly_price_ratio.png"),
    g
  )

  