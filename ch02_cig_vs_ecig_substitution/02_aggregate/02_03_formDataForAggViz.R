direcs$dat.der %>%
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
                lravg_sum_price = log(ravg_sum_price)) %>%
  mutate(week = as.character(week)) %>%
  dbWriteTable(results, "for_agg_viz", ., overwrite = T)
