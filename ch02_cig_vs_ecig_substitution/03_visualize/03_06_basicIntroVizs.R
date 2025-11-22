ecig_occurences = read.csv(
  paste0(direcs$dat.ext, "/google_scholar/ecig_occurences.csv")
) %>%
  mutate(productID = 74670) %>%
  filter(year >= 2013 & year <= 2020) %>%
  dplyr::rename(value = results) 

cig_occurences = read.csv(
  paste0(direcs$dat.ext, "/google_scholar/cig_occurences.csv")
) %>%
  mutate(productID = 74600) %>%
  filter(year >= 2013 & year <= 2020) %>%
  dplyr::rename(value = results)

occurences <- rbind(cig_occurences, ecig_occurences)

ecig_sales = {
  
  list.files(paste0(direcs$dat.der, "nielsen/blp/sales/total_all_sales"), full.names = TRUE)[list.files(paste0(direcs$dat.der, "nielsen/blp/sales/total_all_sales")) %>%
    grep("_7467_", .)] %>%
    lapply(
      function(df){
        
        read_tsv(df)
        
      }
    ) %>%
    Reduce(rbind, .) %>%
    dplyr::rename(value = annual_all_sales)
  
}

cig_sales = {
  
  list.files(paste0(direcs$dat.der, "nielsen/blp/sales/total_all_sales"), full.names = TRUE)[list.files(paste0(direcs$dat.der, "nielsen/blp/sales/total_all_sales")) %>%
                                                                                               grep("_7460_", .)] %>%
    lapply(
      function(df){
        
        read_tsv(df)
        
      }
    ) %>%
    Reduce(rbind, .) %>%
    dplyr::rename(value = annual_all_sales)
  
}

dframe <- rbind(occurences, ecig_sales, cig_sales)


occurences_fig <- {
  
  dframe %>%
    filter(productID == 74670 | productID == 74600) %>%
    filter(year >= 2012) %>%
    group_by(productID) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(pct_change = value/lag(value) - 1) %>%
    mutate(productID = as.factor(productID)) %>%
    ggplot(aes(x = year, y = pct_change, fill = productID)) +
    geom_bar(stat = "identity", width = 0.6, position = "dodge") +
    theme_bw() +
    scale_fill_manual("", values = c("74600" = color_pal$sunset, "74670" = color_pal$river_green),
                      labels = c("Cigarettes", "E-Cigarettes")) +
    xlab("Year") +
    ylab("Percent Change from Previous Year") +
    ggtitle("Change in Occurrences of 'E-Cigarette' and 'Cigarette' On Google Scholar") +
    theme(plot.title = element_text(size = 10)) +
    scale_y_continuous(labels = scales::percent) +
    geom_hline(aes(yintercept = 0), size = 1)
}

sales_fig <- {
  
  dframe %>%
    filter(productID == 7467 | productID == 7460) %>%
    filter(year >= 2013) %>%
    group_by(productID) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(pct_change = value/lag(value) - 1) %>%
    mutate(productID = as.factor(productID)) %>%
    ggplot(aes(x = year, y = pct_change, fill = productID)) +
    geom_bar(stat = "identity", width = 0.6, position = "dodge") +
    theme_bw() +
    scale_fill_manual("", values = c("7460" = color_pal$sunset, "7467" = color_pal$river_green),
                      labels = c("Cigarettes", "E-Cigarettes")) +
    xlab("Year") +
    ylab("Percent Change from Previous Year") +
    ggtitle("Change in Dollar Sales of 'E-Cigarette' and 'Cigarette' On Google Scholar") +
    theme(plot.title = element_text(size = 10)) +
    scale_y_continuous(labels = scales::percent) +
    geom_hline(aes(yintercept = 0), size = 1)
}

fig <- grid.arrange(occurences_fig, sales_fig)
ggsave(paste0(direcs$pap.fig, "final_figures/sales_and_occurences.png"), fig)

sales <- dframe %>% filter(productID == 7467 | productID == 7460)
ibis_e <- (cbind(read.csv(paste0(direcs$dat.ext, "ibisworld/ecig_manuf_data.csv"))[,1:2], productID =  7467) 
           %>% dplyr::rename(year = Year)) 
ibis_c <-  (cbind(read.csv(paste0(direcs$dat.ext, "ibisworld/tobacco_manuf_data.csv"))[,1:2], productID =  7460) 
            %>% dplyr::rename(year = Year))
ibis <- rbind(ibis_e, ibis_c) %>%
  dplyr::rename(total = Revenue....million.)

sales <- merge(sales, ibis, by = c("year", "productID"))
sales <- sales %>%
  mutate(value = value/1e6) %>%
  mutate(rate = value/total)
              


rate_fig <- {
  
  sales %>%
    filter(productID == 7467 | productID == 7460) %>%
    filter(year >= 2013) %>%
    mutate(productID = as.factor(productID)) %>%
    ggplot(aes(x = year, y = rate, color = productID)) +
    geom_point(stat = "identity", size = 4) +
    theme_bw() +
    scale_color_manual("", values = c("7460" = color_pal$sunset, "7467" = color_pal$river_green),
                      labels = c("Cigarettes", "E-Cigarettes")) +
    geom_hline(yintercept = {
      (sales %>% 
         filter(productID == 7467) %>% 
         summarize(m = mean(rate)))$m
    }, color = color_pal$river_green, linetype = 2, linewidth = 2, alpha = 0.5) +
    geom_hline(yintercept = {
      (sales %>% 
         filter(productID == 7460 & year >= 2013) %>% 
         summarize(m = mean(rate)))$m
    }, color = color_pal$sunset, linetype = 4, linewidth = 2, alpha = 0.5) +
    xlab("Year") +
    ylab("Coverage Rate") +
    ggtitle("") +
    theme(plot.title = element_text(size = 10)) +
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.12, 0.03))
}

ggsave(paste0(direcs$pap.fig, "final_figures/cov_rate.png"), rate_fig)


