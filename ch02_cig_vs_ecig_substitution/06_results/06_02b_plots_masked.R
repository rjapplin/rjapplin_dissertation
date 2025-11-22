#Sales and Occurrences-----------------------------------------------------------
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
                      labels = c("Traditional", "Electronic")) +
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
                      labels = c("Traditional", "Electronic")) +
    xlab("Year") +
    ylab("Percent Change from Previous Year") +
    ggtitle("Change in Dollar Sales of 'E-Cigarette' and 'Cigarette' On Google Scholar") +
    theme(plot.title = element_text(size = 10)) +
    scale_y_continuous(labels = scales::percent) +
    geom_hline(aes(yintercept = 0), size = 1)
}

fig <- grid.arrange(occurences_fig, sales_fig)
ggsave("paper/figures/sales_and_occurrences.png", plot = fig, width = 22.2,
       height = 13,
       units = "in")


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
                       labels = c("Traditional", "Electronic")) +
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

ggsave("paper/figures/coverage_rate.png", rate_fig,
       width = 22.2, height = 13, units = "in")

#All volume---------------------------------------------------------------------
dbReadTable(results2, "for_agg_viz") %>%
  group_by(year, product_id) %>%
  dplyr::summarize(
    tv = sum(total_volume)
  ) %>%
  mutate(tv = ifelse(product_id == 7460, tv/20, tv)) %>%
  mutate(category = ifelse(
    product_id == 7460, "Traditional", "Electronic"
  )) %>%
  filter(year >= 2013) %>%
  ggplot(aes(x = year, y = log(tv), fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Year") + ylab("Log(Total Volume)") +
  scale_fill_manual(values = c(color_pal$river_green, color_pal$sunset)) -> fig
  
ggsave("paper/figures/all_volume.png",
       width = 22.2, height = 13, units = "in",
       plot = fig)

#Avg Price entire NRSD----------------------------------------------------------
dbReadTable(results2, "for_agg_viz") %>%
  group_by(year, product_id) %>%
  dplyr::summarize(
    p = mean(avg_sum_price)
  ) %>%
  mutate(p = ifelse(product_id == 7460, p*20, p)) %>%
  mutate(category = ifelse(
    product_id == 7460, "Traditional", "Electronic"
  )) %>%
  filter(year >= 2013) %>%
  ggplot(aes(x = year, y = p, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Year") + ylab("Average Price") +
  scale_fill_manual(values = c(color_pal$river_green, color_pal$sunset)) -> fig

ggsave("paper/figures/all_avg_price.png",
       width = 22.2, height = 13, units = "in",
       plot = fig)

#Prices Over Time by Category---------------------------------------------------
df2 %>%
  group_by(category, year) %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) %>%
  dplyr::summarize(price = mean(price)) %>%
  ggplot(aes(x = as.factor(year), y = price, group = category,
             color = category)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = c(color_pal$river_green, color_pal$sunset)) +
  geom_line() +
  geom_point() +
  theme(
    #plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank(),
    axis.text.x = element_text(angle= 90, vjust = 0, hjust = 0),
    legend.position = "bottom",
    legend.key.size = unit(1.5, "cm"),
    legend.text = element_text(size = 10)
  ) +
  xlab("Year") +
  ylab("Average Price") -> fig

ggsave("paper/figures/avg_price_by_category.png",
       width = 22.2, height = 13, units = "in",
       plot = fig)

#Inside Volume------------------------------------------------------------------
dbReadTable(main, "movement") %>%
  select(marketid, brand_code_uc, brand_mkt_vol, brand_mkt_sales) %>%
  inner_join(dbReadTable(main, "brands") %>% 
               select(brand_code_uc, brand_descr, category),
             by = "brand_code_uc") %>%
  dplyr::rename(
    vol = brand_mkt_vol,
    sales = brand_mkt_sales,
    brand = brand_descr
  ) %>%
  group_by(category, brand) %>%
  dplyr::summarize(
    vol = mean(vol),
    sales = mean(sales)
  ) %>%
  inner_join(dbReadTable(results2, "brand_map_and_mask"), by = "brand") %>%
  mutate(broader_prefix = ifelse(broader_prefix < 10, 
                                 paste0("0", broader_prefix),
                                 broader_prefix)
  ) %>%
  mutate(brand = paste0(broader_prefix, ". ", MASKED_NAME)) %>%
  group_by(category, brand) %>%
  dplyr::summarize(
    vol = sum(vol),
    sales = sum(sales)
  ) %>%
  mutate(vol = ifelse(category == 7467, vol/20, vol)) %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) %>%
  ggplot(aes(x = brand, y = vol, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~category, scales = "free_x") + 
  theme_bw() +
  ylab("Aveage Market Inside Volume") +
  xlab("Brand") +
  scale_fill_manual(values = c(color_pal$river_green, color_pal$sunset))-> fig

ggsave("paper/figures/avg_brand_mkt_volume.png",
       width = 22.2, height = 13, units = "in", plot = fig)

#Inside Volume (Remove Marlboro and Juul)---------------------------------------
dbReadTable(main, "movement") %>%
  select(marketid, brand_code_uc, brand_mkt_vol, brand_mkt_sales) %>%
  inner_join(dbReadTable(main, "brands") %>% 
               select(brand_code_uc, brand_descr, category),
             by = "brand_code_uc") %>%
  dplyr::rename(
    vol = brand_mkt_vol,
    sales = brand_mkt_sales,
    brand = brand_descr
  ) %>%
  group_by(category, brand) %>%
  dplyr::summarize(
    vol = mean(vol),
    sales = mean(sales)
  ) %>%
  inner_join(dbReadTable(results2, "brand_map_and_mask"), by = "brand") %>%
  mutate(broader_prefix = ifelse(broader_prefix < 10, 
                                 paste0("0", broader_prefix),
                                 broader_prefix)
  ) %>%
  mutate(brand = paste0(broader_prefix, ". ", MASKED_NAME)) %>%
  group_by(category, brand) %>%
  dplyr::summarize(
    vol = sum(vol),
    sales = sum(sales)
  ) %>%
    mutate(vol = ifelse(category == 7467, vol/20, vol)) %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) %>%
  filter(brand != "MASKED") %>%
  ggplot(aes(x = brand, y = vol, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~category, scales = "free_x") + 
  theme_bw() +
  ylab("Aveage Market Inside Volume") +
  xlab("Brand") +
  scale_fill_manual(values = c(color_pal$river_green, color_pal$sunset))-> fig

ggsave("paper/figures/avg_brand_mkt_volume_ro.png",
       width = 22.2, height = 13, units = "in", plot = fig)

  
#Inside Volume Over Time--------------------------------------------------------
dbReadTable(main, "movement") %>%
  select(marketid, brand_code_uc, brand_mkt_vol, brand_mkt_sales) %>%
  inner_join(dbReadTable(main, "brands") %>% 
               select(brand_code_uc, brand_descr, category),
             by = "brand_code_uc") %>%
  dplyr::rename(
    vol = brand_mkt_vol,
    sales = brand_mkt_sales,
    brand = brand_descr
  ) %>%
  inner_join(df2 %>% 
               select(cdid, brand, market, year, quarter) %>%
               distinct() %>%
               dplyr::rename(marketid = market),
             by = c("marketid", "brand")
  ) %>%
  mutate(Category = case_when(
    category == 7467 & brand != "MASKED",
    category == 7467 & brand == "MASKED",
    category == 7460 & brand != "MASKED",
    category == 7460 & brand == "MASKED"
  )) %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) %>%
  group_by(year, quarter, Category, category) %>%
  dplyr::summarize(
    vol = sum(vol)
  ) %>%
  mutate(vol = ifelse(category == "Traditonal", vol/20, vol)) %>%
  mutate(Date = paste0(year, "-Q", quarter)) %>%
  group_by(Date, category) %>%
  dplyr::summarize(vol = sum(vol)) %>%
  ggplot(aes(x = Date, y = vol, fill = category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~category, scales = "free_y", nrow = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c(color_pal$river_green, color_pal$sunset)) +
  xlab("Date") +
  ylab("Total Market Inside Volume") -> fig
  
ggsave("paper/figures/category_vol_by_year.png",
       width = 22.2, height = 13, units = "in", plot = fig)




#InShare By brand---------------------------------------------------------------
df2 %>%
  select(cdid, brand, category, inShare) %>%
  group_by(category, brand) %>%
  dplyr::summarize(
    share = mean(inShare)
  ) %>%
  inner_join(dbReadTable(results2, "brand_map_and_mask"), by = "brand") %>%
  mutate(broader_prefix = ifelse(broader_prefix < 10, 
                                 paste0("0", broader_prefix),
                                 broader_prefix)
  ) %>%
  mutate(brand = paste0(broader_prefix, ". ", MASKED_NAME)) %>%
  group_by(category, brand) %>%
  dplyr::summarize(
    share = sum(share)
  ) %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) -> temp

data.frame(category = "Outside", 
           brand = "00. Outside",
           share = 1 - sum(temp$share)) %>%
  rbind(temp) %>%
  ggplot(aes(x = brand, y = share, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~category, scales = "free_x") + 
  theme_bw() +
  ylab("Aveage Market Inside Market Share") +
  xlab("Brand") +
  scale_fill_manual(values = c(color_pal$river_green, 
                               color_pal$warm_neutral,
                               color_pal$sunset))-> fig
ggsave("paper/figures/avg_inside_share.png",
       width = 22.2, height = 13, units = "in", plot = fig)

#InShare By Category Over Time--------------------------------------------------
df2 %>%
  group_by(year,cdid, category) %>%
  dplyr::summarize(
    inShare = sum(inShare)
  ) %>%
  group_by(year, category) %>%
  dplyr::summarize(
    inShare = mean(inShare)
  ) %>%
  group_by(year) %>%
  dplyr::mutate(
    category = ifelse(category == 7467, "Traditional", "Electronic")
  ) %>%
  group_by(year) %>%
  dplyr::mutate(out_share = 1 - sum(inShare)) %>%
  rbind(.,
    data.frame(year = .$year, category = "Outside", inShare = .$out_share)
  ) %>%
  distinct() %>%
  select(-out_share) %>%
  arrange(year, category, inShare) %>%
  ggplot(aes(x = year, y = inShare, color = category)) +
  geom_line(group = 1) +
  geom_point(group = 1) +
  facet_wrap(~category) +
  xlab("Year") +
  ylab("Log(Inside Share)") +
  theme_bw() +
  scale_color_manual(values = c(color_pal$river_green,
                                color_pal$warm_neutral,
                                color_pal$sunset)) +
  theme(
    #plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank(),
    axis.text.x = element_text(angle= 90, vjust = 0, hjust = 0),
    legend.position = "bottom",
    legend.key.size = unit(1.5, "cm"),
    legend.text = element_text(size = 10)
  ) -> fig

ggsave("paper/figures/category_share_by_year.png",
       width = 22.2, height = 13, units = "in", plot = fig)

#InShare By Brand Over Time ----------------------------------------------------
temp <- df2 %>%
  group_by(year, brand) %>%
  dplyr::summarize(
    inShare_bar = mean(inShare)
  ) %>%
  left_join(
    dbReadTable(results, "brand_map_and_mask"),
    by = "brand"
  ) %>%
  group_by(year, broader_prefix, MASKED_NAME) %>%
  dplyr::summarize(inShare_bar = sum(inShare_bar)) %>%
  group_by(year) %>%
  dplyr::mutate(out = 1 - sum(inShare_bar))

temp <- 
  rbind(temp,
        temp %>% 
          select(year, out) %>%
          dplyr::rename(inShare_bar = out) %>%
          dplyr::mutate(
            MASKED_NAME = "OUTSIDE",
            broader_brand_id = 0,
            out = NA) %>%
          distinct()
  ) %>%
  select(-out)

temp %>%
  mutate(prefix = ifelse(broader_prefix < 10,
                         paste0("0", broader_prefix),
                         broader_prefix)) %>%
  mutate(prefix = ifelse(MASKED_NAME == "OUTSIDE", "00", prefix)) %>%
  mutate(MASKED_NAME = paste0(prefix, ". ", MASKED_NAME)) %>%
  ggplot(aes(x = as.factor(year), y = inShare_bar, group = 1)) +
  geom_line(group = 1) +
  geom_point() +
  facet_wrap(~MASKED_NAME, scales = "free_y") +
  theme_bw() +
  xlab("Year") +
  ylab("Inside Share")

ggsave("paper/figures/brand_share_by_year.png", width = 22.2, height = 13,
       units = "in")

#Car Data Benchmark-------------------------------------------------------------
dbGetQuery(results, 
           "SELECT firmid, mpe, rmspe, CAST(iter AS CHAR) iter
           FROM cars_oos_perf") %>% 
  mutate(
    type = case_when(
      iter == "avg" ~ "Subbagged",
      iter == "full_train" ~ "Full Training",
      .default = "Individual"
    )
  ) %>%
  pivot_longer(cols = c(mpe, rmspe)) %>%
  mutate(
    name = ifelse(name == "mpe", "Percent Error",
                  "Absolute Percent Error")
  ) %>%
  filter(name == "Absolute Percent Error") %>%
  dplyr::rename(`Error Type` = name) %>%
  ggplot(aes(x = `Error Type`, y = log(value))) +
  geom_boxplot() +
  facet_wrap(~type, ncol = 3) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank()) +
  xlab("") +
  ylab("Log Absolute Percent Error (%) in Test Market Shares") +
  geom_hline(yintercept = log(100), color = "red") -> fig

ggsave("paper/figures/cars_test_benchmark.png",
       plot = fig, width = 22.2, height = 13, units = "in")


#In Sample Performance----------------------------------------------------------
df2 %>%
  filter(!(is.na(delta))) %>%
  select(cdid, category, brand, inShare, inShare_hat_approx) %>%
  mutate(
    pe = (inShare_hat_approx - inShare)/inShare
  ) %>%
  mutate(rspe = sqrt(pe^2)) %>%
  group_by(category, brand) %>%
  dplyr::summarize(
    mpe = mean(pe)*100,
    rmspe = mean(rspe)*100
  ) %>%
  pivot_longer(
    cols = c(mpe, rmspe)
  ) %>%
  inner_join(dbReadTable(results, "brand_map_and_mask") %>%
               select(brand, broader_prefix, MASKED_NAME),
             by = "brand") %>%
  group_by(category, name, broader_prefix, MASKED_NAME) %>%
  dplyr::summarize(value = mean(value)) %>%
  mutate(broader_prefix = ifelse(broader_prefix < 10, paste0("0", broader_prefix),
                                 broader_prefix)) %>%
  mutate(brand = paste0(broader_prefix, ". ", MASKED_NAME)) %>%
  mutate(
    `Error Type` = ifelse(name == "mpe", "Mean Percent Error",
                  "Absolute Percent Error")
  ) %>%
  filter(`Error Type` == "Absolute Percent Error") %>%
  group_by(category, name) %>%
  dplyr::summarize(value = mean(value)) %>%
  mutate(
    category = ifelse(category == 7467, "Traditional", "Electronic")
  ) %>%
  ggplot(aes(x = as.factor(category), y = value, group = name, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ylab("In-Sample Error (%)") +
  xlab("Brand") +
  geom_vline(xintercept = 16.5) +
  scale_fill_manual(values = c(color_pal$river_green,
                               color_pal$sunset)) +
  theme_bw() +
  theme(legend.position = "bottom") -> fig

ggsave("paper/figures/in_sample_performance.png",
       plot = fig, width = 22.2, height = 13, units = "in")


#Out of Sample Performance------------------------------------------------------
dbReadTable(results, "data.oos_avg_delta") %>%
  mutate(
    pe = (inShare_hat_approx - inShare)/inShare
  ) %>%
  mutate(rspe = sqrt(pe^2)) %>%
  group_by(brand, iter) %>%
  dplyr::summarize(
    mpe = mean(pe)*100,
    rmspe = mean(rspe)*100
  ) %>%
  pivot_longer(
    cols = c(mpe, rmspe)
  ) %>%
  inner_join(dbReadTable(results, "brand_map_and_mask") %>%
               select(brand, broader_prefix, MASKED_NAME),
             by = "brand") %>%
  group_by(iter, name, broader_prefix, MASKED_NAME) %>%
  dplyr::summarize(value = mean(value)) %>%
  mutate(broader_prefix = ifelse(broader_prefix < 10, paste0("0", broader_prefix),
                                 broader_prefix)) %>%
  mutate(brand = paste0(broader_prefix, ". ", MASKED_NAME)) %>%
  mutate(
    `Error Type` = ifelse(name == "mpe", "Mean Percent Error",
                          "Absolute Percent Error")
  ) -> temp
  
temp %>%
  filter(value < 120978) %>%
  filter(`Error Type` == "Absolute Percent Error") %>%
  mutate(
    category = ifelse(as.numeric(broader_prefix) <= 16, "Traditional", "Electronic")
  ) %>%
  group_by(category, `Error Type`, iter) %>%
  dplyr::summarize(value = mean(value)) %>%
  ggplot(aes(x = category, y = log(value), 
             group = category, fill = category)) +
  geom_boxplot() +
  coord_flip() +
  geom_vline(xintercept = 15.5) +
  facet_wrap(~`Error Type`, nrow = 2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Brand") +
  ylab("Logged Out-of-Sample Error") +
  geom_hline(yintercept = log(100), color = "red") -> fig

ggsave("paper/figures/out_of_sample_performance.png",
       plot = fig, width = 22.2, height = 13, units = "in")


#Average Own Elasticity Matrix--------------------------------------------------
if(!("avg_elas_long" %in% dbListTables(results))){
  avg_elas <- dbReadTable(results2, "avg_elas")
  colnames(avg_elas) <- colnames(avg_elas) %>%
    gsub("B_", "", .) %>%
    gsub("\\.", " ", .) %>%
    gsub("MASKED", .) %>%
    gsub("MASKED", .)
  rownames(avg_elas) <- colnames(avg_elas)[2:37]
  
  avg_elas %>%
    select(-Brand) %>%
    rownames_to_column(var = "brand") %>%
    reshape2::melt() %>%
    inner_join(dbReadTable(results, "brand_map_and_mask") %>%
                 select(brand, broader_prefix, MASKED_NAME) %>%
                 dplyr::rename(num_prefix = broader_prefix, num_name = MASKED_NAME),
               by = "brand") %>%
    inner_join(dbReadTable(results, "brand_map_and_mask") %>%
                 select(brand, broader_prefix, MASKED_NAME) %>%
                 dplyr::rename(wrt_prefix = broader_prefix, wrt_name = MASKED_NAME,
                               variable = brand),
               by = "variable") %>%
    group_by(num_name, wrt_name, num_prefix, wrt_prefix) %>%
    dplyr::summarize(elas = mean(value)) %>%
    mutate(num_prefix = ifelse(num_prefix < 10, paste0("0", num_prefix), num_prefix),
           wrt_prefix = ifelse(wrt_prefix < 10, paste0("0", wrt_prefix), wrt_prefix))  %>%
    mutate(num_name = paste0(num_prefix, ". ", num_name),
           wrt_name = paste0(wrt_prefix, ". ", wrt_name)) %>%
    ungroup() %>%
    select(-num_prefix, -wrt_prefix) %>%
    dbWriteTable(results2, "avg_elas_long", ., overwrite = T)
}

dbReadTable(results2, "avg_elas_long") %>%
  mutate(num_prefix = substr(num_name, 1, 2),
         wrt_prefix = substr(wrt_name, 1, 2)) %>%
  mutate(num_name = paste0(num_prefix, ". ", ifelse(as.numeric(num_prefix) <= 16, "Trad. Brand", "E. Brand")),
         wrt_name = paste0(wrt_prefix, ".", ifelse(as.numeric(wrt_prefix) <= 16, "Trad. Brand", "E. Brand"))) %>%
  filter(elas < 0) %>%
  ggplot(aes(factor(wrt_name), factor(num_name))) +
  geom_tile(aes(fill = elas), color = "white") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle= 90, vjust = 0, hjust = 0),
    legend.position = "bottom",
    legend.key.size = unit(1.5, "cm"),
    legend.text = element_text(size = 10)
  ) +
  ylab("") + xlab("") +
  scale_fill_gradient(low = color_pal$sunset, high = "gray95") +
  geom_vline(xintercept = 16.5, size = 1) +
  geom_hline(yintercept = 16.5, size = 1) +
  labs(fill = "Own-Price Elasticity") -> fig

ggsave("paper/figures/own_elasticity.png",
       plot = fig, width = 22.2, height = 13, units = "in")

#Average Cross-Price Elasticity Matrix------------------------------------------
dbReadTable(results2, "avg_elas_long") %>%
  mutate(num_prefix = substr(num_name, 1, 2),
         wrt_prefix = substr(wrt_name, 1, 2)) %>%
  mutate(num_name = paste0(num_prefix, ". ", ifelse(as.numeric(num_prefix) <= 16, "Trad. Brand", "E. Brand")),
         wrt_name = paste0(wrt_prefix, ".", ifelse(as.numeric(wrt_prefix) <= 16, "Trad. Brand", "E. Brand"))) %>%
  filter(elas > 0) %>%
  ggplot(aes(factor(wrt_name), factor(num_name))) +
  geom_tile(aes(fill = elas), color = "white") +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank(),
    axis.text.x = element_text(angle= 90, vjust = 0, hjust = 0),
    legend.position = "bottom",
    legend.key.size = unit(1.5, "cm"),
    legend.text = element_text(size = 10)
  ) +
  ylab("") + xlab("") +
  scale_fill_gradient(low = "grey95", high = color_pal$river_green) +
  geom_vline(xintercept = 16.5, size = 2) +
  geom_hline(yintercept = 16.5, size = 2) +
  labs(fill = "Cross-Price Elasticity") -> fig

ggsave("paper/figures/cross_elasticity.png",
       plot = fig, width = 22.2, height = 13,
       units = "in")
  
#Avg Brand Level Elasticity By Category Year------------------------------------
if(!("elas_by_market" %in% dbListTables(results))){
  load("code/05_analysis/elasticity_lists2.RData")
  elas_list <- list()
  for(i in 1:length(elasticity_list)){
    
    em <- elasticity_list[[i]]
    market <- em$market %>% unique()
    em <- em %>% select(-market)
    em <- em %>%
      rownames_to_column(var = "BRAND") %>%
      reshape2::melt() %>%
      filter(BRAND == variable) %>%
      mutate(market = market) %>%
      inner_join((dbReadTable(main, "brands") %>% dplyr::rename(BRAND = brand_descr) %>% select(BRAND, category)),
                 by = "BRAND") %>%
      group_by(category, market) %>%
      dplyr::summarize(mean_own = mean(value, na.rm = TRUE)) 
    elas_list[[i]] <- em
    print(i)
    
  }
  
  elas_list <- Reduce(rbind, elas_list)
  
  elas_list %>%
    dplyr::rename(cdid = market) %>%
    dbWriteTable(results2, "elas_by_market", ., overwrite = T)
  rm(elasticity_list, elasticity_list2)
  
}

dbReadTable(results2, "elas_by_market") %>%
  inner_join(sdf2 %>% select(cdid, year) %>% distinct(), by = "cdid") %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) %>%
  group_by(year, category) %>%
  dplyr::summarize(
    own = mean(mean_own, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = as.factor(year), y = own, color = category, group = category)) +
  ylab("Own-Price Elasticity") +
  xlab("Year") +
  theme_bw() +
  scale_color_manual(values = c(color_pal$river_green, color_pal$sunset)) +
  geom_line() +
  geom_point() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank(),
    axis.text.x = element_text(angle= 90, vjust = 0, hjust = 0),
    legend.position = "bottom",
    legend.key.size = unit(1.5, "cm"),
    legend.text = element_text(size = 10)
  ) -> fig

ggsave("paper/figures/own_over_time.png",
       width = 22.2, height = 13,
       units = "in", plot = fig)


#Average Markups by Brand-------------------------------------------------------
temp <- dbReadTable(results2, "markup_and_cost") %>%
    group_by(brand) %>%
    dplyr::summarize(markup = mean(percent_markup, na.rm = T),
                  marginal_cost =mean(marginal_cost, na.rm = T)
    ) %>%
    left_join(sdf2 %>% select(brand, category) %>% distinct(), by = "brand") %>%
    inner_join(dbReadTable(results, "brand_map_and_mask") %>% 
                 select(brand, MASKED_NAME, broader_prefix),
               by = "brand") %>%
    group_by(MASKED_NAME, category, broader_prefix) %>%
    dplyr::summarize(markup = mean(markup),
                     marginal_cost = mean(marginal_cost)) %>%
    mutate(broader_prefix = ifelse(
      broader_prefix < 10, paste0("0", broader_prefix), broader_prefix)) %>%
    mutate(brand = paste0(broader_prefix, ". ", MASKED_NAME)) %>%
    ungroup() %>%
    select(-MASKED_NAME) %>%
    arrange(brand) 
  
temp %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) %>%
  ggplot(aes(x = brand, y = round(markup*100, 2), fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values = c(color_pal$river_green, color_pal$sunset)) +
  xlab("Brand") +
  ylab("Markup (%)") -> fig

ggsave("paper/figures/brand_markups.png", plot = fig,
       width = 22.2, height = 13, units = "in")

temp %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) %>%
  ggplot(aes(x = brand, y = marginal_cost, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values = c(color_pal$river_green, color_pal$sunset)) +
  xlab("Brand") +
  ylab("Marginal Cost ($)") -> fig

ggsave("paper/figures/brand_mc.png", plot = fig,
       width = 22.2, height = 13, units = "in")

temp %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) %>%
  group_by(category) %>%
  dplyr::summarize(
    markup = round(mean(markup)*100, 1),
    marginal_cost = mean(marginal_cost)
  ) %>%
  pivot_longer(cols = c(markup, marginal_cost)) %>%
  mutate(
    name = ifelse(name == "marginal_cost", "Marginal Cost", "Markup")
  ) %>%
  ggplot(aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity") +
  #coord_flip() +
  theme_bw() +
  scale_fill_manual(values = c(color_pal$river_green, color_pal$sunset)) +
  xlab("Brand") +
  ylab("Markup (%)") +
  facet_wrap(~name)
  
#Markups Over Time By Category--------------------------------------------------
temp <- dbReadTable(results2, "markup_and_cost") %>%
  inner_join(sdf2 %>% select(brand, year, cdid, category) %>% distinct(),
             by = c("cdid", "brand")) %>%
  group_by(category, year) %>%
  dplyr::summarize(markup = mean(percent_markup, na.rm = T),
                   marginal_cost = mean(marginal_cost, na.rm = T))

temp %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) %>%
  ggplot(aes(x = as.factor(year), y = round(markup*100, 2), color = category, group = category)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = c(color_pal$river_green, color_pal$sunset)) +
  xlab("Year") +
  ylab("Markup (%)") -> fig

ggsave("paper/figures/markup_over_time.png", plot = fig,
       width = 22.2, height = 13, units = "in")
  
temp %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) %>%
  ggplot(aes(x = as.factor(year), y = marginal_cost, color = category, group = category)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = c(color_pal$river_green, color_pal$sunset)) +
  xlab("Brand") +
  ylab("Marginal Cost ($)") -> fig

ggsave("paper/figures/mc_over_time.png", plot = fig,
       width = 22.2, height = 13, units = "in")

#Laffer Curve-------------------------------------------------------------------
dbReadTable(results2, "counterfactuals_all_metrics") %>%
  filter(cfid == 4 & category == 7460) %>%
  left_join(sdf2 %>% select(year, quarter, cdid) %>% distinct(),
            by = "cdid") %>%
  select(cdid, year, quarter, brand, tax, gov_rev) %>%
  group_by(cdid, tax) %>%
  dplyr::summarize(
    Average = mean(gov_rev)/2.9
  ) %>%
  pivot_longer(cols = c(Average)) %>%
  group_by(name) %>%
  dplyr::mutate(max_rev = max(value)) %>%
  dplyr::rename(Statistic = name) %>%
  ggplot(aes(x = as.factor(tax), y = value, group = Statistic)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  geom_vline(xintercept = "0.6") +
  xlab("Tax ($)") +
  ylab("Average Market Government Revenue ($)") -> fig

ggsave("paper/figures/brand_gov_rev.png", plot = fig,
       width = 22.2, height = 13, units = "in")


  


#IV Robustness------------------------------------------------------------------
dbReadTable(results2, "iv_robustness") %>%
  filter(price >= quantile(price, 0.1) & price <= quantile(price, 0.9)) %>%
  ggplot(aes(x = price, y = "")) +
  geom_boxplot() +
  theme_bw() +
  xlab("Price Coefficient") +
  ylab("") -> fig

ggsave("paper/figures/iv_robust.png", plot = fig,
       width = 22.2, height = 13, units = "in")

#M&A Results--------------------------------------------------------------------
dbReadTable(results2, "ma_counterfactuals") %>%
  inner_join(
    sdf2 %>% select(brand, category) %>% distinct(),
    by = c("brand")
  ) %>%
  select(cdid, category, brand, market_size_real,
         price, cfprice, 
         inShare_hat_approx, inShare_hat_approx_cf,
         markup, cf_markup,
         marginal_cost, marginal_cost_cf,
         profit, profit_cf, 
         welfare, welfare_cf,
         group) %>%
  mutate(q0 = (inShare_hat_approx*market_size_real)/price,
         q1 = (inShare_hat_approx_cf*market_size_real/cfprice)
  ) %>%
  mutate(dq = q1 - q0) %>%
  group_by(cdid, category) %>%
  dplyr::mutate(cat_share = sum(inShare_hat_approx),
                cat_share_cf = sum(inShare_hat_approx_cf)) %>%
  group_by(category, cdid) %>%
  dplyr::mutate(
    group = case_when(group == "seller" ~ "Acquired Brand(s)",
                      group == "buyer" ~ "Acquring Firm's Existing Brands",
                      group == "outside" ~ "Competing Brands")
  ) %>%
  group_by(category, group) %>%
  dplyr::summarize(
    dp = mean(cfprice - price, na.rm = T)/mean(price),
    ds = mean(inShare_hat_approx_cf - inShare_hat_approx, na.rm = T)/mean(inShare_hat_approx),
    dmk = mean(cf_markup - markup, na.rm = T)/mean(markup),
    dc = mean(marginal_cost_cf - marginal_cost, na.rm = T)/mean(marginal_cost),
    dpi = mean(profit_cf - profit, na.rm = T)/mean(profit),
    dWb = mean(welfare_cf*(q1) - welfare*q1, na.rm = T)/mean(welfare*q1),
  ) %>%
  pivot_longer(cols = starts_with("d")) %>%
  mutate(name = case_when(
    name == "dp" ~ "Change in Price",
    name == "ds" ~ "Change in Share",
    name == "dc" ~ "Change in Cost",
    name == "dmk" ~ "Change in Markup",
    name == "dpi" ~ "Change in Profit",
    name == "dWb" ~ "Change in Consumer Surplus"
  )) %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) %>%
  ggplot(aes(x = group, y = value, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 5.5) +
  facet_wrap(~name, nrow = 3, scales = "free_x") +
  ylab("Change in Average Market") +
  xlab("") +
  scale_fill_manual(values = c(color_pal$river_green,
                               color_pal$sunset)) +
  theme_bw() +
  theme(legend.position = "bottom") -> fig

ggsave("paper/figures/aq_impact.png", plot = fig,
       width = 22.2, height = 13, units = "in")
  

dbReadTable(results2, "ma_counterfactuals") %>%
  inner_join(
    sdf2 %>% select(brand, category) %>% distinct(),
    by = c("brand")
  ) %>%
  select(ma_cf_group, cdid, category, brand, market_size_real,
         price, cfprice, 
         inShare_hat_approx, inShare_hat_approx_cf,
         markup, cf_markup,
         marginal_cost, marginal_cost_cf,
         profit, profit_cf, 
         welfare, welfare_cf,
         group) %>%
  mutate(q0 = (inShare_hat_approx*market_size_real)/price,
         q1 = (inShare_hat_approx_cf*market_size_real/cfprice)
  ) %>%
  mutate(dq = q1 - q0) %>%
  group_by(cdid, ma_cf_group,category) %>%
  dplyr::mutate(cat_share = sum(inShare_hat_approx),
                cat_share_cf = sum(inShare_hat_approx_cf)) %>%
  group_by(category) %>%
  dplyr::summarize(
    dshare = mean(cat_share_cf - cat_share)/mean(cat_share),
    dshare_sd = sd(cat_share_cf - cat_share)/mean(cat_share),
    n = n_distinct(cdid),
    se = dshare_sd/(sqrt(n)))


#ban Results--------------------------------------------------------------------
dbReadTable(results2, "random_ban_counterfactuals") %>%
  inner_join(
    sdf2 %>% select(brand, category) %>% distinct(),
    by = c("brand")
  ) %>%
  select(cdid, category, brand, banned_brand, market_size_real,
         price, cfprice, 
         inShare_hat_approx, inShare_hat_approx_cf,
         markup, cf_markup,
         marginal_cost, marginal_cost_cf,
         profit, profit_cf, 
         welfare, welfare_cf) %>%
  mutate(is_banned = ifelse(brand == banned_brand, 1, 0)) %>%
  mutate(q0 = (inShare_hat_approx*market_size_real)/price,
         q1 = (inShare_hat_approx_cf*market_size_real/cfprice)
  ) %>%
  mutate(dq = q1 - q0) %>%
  group_by(cdid, category) %>%
  dplyr::mutate(cat_share = sum(inShare_hat_approx),
                cat_share_cf = sum(inShare_hat_approx_cf)) %>%
  group_by(cdid) %>%
  dplyr::mutate(out_share = 1 - sum(inShare_hat_approx),
                out_share_cf = 1 - sum(inShare_hat_approx_cf)) %>%
  group_by(category, cdid) %>%
  group_by(category, is_banned) %>%
  dplyr::mutate(
    dp = mean(cfprice - price, na.rm = T)/mean(price),
    ds = mean(inShare_hat_approx_cf - inShare_hat_approx, na.rm = T)/mean(inShare_hat_approx),
    dmk = mean(cf_markup - markup, na.rm = T)/mean(markup),
    dc = mean(marginal_cost_cf - marginal_cost, na.rm = T)/mean(marginal_cost),
    dpi = mean(profit_cf - profit, na.rm = T)/mean(profit),
    dWb = mean(welfare_cf*(q1) - welfare*q1, na.rm = T)/mean(welfare*q1),
    dos = mean(out_share_cf - out_share)/mean(out_share)
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    dos = mean(out_share_cf - out_share)/mean(out_share)
  ) %>%
  select(category, is_banned, dp, ds, dmk, dc, dpi, dWb, dos) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("d")) %>%
  mutate(name = case_when(
    name == "dp" ~ "Change in Price",
    name == "ds" ~ "Change in Share",
    name == "dc" ~ "Change in Cost",
    name == "dmk" ~ "Change in Markup",
    name == "dpi" ~ "Change in Profit",
    name == "dWb" ~ "Change in Consumer Surplus",
    name == "dos" ~ "Change in Outside Share"
  )) %>%
  filter(is_banned != 1) %>%
  mutate(category = ifelse(category == 7467, "Traditional", "Electronic")) %>%
  dplyr::mutate(
    category = ifelse(name == "Change in Outside Share", "Outside", category)
  ) %>%
  dplyr::mutate(
    name = ifelse(category == "Outside", "Change in Share", name)
  ) %>%
  ggplot(aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 5.5) +
  facet_wrap(~name, nrow = 3, scales = "free_x") +
  ylab("Change in Average Market") +
  xlab("") +
  scale_fill_manual(values = c(color_pal$river_green,
                               color_pal$warm_neutral,
                               color_pal$sunset)) +
  theme_bw() +
  theme(legend.position = "bottom")

dbReadTable(results2, "ma_counterfactuals") %>%
  inner_join(
    sdf2 %>% select(brand, category) %>% distinct(),
    by = c("brand")
  ) %>%
  select(ma_cf_group, cdid, category, brand, market_size_real,
         price, cfprice, 
         inShare_hat_approx, inShare_hat_approx_cf,
         markup, cf_markup,
         marginal_cost, marginal_cost_cf,
         profit, profit_cf, 
         welfare, welfare_cf,
         group) %>%
  mutate(q0 = (inShare_hat_approx*market_size_real)/price,
         q1 = (inShare_hat_approx_cf*market_size_real/cfprice)
  ) %>%
  mutate(dq = q1 - q0) %>%
  group_by(cdid, ma_cf_group,category) %>%
  dplyr::mutate(cat_share = sum(inShare_hat_approx),
                cat_share_cf = sum(inShare_hat_approx_cf)) %>%
  group_by(category) %>%
  dplyr::summarize(
    dshare = mean(cat_share_cf - cat_share)/mean(cat_share),
    dshare_sd = sd(cat_share_cf - cat_share)/mean(cat_share),
    n = n_distinct(cdid),
    se = dshare_sd/(sqrt(n)))




  

#Cleanup------------------------------------------------------------------------
  rm(avg_elas, avg_elas_long, cig_occurences,
     cig_sales, dframe, ecig_occurences, ecig_sales, 
     elas_list, elasticity_list, elasticity_list2,
     em, fig, ibis, ibis_c, ibis_e, occurences,
     occurences_fig, rate_fig, raw_weekly,
     sales, sales_fig, temp, w, i)
  
  