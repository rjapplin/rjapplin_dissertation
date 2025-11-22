brand_info <- read_csv("cig_brands.csv") %>%
  mutate(subparent = ifelse(is.na(subparent), brand_descr, subparent),
         parent = ifelse(is.na(parent), brand_descr, parent)) 


cigs <- read_tsv(
  paste0(direcs$dat.der, "nielsen/aggregate/twoway/week/tw_wk_br_master.tsv")
) %>%
  filter(product_id == 7460) %>%
  mutate(year = substr(week, 1, 4)) %>%
  inner_join(brand_info, by = "brand_code_uc") %>%
  group_by(subparent, year) %>%
  dplyr::summarize(total_sales = sum(total_sales),
                   total_volume = sum(total_volume),
                   avg_price = mean(avg_price)) %>%
  ungroup() %>%
  group_by(year) %>%
  dplyr::mutate(all_sales = sum(total_sales),
                all_volume = sum(total_volume),
                all_avg_price = mean(avg_price)) %>%
  mutate(sales_market_share = 100*total_sales/all_sales,
         vol_market_share = 100*total_volume/all_volume,
         price_ratio = avg_price/all_avg_price) %>%
  ungroup() %>%
  group_by(year) %>%
  dplyr::mutate(rank_sales = rank(sales_market_share),
                rank_vol = rank(vol_market_share),
                rank_price = rank(price_ratio)) 


cigs %>%
  group_by(year) %>%
  dplyr::mutate(top_three_sales = ifelse(rank_sales >= max(rank_sales) - 2, 1, 0)) %>%
  filter(top_three_sales == 1) %>%
  dplyr::summarize(ratio = sum(sales_market_share),
                   ratio_type = "Three Firm (Sales)") -> three_firm_sales

cigs %>%
  group_by(year) %>%
  dplyr::mutate(top_five_sales = ifelse(rank_sales >= max(rank_sales) - 4, 1, 0)) %>%
  filter(top_five_sales == 1) %>%
  dplyr::summarize(ratio = sum(sales_market_share),
                   ratio_type = "Five Firm (Sales)") -> five_firm_sales


cigs %>%
  group_by(year) %>%
  dplyr::mutate(top_three_vol = ifelse(rank_vol >= max(rank_vol) - 2, 1, 0)) %>%
  filter(top_three_vol == 1) %>%
  dplyr::summarize(ratio = sum(vol_market_share),
                   ratio_type = "Three Firm (Volume)") -> three_firm_vol

cigs %>%
  group_by(year) %>%
  dplyr::mutate(top_five_vol = ifelse(rank_vol >= max(rank_vol) - 4, 1, 0)) %>%
  filter(top_five_vol == 1) %>%
  dplyr::summarize(ratio = sum(vol_market_share),
                   ratio_type = "Five Firm (Volume)") -> five_firm_vol

rbind(three_firm_sales, three_firm_vol, five_firm_sales, five_firm_vol) %>%
  ggplot(aes(x = year, y = ratio, group = ratio_type)) +
  geom_line(aes(color = ratio_type), size = 1.75) +
  xlab("Year") + ylab("Concentration Ratio") +
  theme_economist_white(gray_bg = FALSE) +
  scale_color_manual(values = unlist(color_pal)[c(1,4, 5, 7)] %>% unname(),
                     name = "Ratio Type") + 
  theme(
    legend.position = "bottom"
  ) -> g
ggsave(paste0(direcs$pap.fig, "nielsen/brand_shares/concentration_brand_7460.png"))

cigs %>%
  filter(subparent %in% c("MASKED")) %>%
    ggplot(aes(x = year, y = sales_market_share, group = subparent)) +
    geom_line(aes(color = subparent), size = 1.75) +
    geom_point(aes(color = subparent), size = 2) +
    scale_color_manual(values = unlist(color_pal)[c(1:7, 9)] %>% unname(),
                       name = "Brand") +
    theme_economist_white(gray_bg = FALSE) + xlab("Year") +
    ylab("Market Share (By Dollar Sales)") +
    theme(
      legend.position = "bottom"
    ) -> g
ggsave(paste0(direcs$pap.fig, "nielsen/brand_shares/brand_shares_sales_7460.png"))

cigs %>%
  filter(vol_market_share >= 5) %>%
  ggplot(aes(x = year, y = vol_market_share, group = subparent)) +
  geom_line(aes(color = subparent)) +
  geom_point(aes(color = subparent)) +
  scale_color_manual(values = unlist(color_pal)[c(1:7, 9)] %>% unname(),
                     name = "Brand") +
  theme_economist_white(gray_bg = FALSE) + xlab("Year") +
  ylab("Market Share (By Dollar Sales)") +
  theme(
    legend.position = "bottom"
  ) -> g
ggsave(paste0(direcs$pap.fig, "nielsen/brand_shares/brand_shares_vol_7460.png"))
