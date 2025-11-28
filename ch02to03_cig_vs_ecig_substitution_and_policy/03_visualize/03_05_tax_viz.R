taxes <- read_tsv(
  paste0(direcs$dat.der, "taxes/tob_taxes_qtr_master.tsv")
) %>%
  filter(productID %in% c(7460, 7467)) %>%
  dplyr::select(year, state, state_abbr, productID, avgAnnualTax, 
                avgAnnualTax_20mu_tv, avgAnnualTax_35mu_tiv,
                avgAnnualTax_35mu_tv) %>%
  distinct() %>%
  filter(state_abbr %in% state.abb) %>%
  inner_join(
    read_tsv(paste0(direcs$dat.ext, "fred/cpi_ann.tsv")), by = "year") %>%
  group_by(productID) %>%
  group_split()

#Figures for Cigarette Taxes----------------------------------------------------
taxes_7460 <- taxes[[1]] %>%
  dplyr::select(year, state, state_abbr, productID, avgAnnualTax, cpi_ann) %>%
  dplyr::mutate(region = tolower(state.name[match(state_abbr, state.abb)])) %>%
  dplyr::mutate(avgAnnualTax = ifelse(is.na(avgAnnualTax), 0, avgAnnualTax))

#Cigarette Tax Map--------------------------------------------------------------
taxes_7460_map <- left_join(map_data("state"), taxes_7460, by = "region")

#Nominal
taxes_7460_map %>%
  ggplot(
    aes(long, lat, group = group)
  ) +
  geom_polygon(aes(fill = avgAnnualTax), color = "gray") +
  theme_economist_white(gray_bg = FALSE) +
  scale_fill_gradient(low = "white", high = color_pal$wildcat_blue,
                      name = "Tax Rate") +
  theme(axis.line = element_line(color='black'),
        #plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        legend.position = "bottom"
  ) + xlab("") + ylab("") +
  facet_wrap(vars(year)) -> g
  ggsave(
    filename = paste0(direcs$pap.fig, "taxes/nom_taxes_7460.png"), g
  )

#Real
taxes_7460_map %>%
  dplyr::mutate(tax = toReal(avgAnnualTax, cpi_ann/2.7)) %>%
  ggplot(
    aes(long, lat, group = group)
  ) +
  geom_polygon(aes(fill = tax), color = "gray") +
  theme_economist_white(gray_bg = FALSE) +
  scale_fill_gradient(low = "white", high = color_pal$wildcat_blue,
                      name = "Real Tax Rate (Constant 2021 Dollars)") +
  theme(axis.line = element_line(color='black'),
        #plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        legend.position = "bottom"
  ) + xlab("") + ylab("") +
  facet_wrap(vars(year)) -> g
  ggsave(
    filename = paste0(direcs$pap.fig, "taxes/real_taxes_7460.png"), g
  )

  #E-Cigarette Tax Map
taxes_7467 <- taxes[[2]] %>%
  dplyr::select(year, state, state_abbr, productID, avgAnnualTax_20mu_tv,
               avgAnnualTax_35mu_tiv, avgAnnualTax_35mu_tv, cpi_ann) %>%
  dplyr::mutate(region = tolower(state.name[match(state_abbr, state.abb)]))

taxes_7467_map <- left_join(map_data("state"), taxes_7467, by = "region")

#Nominal
taxes_7467_map %>%
  ggplot(
    aes(long, lat, group = group)
  ) +
  geom_polygon(aes(fill = avgAnnualTax_35mu_tv), color = "gray") +
  theme_economist_white(gray_bg = FALSE) +
  scale_fill_gradient(low = "white", high = color_pal$wildcat_blue,
                      name = "Tax Rate") +
  theme(axis.line = element_line(color='black'),
        #plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        legend.position = "bottom"
  ) + xlab("") + ylab("") +
  facet_wrap(vars(year)) -> g
ggsave(
  filename = paste0(direcs$pap.fig, "taxes/nom_taxes_7467.png"), g
)

#Real
taxes_7467_map %>%
  ggplot(
    aes(long, lat, group = group)
  ) +
  geom_polygon(aes(fill = toReal(avgAnnualTax_35mu_tv, cpi_ann/2.7)),
               color = "gray") +
  theme_economist_white(gray_bg = FALSE) +
  scale_fill_gradient(low = "white", high = color_pal$wildcat_blue,
                      name = "Tax Rate") +
  theme(axis.line = element_line(color='black'),
        #plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        legend.position = "bottom"
  ) + xlab("") + ylab("") +
  facet_wrap(vars(year)) -> g
ggsave(
  filename = paste0(direcs$pap.fig, "taxes/real_taxes_7467.png"), g
)
