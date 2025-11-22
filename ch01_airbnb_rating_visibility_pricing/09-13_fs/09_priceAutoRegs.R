#Nominal------------------------------------------------------------------------
ia_prices <- dbGetQuery(iaan,
                        "SELECT KEY_LIST_ID, KEY_HOST_ID,
                        DATE, price
                        FROM [DATA.FULL_SAMPLE]") 
ia_prices <- ia_prices %>%
  arrange(KEY_LIST_ID, KEY_HOST_ID, DATE) %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  mutate(
    price1 = lag(price, n = 1),
    price2 = lag(price, n = 2),
    price3 = lag(price, n = 3),
    price4 = lag(price, n = 4),
    price5 = lag(price, n = 5),
    price6 = lag(price, n = 6),
    price7 = lag(price, n = 7),
    price8 = lag(price, n = 8),
    price9 = lag(price, n = 9),
    price10 = lag(price, n = 10),
    price11 = lag(price, n = 11),
    price12 = lag(price, n = 12),
    price13 = lag(price, n = 13),
    price14 = lag(price, n = 14),
    price15 = lag(price, n = 15),
    price16 = lag(price, n = 16),
    price17 = lag(price, n = 17),
    price18 = lag(price, n = 18),
    price19 = lag(price, n = 19),
    price20 = lag(price, n = 20)
  ) %>%
  ungroup() %>%
  select(starts_with("price"))

AIC_nom <- vector()
BIC_nom <- vector()
for(l in 1:20){
  
  rform <- paste0("price ~ ", paste0("price", 1:l, collapse = " + "))
  
  reg <- ia_prices %>% 
    filter(!(is.na(price20))) %>%
    lm(rform, .)
  AIC_nom[l] <- AIC(reg)
  BIC_nom[l] <- BIC(reg)
  rm(reg)
  
}

data.frame(plag = 1:20, aic = AIC_nom, bic = BIC_nom) %>%
  pivot_longer(cols = c(aic, bic)) %>%
  dbWriteTable(iaan,
               "FIGURE.PRICE_LAG_IC",
               .,
               overwrite = T)

rform <- paste0("price ~ ", paste0("price", 1:20, collapse = " + "))
reg <- lm(rform, ia_prices)
regsum <- summary(reg)
regsum$coefficients %>%
  as.data.frame() %>%
  mutate(AdjRSq = regsum$adj.r.squared,
         fstat = regsum$fstatistic[1]) %>%
  dbWriteTable(iaan,
               "MODEL.PRICE_LAG_20",
               .,
               overwrite = T)

rform <- paste0("price ~ ", paste0("price", 1:3, collapse = " + "))
reg <- lm(rform, ia_prices)
regsum <- summary(reg)
regsum$coefficients %>%
  as.data.frame() %>%
  mutate(AdjRSq = regsum$adj.r.squared,
         fstat = regsum$fstatistic[1]) %>%
  dbWriteTable(iaan,
               "MODEL.PRICE_LAG_3",
               .,
               overwrite = T)

rm(AIC_nom, BIC_nom, l, rform, regsum, reg, ia_prices)

#Real Prices--------------------------------------------------------------------
ia_prices <- dbGetQuery(iaan,
                        "SELECT KEY_LIST_ID, KEY_HOST_ID,
                        DATE, year, month, price
                        FROM [DATA.FULL_SAMPLE]") %>%
  left_join(cpi, by = c("year", "month")) %>%
  mutate(price = (price/cpi_b2015)*100) %>%
  select(-cpi, -cpi_b2015, -year, -month)

ia_prices <- ia_prices %>%
  arrange(KEY_LIST_ID, KEY_HOST_ID, DATE) %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  mutate(
    price1 = lag(price, n = 1),
    price2 = lag(price, n = 2),
    price3 = lag(price, n = 3),
    price4 = lag(price, n = 4),
    price5 = lag(price, n = 5),
    price6 = lag(price, n = 6),
    price7 = lag(price, n = 7),
    price8 = lag(price, n = 8),
    price9 = lag(price, n = 9),
    price10 = lag(price, n = 10),
    price11 = lag(price, n = 11),
    price12 = lag(price, n = 12),
    price13 = lag(price, n = 13),
    price14 = lag(price, n = 14),
    price15 = lag(price, n = 15),
    price16 = lag(price, n = 16),
    price17 = lag(price, n = 17),
    price18 = lag(price, n = 18),
    price19 = lag(price, n = 19),
    price20 = lag(price, n = 20)
  ) %>%
  ungroup() %>%
  select(starts_with("price"))

AIC_nom <- vector()
BIC_nom <- vector()
for(l in 1:20){
  
  rform <- paste0("price ~ ", paste0("price", 1:l, collapse = " + "))
  
  reg <- ia_prices %>% 
    filter(!(is.na(price20))) %>%
    lm(rform, .)
  AIC_nom[l] <- AIC(reg)
  BIC_nom[l] <- BIC(reg)
  rm(reg)
  
}

data.frame(plag = 1:20, aic = AIC_nom, bic = BIC_nom) %>%
  pivot_longer(cols = c(aic, bic)) %>%
  dbWriteTable(iaan,
               "FIGURE.REAL_PRICE_LAG_IC",
               .,
               overwrite = T)


rform <- paste0("price ~ ", paste0("price", 1:20, collapse = " + "))
reg <- lm(rform, ia_prices)
regsum <- summary(reg)
regsum$coefficients %>%
  as.data.frame() %>%
  mutate(AdjRSq = regsum$adj.r.squared,
         fstat = regsum$fstatistic[1]) %>%
  dbWriteTable(iaan,
               "MODEL.REAL_PRICE_LAG_20",
               .,
               overwrite = T)

rform <- paste0("price ~ ", paste0("price", 1:3, collapse = " + "))
reg <- lm(rform, ia_prices)
regsum <- summary(reg)
regsum$coefficients %>%
  as.data.frame() %>%
  mutate(AdjRSq = regsum$adj.r.squared,
         fstat = regsum$fstatistic[1]) %>%
  dbWriteTable(iaan,
               "MODEL.REAL_PRICE_LAG_3",
               .,
               overwrite = T)

rm(AIC_nom, BIC_nom, l, rform, regsum, reg, ia_prices)
