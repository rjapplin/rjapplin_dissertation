#Category Level Regressions-----------------------------------------------------
df2 %>%
  group_by(category, cdid, fips_county_code, year, quarter) %>%
  dplyr::summarize(price = mean(price),
         inShare = sum(inShare),
         iv1 = sum(iv1),
         iv2 = sum(iv2),
         iv3 = sum(iv3),
         iv4 = sum(iv4),
         iv5 = sum(iv5),
         iv6 = sum(iv6),
         iv7 = sum(iv7),
         iv8 = sum(iv8),
         iv9 = sum(iv9),
         iv10 = sum(iv10),
  ) %>%
  mutate(outShare = 1 - inShare) -> agg_df

felm(log(inShare) ~ as.factor(category) | year + fips_county_code | 
       (log(price) ~ iv1 + iv2 + iv3 + iv4 + iv8 + iv9 + iv10), data =agg_df)


felm(I(log(inShare) - log(outShare)) ~ as.factor(category) | year + fips_county_code | 
    (price~ iv1 + iv2 + iv3 + iv4 + iv5 + iv6 + iv8 + iv9 + iv10), data =agg_df)
     