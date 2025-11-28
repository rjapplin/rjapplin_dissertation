union(idPre18Samp(2006:2017, 7460), idPre18Samp(2013:2017, 7467)) -> stores

stores %>%
  data.frame(stores = .) %>%
  write_tsv(
    paste0(direcs$dat.der, "nielsen/stores_06_17_sample.tsv")
  )

rm(stores)
