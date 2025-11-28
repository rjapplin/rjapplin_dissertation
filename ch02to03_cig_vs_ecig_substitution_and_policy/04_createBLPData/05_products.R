brands <- dbGetQuery(knd.blp.int,
                     "SELECT DISTINCT brand_code_uc FROM blp_df_06_all")
brands <- brands$brand_code_uc

products_master <- dbGetQuery(knd,
  glue::glue_sql(
    "SELECT * FROM rms_products WHERE brand_code_uc IN ({brands*}) AND
    product_module_code IN (7460, 7467)",
    .con = knd
  )
)

upcs <- unique(products_master$upc)

for(year in 2006:2020){
  
  if(year == 2006){
  products_info <- dbGetQuery(knd,
                              glue::glue_sql(
                               "SELECT CAST(upc as CHAR(12)) AS upc, * FROM rms_products_extra_2006
                                WHERE upc IN ({upcs*})",
                               .con = knd
                              )
  )
  } else {
    products_info <- rbind(products_info, dbGetQuery(knd,
                                           glue::glue_sql(
                                             paste0("SELECT CAST(upc as CHAR(12)) AS upc, * FROM rms_products_extra_", year, "
                                WHERE upc IN ({upcs*})"),
                                             .con = knd
                                           )))
  }
  
}

products_info$upc <- as.numeric(products_info$upc)
products_info[,1] <- NULL
products_master$upc <- as.numeric(products_master$upc)


products_info <- products_info %>%
  select(upc, upc_ver_uc, panel_year, flavor_descr, form_descr, style_descr, type_descr,
         common_consumer_name_descr, strength_descr) %>%
  distinct(upc, upc_ver_uc, flavor_descr, form_descr, style_descr, type_descr,
           common_consumer_name_descr, strength_descr)


products2 <- inner_join(distinct(products_master, upc, upc_ver_uc, brand_code_uc, brand_descr, product_module_code),
                        products_info, by = c("upc", "upc_ver_uc"))

products2 %>%
  mutate(flavor = ifelse(product_module_code == 7460, type_descr, flavor_descr),
         strength = strength_descr,
         type = ifelse(product_module_code == 7460, style_descr, paste0(form_descr, " ", common_consumer_name_descr))) %>%
  select(upc, upc_ver_uc, brand_code_uc, brand_descr, flavor, strength, type) -> products3

products <- products3

write_tsv(
  products, paste0(direcs$dat.der, "nielsen/products.tsv")
)




