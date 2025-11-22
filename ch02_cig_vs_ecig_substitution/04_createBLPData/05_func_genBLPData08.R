genBLPDataStructure08_characteristics <- function(){
  
  #Reads in the dataframe containing all cigarette and e-cigarette prooducts
  #along with their flavor, type, and strength. Puts a prefix to these
  #three for some data wrangling purposes
  df <- read_tsv(
    paste0(direcs$dat.der, "nielsen/products/blp_products.tsv")
  ) %>%
    select(upc, upc_ver_uc, brand_code_uc, flavor, type, strength) %>%
    mutate(flavor = paste0("f_", flavor),
           type = paste0("t_", type),
           strength = paste0("s_", strength)
    )
  
  #Get unique upcs
  upcs <- unique(df$upc)
  charList <- list()
  
  for(year in 2013:2020){
    
    if(year < 2013){
      
      #Read in data, filter to upcs in the unique upcs from the product
      #characteristic raw data, join in the product raw data, group by
      #time and location, split groups into separate data frames and
      #create a count of occurences of unique values (
      #read documentation below for more details
      #)
      knddf <- dbReadTable(knd.blp.int, paste0("blp_df_02_7460_", year)) %>%
        filter(as.numeric(upc) %in% upcs) %>%
        mutate(upc = as.numeric(upc)) %>%
        select(upc, upc_ver_uc, fips_state_code, fips_county_code, year, quarter, brand_code_uc) %>%
        inner_join(df, by = c("upc", "upc_ver_uc", "brand_code_uc")) %>%
        select(brand_code_uc, year, quarter, fips_state_code, fips_county_code,
               flavor, type, strength) %>%
        group_by(year, quarter, fips_state_code, fips_county_code) %>%
        group_split() %>%
        lapply(
          
          function(mkt){
            
            #Creates a frequency table based on brand code and flavor. Converts
            #it to a dataframe, uses the fact that dataframe is subset to
            #specific year and location and uses that to create columns
            #that give these values. Make brand name (a rowname) into a column
            flavor <- as.data.frame.matrix(table(mkt$brand_code_uc, mkt$flavor)) %>%
              rownames_to_column() %>%
              mutate(year = mkt$year[1],
                     quarter = mkt$quarter[1],
                     fips_state_code = mkt$fips_state_code[1],
                     fips_county_code = mkt$fips_county_code[1])
            
            #Same idea but for strength
            strength <- as.data.frame.matrix(table(mkt$brand_code_uc, mkt$strength)) %>%
              rownames_to_column() %>%
              mutate(year = mkt$year[1],
                     quarter = mkt$quarter[1],
                     fips_state_code = mkt$fips_state_code[1],
                     fips_county_code = mkt$fips_county_code[1])
            
            #Same idea but for type
            type <- as.data.frame.matrix(table(mkt$brand_code_uc, mkt$type)) %>%
              rownames_to_column() %>%
              mutate(year = mkt$year[1],
                     quarter = mkt$quarter[1],
                     fips_state_code = mkt$fips_state_code[1],
                     fips_county_code = mkt$fips_county_code[1])
            
            #Collect the results into a single list
            return(list(flavor, strength, type))
            
          }
          
        )
      
      charList[[year-2005]] <- knddf
      print(year)
    
    } else {
      
      #Does the same thing as above, but takes into account that there is now
      #cigarette and e-cigarette data
      knddf <- dbGetQuery(knd.blp.int,
                        paste0("SELECT * FROM blp_df_02_7460_", year, " UNION SELECT * FROM blp_df_02_7467_", year)
      ) %>%
        filter(as.numeric(upc) %in% upcs) %>%
        select(upc, upc_ver_uc, fips_state_code, fips_county_code, year, quarter, brand_code_uc) %>%
        mutate(upc = as.numeric(upc)) %>%
        inner_join(df, by = c("upc", "upc_ver_uc", "brand_code_uc")) %>%
        select(brand_code_uc, year, quarter, fips_state_code, fips_county_code,
               flavor, type, strength) %>%
        group_by(year, quarter, fips_state_code, fips_county_code) %>%
        group_split() %>%
        lapply(
          
          function(mkt){
            
            flavor <- as.data.frame.matrix(table(mkt$brand_code_uc, mkt$flavor)) %>%
              rownames_to_column() %>%
              mutate(year = mkt$year[1],
                     quarter = mkt$quarter[1],
                     fips_state_code = mkt$fips_state_code[1],
                     fips_county_code = mkt$fips_county_code[1])
            
            strength <- as.data.frame.matrix(table(mkt$brand_code_uc, mkt$strength)) %>%
              rownames_to_column() %>%
              mutate(year = mkt$year[1],
                     quarter = mkt$quarter[1],
                     fips_state_code = mkt$fips_state_code[1],
                     fips_county_code = mkt$fips_county_code[1])

            type <- as.data.frame.matrix(table(mkt$brand_code_uc, mkt$type)) %>%
              rownames_to_column() %>%
              mutate(year = mkt$year[1],
                     quarter = mkt$quarter[1],
                     fips_state_code = mkt$fips_state_code[1],
                     fips_county_code = mkt$fips_county_code[1])
            
            return(list(flavor, strength, type))
            
          }
          
        )
      
      charList[[year-2005]] <- knddf
      
    }
    
  }
  
  #Do a loop based on number of years of data (15). Note that charList[[t]]
  #contains a list of frequency tables for flavor, strength, and type for
  #year t. 
  for(t in 1:15){
    for(nm in 1:length(charList[[t]])){
      if(nm == 1 & t == 1){
        flavor <- charList[[t]][[nm]][[1]]
        strength <- charList[[t]][[nm]][[2]]
        type <- charList[[t]][[nm]][[3]]
      } else {
        flavor <- rbind.fill(flavor, charList[[t]][[nm]][[1]])
        strength <- rbind.fill(strength, charList[[t]][[nm]][[2]])
        type <- rbind.fill(type, charList[[t]][[nm]][[3]])
      }
      #print(nm)
    }
    print(t)
  }
  
  #Convert NAs to 0s in flavor counts
  flavor %>%
    dplyr::mutate(f_fruit = ifelse(is.na(f_fruit), 0, f_fruit),
                  f_NA = ifelse(is.na(f_NA), 0, f_NA),
                  f_other = ifelse(is.na(f_other), 0, f_other),
                  f_regular = ifelse(is.na(f_regular), 0, f_regular),
                  f_menthol = ifelse(is.na(f_menthol), 0, f_menthol)
    ) -> flavor
  flavor %>%
    dplyr::rename(brand_code_uc = rowname) -> flavor
  
  #Convert NAs to 0s in strength counts
  strength %>%
    dplyr::mutate(
      s_low = ifelse(is.na(s_low), 0, s_low),
      s_mid = ifelse(is.na(s_mid), 0, s_mid),
      s_NA = ifelse(is.na(s_NA), 0, s_NA),
      s_regular = ifelse(is.na(s_regular), 0, s_regular),
      `s_ultra low` = ifelse(is.na(`s_ultra low`), 0, `s_ultra low`),
      s_strong = ifelse(is.na(s_strong), 0, s_strong),
      s_other = ifelse(is.na(s_other), 0, s_other)
    ) -> strength
  strength %>%
    dplyr::rename(brand_code_uc = rowname) -> strength
  
  #Convert NAs to 0s in type counts
  type %>%
    dplyr::mutate(
      t_device = ifelse(is.na(t_device), 0, t_device),
      t_filter = ifelse(is.na(t_filter), 0, t_filter),
      t_nofilter = ifelse(is.na(t_nofilter), 0, t_nofilter),
      t_refill = ifelse(is.na(t_refill), 0, t_refill),
      t_kit = ifelse(is.na(t_kit), 0, t_kit),
      t_NA = ifelse(is.na(t_NA), 0, t_NA),
      t_other = ifelse(is.na(t_other), 0, t_other)
    ) -> type
  type %>%
    dplyr::rename(brand_code_uc = rowname) -> type

  write_tsv(flavor, paste0(direcs$dat.der, "nielsen/products/flavor.tsv"))
  write_tsv(strength, paste0(direcs$dat.der, "nielsen/products/strength.tsv"))
  write_tsv(type, paste0(direcs$dat.der, "nielsen/products/type.tsv"))
  
  #Sum flavor counts to get total products for each brand. Grouped by
  #time and location. Create column that equals a brands total number of
  #products across brands by time and location. Create brand's flavor
  #make up for location l and time t by taking number of flavor characteristic
  #c products and dividing by prodN. Create IV by removing brand's own
  #characteristic c count from total number of characteristic c across all
  #brands and dividing by total number of products net of brand's own
  flavor %>%
    dplyr::mutate(
      prodN = rowSums(.[c(2, 3, 4, 9, 10)])
    ) %>%
    group_by(year, quarter, fips_state_code, fips_county_code) %>%
    dplyr::mutate(
      n = sum(f_menthol) + sum(f_regular) + sum(f_fruit) + sum(f_NA) + sum(f_other)
    ) %>%
    mutate(
          f_fruit_p = f_fruit/prodN,
          f_menthol_p = f_menthol/prodN,
          f_NA_p = f_NA/prodN,
          f_regular_p = f_regular/prodN,
          f_other_p = f_other/prodN
        ) %>%
        dplyr::mutate(
          f_fruit_iv = (sum(f_fruit) - f_fruit)/(n - prodN),
          f_menthol_iv = (sum(f_menthol) - f_menthol)/(n - prodN),
          f_NA_iv = (sum(f_NA) - f_NA)/(n - prodN),
          f_other_iv = (sum(f_other) - f_other)/(n - prodN),
          f_regular_iv = (sum(f_regular) - f_regular)/(n - prodN)
        ) %>%
        select(year, quarter, fips_state_code, fips_county_code, brand_code_uc, f_fruit_p, f_menthol_p, f_regular_p, f_other_p, f_NA_p,
               f_fruit_iv, f_menthol_iv, f_regular_iv, f_other_iv, f_NA_iv, prodN, n) %>%
        dplyr::rename(
          fruit = f_fruit_p,
          menthol = f_menthol_p,
          tobacco = f_regular_p,
          f_other = f_other_p,
          f_unknown = f_NA_p,
          IV_f_tobacco = f_regular_iv,
          IV_f_menthol = f_menthol_iv,
          IV_f_fruit = f_fruit_iv,
          IV_f_other = f_other_iv,
          IV_f_NA = f_NA_iv
        ) -> flavor2
  
  
  strength %>%
    dplyr::mutate(
      prodN = rowSums(.[c(2, 3, 4, 5, 6, 11, 12)])
    ) %>%
    group_by(year, quarter, fips_state_code, fips_county_code) %>%
    dplyr::mutate(
      n = sum(prodN)
    ) %>%
    mutate(
      low = s_low/prodN,
      mid = s_mid/prodN,
      s_other_p = s_other/prodN,
      regular = s_regular/prodN,
      ultralow = `s_ultra low`/prodN,
      strong = s_strong/prodN,
      s_unknown_p = s_NA/prodN
    ) %>%
    dplyr::mutate(
      IV_s_low = (sum(s_low) - s_low)/(n - prodN),
      IV_s_mid = (sum(s_mid) - s_mid)/(n - prodN),
      IV_s_other = (sum(s_other) - s_other)/(n - prodN),
      IV_s_regular = (sum(s_regular) - s_regular)/(n - prodN),
      IV_s_ultralow = (sum(`s_ultra low`) - `s_ultra low`)/(n - prodN),
      IV_s_strong = (sum(s_strong) - s_strong)/(n - prodN),
      IV_s_NA = (sum(s_NA) - s_NA)/(n - prodN)
    ) %>%
    select(year, quarter, fips_state_code, fips_county_code, brand_code_uc,
           low, mid, s_other_p, regular, ultralow, strong, s_unknown_p,
           IV_s_low, IV_s_mid, IV_s_other, IV_s_regular, IV_s_ultralow,
           IV_s_strong, IV_s_NA, prodN, n) %>%
    dplyr::rename(
      s_other = s_other_p,
      s_unknown = s_unknown_p
    ) -> strength2
  
  type %>%
    dplyr::mutate(
      prodN = rowSums(.[c(2, 3, 4, 5, 6, 11, 12)])
    ) %>%
    group_by(year, quarter, fips_state_code, fips_county_code) %>%
    dplyr::mutate(
      n = sum(prodN)
    ) %>%
    dplyr::mutate(
      device = t_device/prodN,
      filter = t_filter/prodN,
      kit = t_kit/prodN,
      nofilter = t_nofilter/prodN,
      refill = t_refill/prodN,
      t_unknown = t_NA/prodN,
      t_other_p = t_other/prodN
    ) %>%
    dplyr::mutate(
      IV_t_device = (sum(t_device) - t_device)/(n - prodN),
      IV_t_filter = (sum(t_filter) - t_filter)/(n - prodN),
      IV_t_kit = (sum(t_kit) - t_kit)/(n - prodN),
      IV_t_nofilter = (sum(t_nofilter) - t_nofilter)/(n - prodN),
      IV_t_refill = (sum(t_refill) - t_refill)/(n - prodN),
      IV_t_NA = (sum(t_NA) - t_NA)/(n - prodN),
      IV_t_other = (sum(t_other) - t_other)/(n - prodN)
    ) %>%
    select(year, quarter, fips_state_code, fips_county_code, brand_code_uc,
           device, filter, kit, nofilter, refill, t_unknown, t_other_p,
           IV_t_device, IV_t_filter, IV_t_kit, IV_t_nofilter, IV_t_refill,
           IV_t_NA, IV_t_other, prodN, n) %>%
    dplyr::rename(
      t_other = t_other_p
    ) -> type2
  
  flavor2 <- flavor2 %>% mutate(brand_code_uc = as.numeric(brand_code_uc))
  type2 <- type2 %>% mutate(brand_code_uc = as.numeric(brand_code_uc))
  strength2 <- strength2 %>% mutate(brand_code_uc = as.numeric(brand_code_uc))
  
  mergevar <- c("year", "quarter", "fips_state_code", "fips_county_code", "brand_code_uc")
  
  characteristics <- inner_join(flavor2, type2, by = mergevar) %>%
    merge(strength2, by = mergevar) %>%
    select(brand_code_uc, year, quarter, fips_state_code, fips_county_code,
           fruit, menthol, tobacco, f_other, f_unknown, device, filter, kit,
           nofilter, refill, t_unknown, t_other, low, mid, s_other, regular,
           ultralow, strong, s_unknown, prodN, n)
    
    IVs <- inner_join(flavor2, type2, by = mergevar) %>%
      merge(strength2, by = mergevar) %>%
      select(brand_code_uc, year, quarter, fips_state_code, fips_county_code,
             IV_f_fruit, IV_f_menthol, IV_f_tobacco, IV_f_other, IV_f_NA,
             IV_t_device, IV_t_filter, IV_t_kit, IV_t_nofilter, IV_t_refill,
             IV_t_NA, IV_t_other, IV_s_low, IV_s_mid, IV_s_other, IV_s_regular,
             IV_s_ultralow, IV_s_strong, IV_s_NA, prodN, n)
  
    taxes <- read_tsv(paste0(direcs$dat.der, "taxes/tob_taxes_qtr_master.tsv")) %>%
      filter(productID %in% c(7460, 7467)) %>%
      mutate(IV_tax1 = ifelse(productID == 7460, taxAmount, taxAmount_35mu_tiv),
             IV_tax2 = ifelse(productID == 7460, taxAmount, taxAmount_35mu_tv),
             IV_tax3 = ifelse(productID == 7460, taxAmount, taxAmount_20mu_tv)) %>%
      select(year, quarter, productID, state, IV_tax1, IV_tax2, IV_tax3) %>%
      dplyr::rename(category = productID,
             fips_state_code = state) %>%
      mutate(IV_tax_avg = (IV_tax1 + IV_tax2 + IV_tax3)/3)
  
    characteristics %>%
      group_by(brand_code_uc) %>%
      dplyr::summarize(filter = mean(filter)) %>%
      mutate(filterdum = filter == 0) %>%
      arrange(filterdum) %>%
      mutate(category = ifelse(filterdum == 0, 7467, 7460)) %>%
      select(brand_code_uc, category) -> brand_category
    
    IVs <- inner_join(IVs, brand_category, by = c("brand_code_uc")) %>%
      left_join(taxes, by = c("year", "quarter", "fips_state_code", "category")) %>%
      mutate(IV_tax1 = ifelse(is.na(IV_tax1), 0, IV_tax1),
             IV_tax2 = ifelse(is.na(IV_tax2), 0, IV_tax2),
             IV_tax3 = ifelse(is.na(IV_tax3), 0, IV_tax3),
             IV_tax_avg = ifelse(is.na(IV_tax_avg), 0, IV_tax_avg)
      )
    
    IVs <- IVs %>% select(-category)
    
    read_tsv(paste0(direcs$dat.der, "nielsen/products/blp_products.tsv")) %>%
      select(brand_code_uc, brand_descr, subparent, parent) %>%
      distinct() %>%
      merge(brand_category, by = "brand_code_uc") -> brands
    
    
    write_tsv(
      brands, paste0(direcs$hom.dat, "main/blp_brands.tsv")
    )
    
    write_tsv(
      IVs, paste0(direcs$hom.dat, "main/blp_ivs.tsv")
    )
    
    write_tsv(
      characteristics, paste0(direcs$hom.dat, "main/blp_character.tsv")
    )
    
    
    
  
}
  