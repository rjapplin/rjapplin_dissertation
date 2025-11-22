#Step 1: Aggregate to qaurterly with sales-weighted price-----------------------
for(ID in c(prodID$tcigte, prodID$ecigte)){
  
  if(ID == prodID$tcigte){
    for(year in 2006:2020){
      dbWriteTable(knd.blp.int,
                   paste0("blp_df_01_", ID, "_", year),
                   genBLPDataStructure01_aggregate(ID, year, db = knd)
      )
    }
  } else if(ID == prodID$ecigte){
    for(year in 2013:2020){
      dbWriteTable(knd.blp.int,
                   paste0("blp_df_01_", ID, "_", year),
                   genBLPDataStructure01_aggregate(ID, year, db = knd)
      )
    }
  }
  
}

#Step 2: Compute sales by brand, brand and quarter, brand and market,
#total sales for year, and total sales by market. Filter out brands who
#have less than 1% market share compared to total overall sales across
#all markets for entire year.---------------------------------------------------
for(ID in c(prodID$tcigte, prodID$ecigte)){
  
  if(ID == prodID$tcigte){
    for(year in 2006:2020){
      blp01 <- genBLPDataStructure02_sumSales(ID, year, 0.01, knd.blp.int)
      dbWriteTable(knd.blp.int,
                   paste0("blp_df_02_", ID, "_", year),
                   blp01, overwrite = TRUE
      )
      
      #Inside Brands Annual Sales
      blp01 %>%
        group_by(brand_code_uc) %>%
        select(productID, year, brand_code_uc, annual_brand_sales) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/sales/inside_brand_sales/", 
                 "inside_brand_sales_", ID, "_", year, ".tsv")
        )
      
      #Inside Brands Quarter Sales
      blp01 %>%
        group_by(brand_code_uc, quarter) %>%
        select(productID, year, brand_code_uc, quarter, brand_quarter_sales) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/sales/inside_brand_quarter_sales",
                 "/inside_brand_quarter_sales_", ID, "_", year, ".tsv")
        )
      
      #Inside Brands Market Sales
      blp01 %>%
        group_by(brand_code_uc, quarter, fips_state_code, fips_county_code) %>%
        select(productID, year, brand_code_uc, quarter, fips_state_code,
               fips_county_code, brand_mkt_sales) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/sales/inside_brand_market_sales/",
                 "inside_brand_market_sales_", ID, "_", year, ".tsv")
        )
      
      #Total Market Sales
      blp01 %>%
        group_by(quarter, fips_state_code, fips_county_code) %>%
        select(productID, year, quarter, fips_state_code, fips_county_code,
               total_mkt_sales) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/sales/inside_market_sales/", 
                 "inside_market_sales_", ID, "_", year, ".tsv")
        )
      
      #Total Sales for Year for Product ID
      blp01 %>%
        ungroup() %>%
        select(productID, year, annual_all_sales) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/sales/total_all_sales/",
                 "total_all_sales_", ID, "_", year, ".tsv")
        )
      
 
      
           
      #Inside Brands Annual Volume
      blp01 %>%
        group_by(brand_code_uc) %>%
        select(productID, year, brand_code_uc, annual_brand_vol) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/volume/inside_brand_vol/", 
                 "inside_brand_vol_", ID, "_", year, ".tsv")
        )
      
      #Inside Brands Quarter Volume
      blp01 %>%
        group_by(brand_code_uc, quarter) %>%
        select(productID, year, brand_code_uc, quarter, brand_quarter_vol) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/volume/inside_brand_quarter_vol",
                 "/inside_brand_quarter_vol_", ID, "_", year, ".tsv")
        )
      
      #Inside Brands Market Volume
      blp01 %>%
        group_by(brand_code_uc, quarter, fips_state_code, fips_county_code) %>%
        select(productID, year, brand_code_uc, quarter, fips_state_code,
               fips_county_code, brand_mkt_vol) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/volume/inside_brand_market_vol/",
                 "inside_brand_market_vol_", ID, "_", year, ".tsv")
        )
      
      #Total Market Volume
      blp01 %>%
        group_by(quarter, fips_state_code, fips_county_code) %>%
        select(productID, year, quarter, fips_state_code, fips_county_code,
               total_mkt_vol) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/volume/inside_market_vol/", 
                 "inside_market_vol_", ID, "_", year, ".tsv")
        )
      
      #Total Volume for Year for Product ID
      blp01 %>%
        ungroup() %>%
        select(productID, year, annual_all_vol) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/volume/total_all_vol/",
                 "total_all_vol_", ID, "_", year, ".tsv")
        )
    }
  } else if(ID == prodID$ecigte){
    for(year in 2013:2020){
      blp01 <- genBLPDataStructure02_sumSales(ID, year, 0.01, knd.blp.int)
      dbWriteTable(knd.blp.int,
                   paste0("blp_df_02_", ID, "_", year),
                   blp01, overwrite = TRUE
      )
      
      #Inside Brands Annual Sales
      blp01 %>%
        group_by(brand_code_uc) %>%
        select(productID, year, brand_code_uc, annual_brand_sales) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/sales/inside_brand_sales/", 
                 "inside_brand_sales_", ID, "_", year, ".tsv")
        )
      
      #Inside Brands Quarter Sales
      blp01 %>%
        group_by(brand_code_uc, quarter) %>%
        select(productID, year, brand_code_uc, quarter, brand_quarter_sales) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/sales/inside_brand_quarter_sales",
                 "/inside_brand_quarter_sales_", ID, "_", year, ".tsv")
        )
      
      #Inside Brands Market Sales
      blp01 %>%
        group_by(brand_code_uc, quarter, fips_state_code, fips_county_code) %>%
        select(productID, year, brand_code_uc, quarter, fips_state_code,
               fips_county_code, brand_mkt_sales) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/sales/inside_brand_market_sales/",
                 "inside_brand_market_sales_", ID, "_", year, ".tsv")
        )
      
      #Inside Goods Market Sales
      blp01 %>%
        group_by(quarter, fips_state_code, fips_county_code) %>%
        select(productID, year, quarter, fips_state_code, fips_county_code,
               total_mkt_sales) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/sales/inside_market_sales/", 
                 "inside_market_sales_", ID, "_", year, ".tsv")
        )
      
      #Total Sales for Year for Product ID
      blp01 %>%
        ungroup() %>%
        select(productID, year, annual_all_sales) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/sales/total_all_sales/",
                 "total_all_sales_", ID, "_", year, ".tsv")
        )
      
      
      
      
      #Inside Brands Annual Volume
      blp01 %>%
        group_by(brand_code_uc) %>%
        select(productID, year, brand_code_uc, annual_brand_vol) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/volume/inside_brand_vol/", 
                 "inside_brand_vol_", ID, "_", year, ".tsv")
        )
      
      #Inside Brands Quarter Volume
      blp01 %>%
        group_by(brand_code_uc, quarter) %>%
        select(productID, year, brand_code_uc, quarter, brand_quarter_vol) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/volume/inside_brand_quarter_vol",
                 "/inside_brand_quarter_vol_", ID, "_", year, ".tsv")
        )
      
      #Inside Brands Market Volume
      blp01 %>%
        group_by(brand_code_uc, quarter, fips_state_code, fips_county_code) %>%
        select(productID, year, brand_code_uc, quarter, fips_state_code,
               fips_county_code, brand_mkt_vol) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/volume/inside_brand_market_vol/",
                 "inside_brand_market_vol_", ID, "_", year, ".tsv")
        )
      
      #Inside Goods Market Volume
      blp01 %>%
        group_by(quarter, fips_state_code, fips_county_code) %>%
        select(productID, year, quarter, fips_state_code, fips_county_code,
               total_mkt_vol) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/volume/inside_market_vol/", 
                 "inside_market_vol_", ID, "_", year, ".tsv")
        )
      
      #Total Volume for Year for Product ID
      blp01 %>%
        ungroup() %>%
        select(productID, year, annual_all_vol) %>%
        distinct() %>%
        write_tsv(
          paste0(direcs$dat.der, "nielsen/blp/volume/total_all_vol/",
                 "total_all_vol_", ID, "_", year, ".tsv")
        )
    }
  }
  
}

#Step 3: Compute Various Market Size Metrics------------------------------------
for(type in c("inside_brand_market",
              "inside_brand_quarter",
              "inside_brand",
              "inside_market",
              "total_all")
){
  
  genBLPDataStructure03_combAgg(type)
  
}

#Step 4: Compute Overall Market Size Metrics to Assign to All Time Periods------
genBLPDataStructure04_compSize()

#Step 5: Add Market Size Metrics to DataBase------------------------------------
genBLPDataStructure05_addMktSizetoDB(db = knd.blp.int)

#Step 6 Aggregate to Brand Level------------------------------------------------
for(productID in c(7460, 7467)){
  for(year in 2006:2020){
    if(year <= 2012 & productID == 7467){
    } else {
      genBLPDataStructure06_aggToBrand(db = knd.blp.int, productID = productID,
                                       year = year)
    }
  }
}

#Step 7 Merge in Mkt Size-------------------------------------------------------
for(productID in c(7460, 7467)){
  for(year in 2006:2020){
    if(year <= 2012 & productID == 7467){
    } else {
      genBLPDataStructure07_mergeInMktSize(db = knd.blp.int, productID = productID,
                                       year = year)
    }
  }
}



#-Step 8 Union Tables-----------------------------------------------------------
tables <- dbListTables(knd.blp.int)[which(grepl("04", dbListTables(knd.blp.int)) == TRUE)]
for(t in 1:length(tables)){
  
  if(t == 1){
    df <- dbReadTable(knd.blp.int, tables[t])
  } else {
    temp <- dbReadTable(knd.blp.int, tables[t])
    df <- rbind(df, temp)
  }
  
  print(t)
  
}

#-Step 9 Write Dataset to Disk--------------------------------------------------
dbWriteTable(knd.blp.int, "blp_df_05_all", df)
df <- dbReadTable(knd.blp.int, "blp_df_05_all")
write_tsv(df, paste0(direcs$hom.dat, "main/blp_knd_main.tsv"))

#-Step 10 Product Characteristics and Instruments-------------------------------
genBLPDataStructure08_characteristics()

#-Step 11 Shares----------------------------------------------------------------
read_tsv(paste0(direcs$hom.dat, "main/blp_knd_main.tsv")) %>%
  merge(read_tsv(paste0(direcs$dat.ext, "fred/cpi_qtr.tsv")), by = c("year", "quarter")) %>%
  mutate(brand_mkt_sales_real = brand_mkt_sales/cpi_qtr*100) %>%
  group_by(year, quarter, fips_state_code, fips_county_code) %>%
  mutate(share_vol = brand_mkt_vol/market_size_vol,
         share_sal = brand_mkt_sales_real/market_size_real) %>%
  select(brand_code_uc, year, quarter, fips_state_code, fips_county_code, share_vol, share_sal) %>%
  dplyr::mutate(out_vol = 1 - sum(share_vol),
                out_sal = 1 - sum(share_sal)) %>%
  write_tsv(paste0(direcs$hom.dat, "main/blp_shares.tsv"))

read_tsv(paste0(direcs$hom.dat, "main/blp_knd_main.tsv")) %>%
  select(year, quarter, fips_state_code, fips_county_code, market_size_vol, market_size_real) %>%
  distinct() %>%
  write_tsv(paste0(direcs$hom.dat, "main/blp_market_size.tsv"))

read_tsv(paste0(direcs$hom.dat, "main/blp_knd_main.tsv")) %>% 
  select(brand_code_uc, year, quarter, fips_state_code, fips_county_code, 
         price_wvol, price_wsale, brand_mkt_vol, brand_mkt_sales) %>%
    write_tsv(paste0(direcs$hom.dat, "main/blp_movement.tsv"))

read_tsv(paste0(direcs$hom.dat, "main/blp_knd_main.tsv")) %>%
  select(brand_code_uc, year, quarter, fips_state_code, fips_county_code,
         feature_wvol, feature_wsale, display_wvol, display_wsale) %>%
  write_tsv(paste0(direcs$hom.dat, "main/blp_feature.tsv"))

#-Market Identifier-------------------------------------------------------------

read_tsv(paste0(direcs$hom.dat, "main/blp_movement.tsv")) %>%
  select(year, quarter, fips_state_code, fips_county_code) %>%
  distinct() %>%
  arrange(fips_state_code, fips_county_code, year, quarter) %>%
  dplyr::mutate(marketid = row_number()) %>%
  write_tsv(paste0(direcs$hom.dat, "main/blp_markets.tsv"))
  


#-Step 12 Create Master Database------------------------------------------------

dbWriteTable(main, "movement",
             read_tsv(
               paste0(direcs$hom.dat, "main/blp_movement.tsv")
             ) %>%
               inner_join(read_tsv(paste0(direcs$hom.dat, "main/blp_markets.tsv")),
                     by = c("year", "quarter", "fips_state_code", "fips_county_code")) %>%
               select(-year, -quarter, -fips_state_code, -fips_county_code) %>%
               select(marketid, brand_code_uc, price_wvol, price_wsale,
                       brand_mkt_vol, brand_mkt_sales) %>%
               arrange(marketid)
)

dbWriteTable(
  main, "X_Rel",
  read_tsv(
    paste0(direcs$hom.dat, "main/blp_character.tsv")
  ) %>%
    inner_join(read_tsv(paste0(direcs$hom.dat, "main/blp_markets.tsv")),
               by = c("year", "quarter", "fips_state_code", "fips_county_code")) %>%
    select(-year, -quarter, -fips_state_code, -fips_county_code) %>%
    arrange(marketid)
)

dbWriteTable(
  main, "IV_Rel",
  read_tsv(
    paste0(direcs$hom.dat, "main/blp_ivs.tsv")
  ) %>%
    inner_join(read_tsv(paste0(direcs$hom.dat, "main/blp_markets.tsv")),
               by = c("year", "quarter", "fips_state_code", "fips_county_code")) %>%
    select(-year, -quarter, -fips_state_code, -fips_county_code) %>%
    arrange(marketid)
)

dbWriteTable(
  main, "shares",
  read_tsv(
    paste0(direcs$hom.dat, "main/blp_shares.tsv")
  ) %>%
    inner_join(read_tsv(paste0(direcs$hom.dat, "main/blp_markets.tsv")),
               by = c("year", "quarter", "fips_state_code", "fips_county_code")) %>%
    select(-year, -quarter, -fips_state_code, -fips_county_code) %>%
    arrange(marketid)
)

dbWriteTable(
  main, "feature",
  read_tsv(
    paste0(direcs$hom.dat, "main/blp_feature.tsv")
  ) %>%
    inner_join(read_tsv(paste0(direcs$hom.dat, "main/blp_markets.tsv")),
               by = c("year", "quarter", "fips_state_code", "fips_county_code")) %>%
    select(-year, -quarter, -fips_state_code, -fips_county_code) %>%
    arrange(marketid)
)

dbWriteTable(
  main, "market_size",
  read_tsv(
    paste0(direcs$hom.dat, "main/blp_market_size.tsv")
  ) %>%
    inner_join(read_tsv(paste0(direcs$hom.dat, "main/blp_markets.tsv")),
               by = c("year", "quarter", "fips_state_code", "fips_county_code")) %>%
    select(-year, -quarter, -fips_state_code, -fips_county_code) %>%
    arrange(marketid)
)

dbWriteTable(
  main, "markets",
  read_tsv(paste0(direcs$hom.dat, "main/blp_markets.tsv"))
)

dbWriteTable(
  main, "brands",
  read_tsv(paste0(direcs$hom.dat, "main/blp_brands.tsv"))
)

#12 Transform Characteristics into counts (optional)----------------------------
transCharacter(main, "X_Rel", "X_Count")
transIV(main, "X_Count", "IV_Rel", "IV_Count")

