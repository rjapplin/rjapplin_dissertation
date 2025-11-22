mapTypeToID <- function(type){
  
  id <- ifelse(type == "Cigar", prodID$tcigar, "")
  id <- ifelse(type == "Cigarette", prodID$tcigte, id)
  id <- ifelse(type == "Little Cigar", prodID$tcigar, id)
  id <- ifelse(type == "Pipe Tobacco", prodID$tobsmk, id)
  id <- ifelse(type == "Roll-Your-Own Tobacco", prodID$tobsmk, id)
  id <- ifelse(type == "Chewing Tobacco", prodID$tobchw, id)
  id <- ifelse(type == "Dissolvable Tobacco", prodID$tobchw, id)
  id <- ifelse(type == "Dry Snuff Tobacco", prodID$tobchw, id)
  id <- ifelse(type == "Moist Snuff Tobacco", prodID$tobchw, id)
  id <- ifelse(type == "Snus Tobacco", prodID$tobchw, id)
  
  return(id)
  
}

#Traditional Combustiable-------------------------------------------------------
combTobTaxes <- getTobTaxData(type = "comb") %>%
  select(LocationAbbr, Year, Quarter, MeasureDesc, ProvisionDesc,
         ProvisionAltValue) %>%
  group_by(MeasureDesc) %>%
  group_split()
names(combTobTaxes) <- sapply(combTobTaxes, function(x) unique(x$MeasureDesc))
combTobTaxes <- lapply(combTobTaxes, 
                       function(df){
                         df$temp <- mapTypeToID(df$MeasureDesc)
                         return(df)
                       }) %>%
  Reduce(rbind, .) %>%
  filter(Year >= 2006 & Year <= 2020) %>%
  dplyr::group_by(LocationAbbr, Year, MeasureDesc) %>%
  dplyr::mutate(annualTax = mean(ProvisionAltValue))
names(combTobTaxes) <- c("state", "year", "quarter", "product", "taxType",
                         "taxAmount", "productID", "avgAnnualTax")
write_tsv(combTobTaxes,
          paste0(direcs$dat.der, "taxes/comb_taxes.tsv"))

#Tradional Non-Combustiable-----------------------------------------------------
ncombTobTaxes <- getTobTaxData(type = "ncomb") %>%
  select(LocationAbbr, Year, Quarter, MeasureDesc, ProvisionDesc,
         ProvisionAltValue) %>%
  group_by(MeasureDesc) %>%
  group_split()
names(ncombTobTaxes) <- sapply(ncombTobTaxes, function(x) unique(x$MeasureDesc))
ncombTobTaxes <- lapply(ncombTobTaxes,
                        function(df){
                          df$productID <- mapTypeToID(df$MeasureDesc)
                          return(df)
                        }) %>%
  Reduce(rbind, .) %>%
  filter(Year >= 2006 & Year <= 2020) %>%
  dplyr::group_by(LocationAbbr, Year, MeasureDesc) %>%
  dplyr::mutate(annualTax = mean(ProvisionAltValue))
names(ncombTobTaxes) <- c("state", "year", "quarter", "product", "taxType",
                         "taxAmount", "productID", "avgAnnualTax")
write_tsv(ncombTobTaxes,
          paste0(direcs$dat.der, "taxes/ncomb_taxes.tsv"))

#Electronic---------------------------------------------------------------------
types <- c("20mu_tv", "35mu_tiv", "35mu_tv")
elec <- list()
for(type in 1:length(types)){
  elec[[type]] <- read_tsv(paste0(direcs$dat.ext,
                                  "ccnppt/ccnppt_ecig_taxes_",
                                  types[type], ".tsv")) %>%
    group_by(Jurisdiction) %>%
    group_split() %>%
    lapply(function(df){
      pivot_longer(df, cols = starts_with("Y20"))
    }) %>%
    lapply(function(df){
      names(df) <- c("LocationAbbr", "YearQuarter", "ProvisionAltValue")
      return(df)
    }) %>%
    Reduce(rbind, .) %>%
    mutate(Year = as.numeric(substr(YearQuarter, 2, 5)),
           Quarter = as.numeric(substr(YearQuarter, 7, 7)))  %>%
    ungroup() %>%
    mutate(ProvisionAltValue = as.numeric(ProvisionAltValue),
           productID = prodID$ecigte) %>%
    filter(Year >= 2013 & Year <= 2020) %>%
    dplyr::group_by(LocationAbbr, Year) %>%
    dplyr::mutate(annualTax = mean(ProvisionAltValue, na.rm = TRUE)) %>%
    mutate(YearQuarter = NULL)
  
    elec[[type]]$annualTax <- ifelse(is.nan(elec[[type]]$annualTax), yes = 0,
                                     no = elec[[type]]$annualTax)
    elec[[type]]$ProvisionAltValue <- ifelse(is.na(
      elec[[type]]$ProvisionAltValue),0, elec[[type]]$ProvisionAltValue)
    names(elec[[type]]) <- c("state", paste0("taxAmount_", types[type]), "year",
                                             "quarter", "productID", 
                                             paste0("avgAnnualTax_",
                                                    types[type])
    )
  
}

elec <- Reduce(function(x, y) merge(x, y, by = c("state", "year", "quarter",
                                          "productID")),
               elec) %>%
  mutate(product = "E-Cigarette",
         taxType = "Per Fluid Ml")

  write_tsv(elec,
    paste0(direcs$dat.der, "taxes/ecig_taxes.tsv")
  )

#Stack--------------------------------------------------------------------------
taxdf <- rbind.fill(combTobTaxes, ncombTobTaxes, elec) %>%
    left_join(
      read_tsv(
        paste0(
          direcs$dat.ext,
          "geographic/state_fips_codes.tsv"))[, c("abbr", "fips")],
      by = c("state" = "abbr")
    ) %>%
    dplyr::rename(state_abbr = state,
           state = fips)
  
taxdf_ann <- taxdf %>%
  dplyr::mutate(quarter = NULL,
                taxAmount = NULL,
                taxAmount_20mu_tv = NULL,
                taxAmount_35mu_tv = NULL,
                taxAmount_35mu_tiv = NULL) %>%
  distinct()

    write_tsv(taxdf,
              paste0(direcs$dat.der, "taxes/tob_taxes_qtr_master.tsv")
      )
    
    write_tsv(taxdf_ann,
              paste0(direcs$dat.der, "taxes/tob_taxes_ann_master.tsv")
              )

#Cleanup------------------------------------------------------------------------
rm(type, types, mapTypeToID, elec, combTobTaxes, ncombTobTaxes)
