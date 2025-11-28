wrangleIbisData <- function(){
  
  dfiles <- c("ecig_manuf_data.csv", "tobacco_manuf_data.csv",
              "tobacco_whole_data.csv")
  dflist <- list()
  for(i in 1:length(dfiles)){
    dflist[[i]] <- paste0(direcs$dat.ext, "ibisworld/", dfiles[i]) %>%
      read_csv(col_names = TRUE)
    
    if(dfiles[i] == "ecig_manuf_data.csv"){
      dflist[[i]] <- dflist[[i]] %>%
        mutate(ind = "ecig",
               type = "manuf")
    } else if(dfiles[i] == "tobacco_manuf_data.csv"){
      dflist[[i]] <- dflist[[i]] %>%
        mutate(ind = "tob",
               type = "manuf")
    } else if(dfiles[i] == "tobacco_whole_data.csv"){
      dflist[[i]] <- dflist[[i]] %>%
        mutate(ind = "tob",
               type = "whole")
    }
    
  }
  
  retdf <- Reduce(rbind, dflist)
  names(retdf) <- c("Year", "Revenue", "IVA", "Establishments",
                    "Enterprises", "Employment", "Exports", "Imports",
                    "Wages", "Demand", "Smokers", "Ind", "Type")
  
  retdf <- merge(retdf,
                 read_tsv(
                   paste0(direcs$dat.ext, "fred/cpi_ann.tsv")
                 ),
                 by.x = "Year", by.y = "year"
  )
                 
  
  
  return(retdf)
  
  
}
  