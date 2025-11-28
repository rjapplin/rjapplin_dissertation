getTobTaxData <- function(dataurl, type){
  
  downloadTobTaxData <- function(dataurl){
    
    read.csv(
      url(dataurl), header = TRUE
    )
    
  }
  
  filePath <- paste0(direcs$dat.ext, "cdc/cdc_", type, "_tob_taxes_master.tsv")
  
  #If file already exists, don't need to do anything here. If not,
  #download it and save it to cdc data directory
  if(file.exists(filePath)){
    return(read_tsv(filePath))
  } else {
  
    write_tsv(
      downloadTobTaxData(dataurl),
      paste0(direcs$dat.ext, "cdc/cdc_", type, "_tob_taxes_master.tsv")
    )
    read_tsv(filePath)
    
  }
  
}
