getCPI <- function(agg = NULL){

  downloadCPI <- function(){
    
    read.csv(
      url(
        read_tsv(
          paste0(direcs$home, "/code/cpi_link.tsv"),
          col_names = FALSE
        )[[1,1]]
      )
    )
    
  }
  convertMonthToQuarter <- function(month){
    
    ifelse(month %in% 1:3, 1,
           ifelse(month %in% 4:6, 2,
                  ifelse(month %in% 7:9, 3, 4)
           )
    )
    
  }  
  
  if(file.exists(paste0(direcs$dat.ext, "fred/cpi.tsv")) == FALSE){
    cpi <- downloadCPI()
    names(cpi) <- c("date", "cpi")
    cpi$year <- substr(cpi$date, 1, 4) %>% as.numeric()
    cpi$month <- substr(cpi$date, 6, 7) %>% as.numeric()
    cpi$quarter <- convertMonthToQuarter(cpi$month)
    
    cpi <- cpi %>%
      dplyr::group_by(year, quarter) %>%
      dplyr::mutate(cpi_qtr = mean(cpi)) %>%
      ungroup() %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(cpi_ann = mean(cpi)) %>%
      ungroup()
    
    cpi <- cpi %>%
      dplyr::filter(year %in% 2005:2022)
    cpi$date <- NULL
    
    cpi_ann <- cpi %>%
      mutate(month = NULL,
             cpi = NULL,
             quarter = NULL,
             cpi_qtr = NULL) %>%
      distinct()
    
    cpi_qtr <- cpi %>%
      mutate(month = NULL,
             cpi = NULL,
             cpi_ann = NULL) %>%
      distinct()
    
    write_tsv(
      cpi, paste0(direcs$dat.ext, "fred/cpi_master.tsv")
    )
    write_tsv(
      cpi_ann, paste0(direcs$dat.ext, "fred/cpi_ann.tsv")
    )
    write_tsv(
      cpi_qtr, paste0(direcs$dat.ext, "fred/cpi_qtr.tsv")
    )
    
    cpifile <- agg
    cpifile <- ifelse(is.null(cpifile), "master", cpifile)
    return(
      read_tsv(
        paste0(direcs$dat.ext, "fred/cpi_", cpifile, ".tsv")
      )
    )
                  
  } else {
   
    cpifile <- agg
    cpifile <- ifelse(is.null(cpifile), "master", cpifile)
    return(
      read_tsv(
        paste0(direcs$dat.ext, "fred/cpi_", cpifile, ".tsv")
      )
    )
    
  }
  
}
