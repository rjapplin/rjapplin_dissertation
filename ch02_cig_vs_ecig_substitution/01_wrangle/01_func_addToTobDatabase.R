#This functions takes as input a directory - direc - and
#file type (typically tsv if a Nielsen data file) and adds it the
#tobacco database. All files added to the database must be given
#a prefix. For RMS data files, this prefix should be rms_ and
#for HMS data files, this prefix should be hms_. The function is designed
#so as to determine if a file is already in the database. If it is,
#the function will ignore it. Whether or not a file is determined
#to be new or not is dependent on file name. Therefore, naming of
#raw data files should not be altered - as the name of the file in the
#database is a function of the name of the raw data file.

addToTobDatabase <- function(direc, type, prefix, db){
  
  getFilePathsAndNames <- function(direc, type){
    
    list(
      paths = dir(direc, recursive = TRUE, full.names = TRUE,
                  pattern = paste0("\\.", type, "$")),
      names = dir(direc, recursive = TRUE, full.names = FALSE,
                  pattern = paste0("\\.", type, "$"))
    )
    
  }
  cleanFileNames <- function(direc.files, type, prefix){
    
    direc.files$names <- gsub(".*/", "", direc.files$names)
    direc.files$names <- gsub(paste0(".", type), "", direc.files$names)
    direc.files$names <- paste0(prefix, direc.files$names)
    
    return(direc.files$names)
    
  }
  detNewFiles <- function(direc.files){
    
    filePaths <- c(direc.files$paths)
    fileNames <- c(direc.files$names)
    fileDF <- data.frame(filePaths = filePaths, fileNames = fileNames) %>%
      subset(!(fileNames %in% dbListTables(tob)))
    return(fileDF)
    
  }
  readToDataBase <- function(newFiles.path, newFiles.name, noFiles, db){
    
    mapply(
      function(tsvfile, tablename, i){
        
        if(!(tablename %in% c("rms_brand_variations", "rms_products",
                              "hms_brand_variations", "hms_products"))){
          dbWriteTable(db,
                       tablename,
                       tsvfile,
                       overwrite = TRUE,
                       row.names = FALSE, 
                       header = TRUE, 
                       sep = "\t")
        } else if(tablename %in% c("rms_brand_variations", "rms_products",
                                   "hms_brand_variations", "hms_products")){
          
          temp <- read_tsv(tsvfile)
          dbWriteTable(tob,
                       tablename,
                       temp,
                       overwrite = TRUE)
          
        }
        print(i/noFiles) #Print "progress" percent
      },
      newFiles.path, newFiles.name, 1:noFiles
    )
    
  }
  
  direc.files <- getFilePathsAndNames(direc = direc, type = type)
  direc.files$names <- cleanFileNames(direc.files, type = type, prefix = prefix)
  newFilesToAdd <- detNewFiles(direc.files)
  noFiles <- dim(newFilesToAdd)[1]
  newFiles.path <- newFilesToAdd$filePaths
  newFiles.name <- newFilesToAdd$fileNames
  
  readToDataBase(newFiles.path, newFiles.name, noFiles, db)
  
}



