#This scripts setups the project environment. Namely, it installs and loads
#required packages ("dependencies") and sets some useful and commonly used
#"global values." These globals should be thought of as global constants: they
#are NOT overwritten throughout the entire project and they SHOULD NOT be
#overwritten throughout the entire project.

#Packages
if(exists("loadDepends") == FALSE){
  source("code/00_setup/00_func_handleDependencies.R")
}


loadDepends(read.table("code/deps.tsv", header = TRUE, sep = "\t")[[1]])

#Path Shortcuts
# direcs <- list()
# home <- here::here() #The top-most directory of the project

# direcs$home <- home
# direcs$hom.dat <- paste0(home, "/data/") #Data Directory
# direcs$dat.ext <- paste0(direcs$hom.dat, "external/") #External Data Directory
# direcs$dat.der <- paste0(direcs$hom.dat, "derived/") #Derived Data Directory
# direcs$ext.niq <- paste0(direcs$dat.ext, "nielsen/") #Nielsen Data Directory
# direcs$niq.rms <- paste0(direcs$ext.niq, "RMS/") #Scanner Data Directory
# direcs$niq.hms <- paste0(direcs$ext.niq, "HMS/") #Panel Data Directory
# direcs$der.niq <- paste0(direcs$dat.der, "nielsen/") #Derived Data from NIQ
# direcs$niq.agg <- paste0(direcs$der.niq, "aggregate/") #Aggregate Data from NIQ
# direcs$der.sum <- paste0(direcs$dat.der, "sumner_replication/") #Sumner Rep Data
# direcs$hom.pap <- paste0(home, "/paper/") #Paper Files
# direcs$pap.fig <- paste0(direcs$hom.pap, "figures/") #Figures

#Product Type IDs
prodID <- list()
prodID$tcigte = 7467 #ID for Traditional Cigarette
#prodID$tcigar = 7462 #ID for Traditional Cigar
#prodID$smkacc = 7463 #ID for Smoking Accessories
#prodID$tobsmk = 7464 #ID for Smoking Tobacco
#prodID$antsmk = 7465 #ID for Anti-Smoking Products
#prodID$tobchw = 7466 #ID for Chewing Tobacco
prodID$ecigte = 7460#ID for Electronic Cigarette
#prodID$ecigar = 7468 #ID for Electronic Cigars
#prodID$eother = 7469 #ID for Remaining Electronic Smoking Accessories

#Color Palette for Figures
color_pal <- list()
color_pal$wildcat_blue <- "#0033A0"
color_pal$bluegrass <- "#1E8AFF"
color_pal$sky <- "#B1C9E8"
color_pal$midnight <- "#1B365D"
color_pal$goldenrod <- "#FFDC00"
color_pal$sunset <- "#FFA360"
color_pal$river_green <- "#4CBCC0"
color_pal$cool_neutral <- "#DCDDDE"
color_pal$warm_neutral <- "#D6D2C4"

#DataBase Location: Recommended to Store Both in Project Directory and
#on an External Drive for Backup Purposes---------------------------------------
platform = Sys.info()["sysname"]
if(platform == "Windows"){
  db_loc <- "D:/applin/nielsen/"
} else if(platform == "Darwin"){
  db_loc <- "/Volumes/UKYECON/applin/nielsen"
}

# knd <- dbConnect(RSQLite::SQLite(), 
#                  dbname = paste0(db_loc, "/knd.sqlite")
# )
# 
# knd.pre18Sample <- dbConnect(RSQLite::SQLite(),
#                              dbname = paste0(db_loc, "/knd_pre18Sample.sqlite"))
# 
# knd.analysis <- dbConnect(RSQLite::SQLite(),
#                           dbname = paste0(db_loc, "/knd_analysis.sqlite"))
# 
# knd.blp.int <- dbConnect(RSQLite::SQLite(),
#                          dbname = paste0(db_loc, "/knd_blp_int.sqlite"))
# 
main <- dbConnect(RSQLite::SQLite(),
                 dbname = paste0(direcs$hom.dat, "main/blp.sqlite"
                 )
)
results2 <- dbConnect(RSQLite::SQLite(),
                      dbname = paste0(direcs$hom.dat, "main/results2.sqlite"
                      )
)

results <- dbConnect(RSQLite::SQLite(),
                      dbname = paste0(direcs$hom.dat, "main/results.sqlite"
                      )
)
                       
#Sorting for Plotting
brandSort <- (dbReadTable(main, "BRANDS") %>%
                select(brand_descr, category) %>%
                dplyr::rename(BRAND = brand_descr) %>%
                mutate(category = ifelse(category == 7460, 1, 0)) %>%
                add_row(BRAND = "OUTSIDE", category = -1) %>%
                arrange(category, BRAND))$BRAND

