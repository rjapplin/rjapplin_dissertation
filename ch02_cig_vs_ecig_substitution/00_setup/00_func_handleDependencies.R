#This script contains functions for installing and loading dependencies.
#It should not be altered.



loadDepends <- function(pkgs){
  
  checkIfInstalled <- function(pkgs){
    
    newPkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
    
    if(length(newPkgs)){
      cat("The following required packages are not installed: \n\n ")
      print(data.frame(Package = newPkgs))
      readline(prompt = "Install all required packages? (y/n) ")
    } else {
      return("all_installed")
    }
    
  }
  
  confirmLoading <- function(pkgs){
    
    cat("The following required packages will be loaded: \n\n ")
    print(data.frame(Package = pkgs))
    readline(prompt = "Ok to load all required packages? (y/n) ")
    
  }
  
  
  install <- checkIfInstalled(pkgs)
  if(install == "n"){
    stop("Please manually install the required packages.")
  } else if(install == "y"){
    install.packages(pkgs[!(pkgs %in% installed.packages()[, "Package"])],
                     dependencies = TRUE)
  } else if(install == "all_installed"){
    
  }
  
  confirm <- confirmLoading(pkgs)
  if(confirm == "n"){
    stop("Please manually load the required packages.")
  } else if(confirm == "y"){
    sapply(pkgs, require, character.only = TRUE)
  }
  
}
