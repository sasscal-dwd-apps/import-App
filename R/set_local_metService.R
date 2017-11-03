################################################################################
#' @export set_local_metService
#'
#' @title Sets the met service in under which the App should run
#' 
#' @description This function is used to select the met service in which the
#' App is going to run. The default options available currently are: 
#' "INAMET", "DMS", "ZMD" and "others"
#'
#' @examples 
#' set_local_metService()
#'
################################################################################
set_local_metService <- function(){
  # Default met services
  metServices <- tolower(c("INAMET", "DMS", "ZMD", "others"))
  
  # Ask the user in which institution should the import-App be installed
  cat("Please select one of the following institutions:\n")
  print(metServices)
  
  # Check if the session is interactive or not 
  # (interactive means that the script is run in the R-Command window)
  if (interactive() == T){
    metService <- readline() 
  }else{
    metService = readLines(con = "stdin", 1)
  }
  
  # Ask over again until the user select one of the options available
  while(tolower(metService) %in% tolower(metServices) == F){
    cat("Entry not valid. Please select one of the following institutions:\n")
    print(metServices)
    if (interactive() == T){
      metService <- readline() 
    }else{
      metService = readLines(con = "stdin", 1)
    }
  }
  return(metService)
}