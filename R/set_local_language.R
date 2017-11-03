################################################################################
#' @export set_local_language
#'
#' @title Sets the language in under which the App should run
#' 
#' @description This function set up the language in which the App will be run. 
#' The default options available currently are: "English", "German" and 
#' "Portuguese".
#' 
#' @examples 
#' set_local_language()
#'
################################################################################
set_local_language <- function(){
  # Default languages
  languages <- tolower(c("english", "portuguese"))
  
  # Ask the user in which institution should the import-App be installed
  cat("Please select one of the following languages:\n")
  print(languages)
  # Check if the session is interactive or not 
  # (interactive means that the script is run in the R-Command window)
  if (interactive() == T){
    language <- readline() 
  }else{
    language <- readLines(con = "stdin", 1)
  }
  
  # Ask over again until the user select one of the options available
  while(tolower(language) %in% tolower(languages) == F){
    cat("Entry not valid. Please select one of the following languages:\n")
    print(languages)
    if (interactive() == T){
      language <- readline() 
    }else{
      language <- readLines(con = "stdin", 1)
    }
  }
  return(language)
}