################################################################################
#' @export local_settings
#'
#' @title Funtion to set-up the local settings
#' 
#' @description This function is used by the import-App to check whether the
#' local settings are already available or if they have to be set-up again. The 
#' settings are saved in the file "localSettings.rda"
#'
#' @param metService string. Abbreviation of the meteorological service in which
#' the app is going to be used. If emtpy the user will be asked to enter this 
#' parameter interactively. The options available are: "INAMET", "DMS", "ZMD" 
#' and "others"
#' 
#' @param language string. The language in which the user wants to run the App.
#' If emtpy the user will be asked to enter this parameter interactively. 
#' The options available are: "english", "german", "portuguese"
#' 
#' @examples 
#' local_settings(metService = 'INAMET', language = 'portuguese')
#' 
################################################################################
local_settings <- function(metService = NULL, language = NULL){
  
  # CHECK IF SETTINGS ALREADY AVAILABLE
  file.name <- "localSettings.rda"
  if (file.exists(file.name)){
    load(file.name)
    cat("log: Local settings already available\n")
    return(localSettings)
  }
  
  ##############################################################################
  # GET MET SERVICE AND LANGUAGE
  metService <- set_local_metService()
  language <- set_local_language()
  
  ##############################################################################
  # SAVE LOCAL SETTINGS
  localSettings <- data.frame(metService = tolower(metService), 
                              language = tolower(language))
  
  # Save settings
  save(localSettings, file = "localSettings.rda")
}