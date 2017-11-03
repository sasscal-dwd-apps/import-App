################################################################################
#' @export set_app_location
#'
#' @title Select the location of the import_App
#' @description This function checks where the import_App is located (in 
#' folder import_standalone)
#' 
#' @param appDir string. Path where the import-App is stored. Typically under 
#' "import_standalone". If empty, the user will be ask to select the path
#' interactively.
#'
#' @examples 
#' appDir <- 'C:/Users/username/Documents/import_standalone/'
#' set_app_location(appDir)
#'
################################################################################
set_app_location <- function(appDir = NULL){
  while (!exists("appDir") | is.null(appDir)){
    tt <- paste0("log: Please, select the folder 'import_standalone', ",
                 "where import_App is located")
    print(tt)
    appDir <- choose.dir(caption = "Select folder")
  }
  if (is.na(appDir) | grepl("\\<import_standalone\\>", appDir) == F){
    tt <- paste0("log: No import_App location selected. ",
                 "A compact version of the App cannot be created") 
    print(tt)
    appDir <- NULL
  }else{
    tt <- paste0("log: ", "'", appDir, "' selected")
    print(tt)
    return(appDir)
  }
  
}