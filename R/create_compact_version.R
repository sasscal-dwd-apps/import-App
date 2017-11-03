################################################################################
#' @export create_compact_version
#'
#' @title Create import-App folder to facilitate installation
#' @description Script to create a new folder containing the import-App in a way
#' that it reduces its size. This new folder can be then zipped manually and 
#' use as the "installation" file of the App.
#' 
#' If this script is run, the user will have to choose between "compiling" the 
#' App to minimize its size ('small') or compiling the App but keeping all 
#' required packages and programs ('large').
#' 
#' The 'small' version should have around 11 Mb once it is zipped
#' The 'large' version should have around 120 Mb once it is zipped
#' 
#' @param appDir string. Path where the import-App is stored. Typically under 
#' "import_standalone". If empty, the user will be ask to select the path
#' interactively.
#' 
#' @param opt string. Which type of 'compaction' should be carried out.
#' There are two options available: 'small' or 'large'. If empty, 
#' the user will be asked to enter the type interactively.
#'
#' @examples 
#' appDir <- 'C:/Users/username/Documents/import_standalone/'
#' create_compact_version(appDir, 'small')
#'
################################################################################
create_compact_version <- function(appDir = NULL, opt = NULL){
  
  ##############################################################################
  #
  #                               SET import_APP LOCATION
  #
  ##############################################################################
  appDir2 <- set_app_location(appDir)
  if (!is.null(appDir2)){
    appDir <- appDir2
    
    ############################################################################
    #
    #                              SET TYPE OF COMPACTION
    #
    ############################################################################
    if (is.null(opt)){
      # Options available
      opts <- c("large", "small")
      
      # Ask the user which kind of compact version wants to get
      print("Options available:")
      print(opts)
      cat("Please select one of the options: ")
      
      # Check if the session is interactive or not
      # (interactive means that the script is run in the R-Command window)
      if (interactive() == T){
        opt <- readline() 
      }else{
        opt = readLines(con = "stdin", 1)
      }
      
      # Ask over again until the user selects one of the options available
      while(tolower(opt) %in% tolower(opts) == F){
        print(paste0("Option not included in the list (", 
                     paste(opts, collapse = ", "),")"))
        cat("Please select one of the list: ")
        if (interactive() == T){
          opt <- readline() 
        }else{
          opt = readLines(con = "stdin", 1)
        }
      }
    }  
    
    ############################################################################
    #
    #                            CREATE COMPACT VERSION
    #
    ############################################################################
    # Run the "compact" function according to the option selected
    tt <- paste0("log: Compact option selected: '", opt, "'")
    print(tt)
    do.call(paste0("compact_", opt), args = list(appDir))
  }
}