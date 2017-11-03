##############################################################################
#' @export master
#'
#' @title Main script to run the import-App
#'
#' @description It runs the import-App within the import_standalone folder. It 
#' checks the availability of the R-packages required and installs them if
#' necessary. It also checks whether specific software is already available and,
#' if not, it will be installed.
#'
#'
#' @details This script is not an R-function and, therefore, does not require
#' any input parameters. 
#' It is recommended to run it through the batch file "import.bat". If
#' it is desired to run the App within an R-Session, please refer to 
#' the "Example" section.
#'
#' @examples 
#' source('master.R')
#'
#' @author Rafael Posada and Jens Riede (SASSCAL/DWD), November 2016

################################################################################
# 
#                               SET DEFAULTS
#
################################################################################
rversion_prev <- R.Version()
rversion <- paste0(rversion_prev$major, ".", rversion_prev$minor)
pkgs.type <- "win.binary"

################################################################################
#
#                                 SET PATH
#
################################################################################
setwd(file.path(dirname(parent.frame(2)$ofile)))

################################################################################
#
#                           SOURCE R-SCRIPTS
#
################################################################################
# Source the R-Scripts placed in the App
dirR <- file.path(".", "R")
pathnames <- list.files(pattern="[.]R$", path=dirR, full.names=TRUE)
sapply(pathnames, FUN=source)

################################################################################
#
#                               LOCAL SETTINGS
#
################################################################################
local_settings()

################################################################################
#
#                                 WORKING PATH
#
################################################################################
appDir_prev <<- file.path(dirname(parent.frame(2)$ofile), "import_App")
setwd(appDir_prev)
appDir <<- getwd()
print(paste0("Current Path: ",appDir))

################################################################################
#
#                           LIBRARIES PATH
#
################################################################################
# Get the libPaths available from where the libraries can be called
libPath <- path.expand(file.path(appDir, "www", "libraries", rversion))
print(paste("Library Path:", libPath))

# Create directory where libraries are to be saved
if (exists("libPath")){
  dir.create(libPath, showWarnings = FALSE, recursive = T)
  .libPaths(c(path.expand(libPath)))
}else{
  .libPaths(c(.libPaths()))
}

#############################################################################
#
#                         DOWNLOAD & INSTALL PACKAGES
#
#############################################################################
packs <- c("backports", #
           "DBI", #
           "digest", #
           "evaluate", #
           "installr", # functions for software installation and updating 
           "jsonlite", # High Performance JSON Parser and Generator
           "knitr", #         
           "gdata", # Various R Programming Tools for Data Manipulation
           "gtools", #
           "highr", #
           "htmltools", #
           "htmlwidgets", # 
           "httpuv", #
           "magrittr", #
           "mime", #
           "miniUI", #
           "R6", #
           "RODBC",
           "RMySQL",
           "Rcpp", #
           "rhandsontable", # Interface to the 'Handsontable.js' Library
           "rJava", #
           "rmarkdown", # Dynamic Documents for R
           "rprojroot", #
           "shiny", # Web Application Framework
           "shinyBS", # Twitter Bootstrap Components for Shiny
           "shinyjs",
           "stringi", #
           "stringr", #          
           "uuid", # Tools for generating and handling of UUIDs
           "XLConnect", #
           "XLConnectJars", # 
           "xtable", #  Excel Connector for R
           "yaml" #
) 

# Check if packages are already installed
directory <- path.expand(file.path(appDir, "www", "R_pkgs", pkgs.type))
download_and_install_packages(packs = packs, packs.loc = directory,
                              lib.loc = libPath, type = pkgs.type)

#############################################################################
# 5) Run the app (when all packages are installed)
print("The App is ready to run...")
# Run the App
shiny::runApp(file.path(appDir), launch.browser=T,
              host = "0.0.0.0", port = 3181)
