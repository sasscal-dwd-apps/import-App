################################################################################
#' @export download_and_install_programs
#'
#' @title Download and install programs
#'
#' @description The function allows the download and later installation of 
#' programs that are required by the ACD-App. Currently (at 10.07.2017),
#' the App requires "phantom", "pandoc", "miktex" and "java"
#' 
#' @param programs string. Program(s) to be downloaded and installed. 
#' The options are "phantom", "pandoc", "miktex" and "java". If empty, all the 
#' programs will be selected. 
#' 
#' @param programs.loc string. Path where the programs should be downloaded. If
#' empty the user will be asked to select a folder interactively.
#' 
#' @details The installation of the programs will be done once they are 
#' downloaded. 
#'
#' @examples 
#' download_and_install_programs(programs = c("miktex", "phantom"), 
#'                               programs.loc = "C:/Users/username/programs/")
#'
################################################################################

download_and_install_programs <- function(programs = NULL,
                                          programs.loc = NULL){
  ##############################################################################
  #
  #                                   LIBRARIES 
  #
  ##############################################################################
  suppressWarnings(library("installr"))
  
  ##############################################################################
  #
  #                                   DEFAULTS
  #
  ##############################################################################
  programs.default <- c("phantom", "pandoc", "miktex", "java")
  
  # Phantom Defaults
  phantom.version <- "phantomjs-2.1.1-windows"
  phantom.downloader <- "https://bitbucket.org/ariya/phantomjs/downloads/"
  phantom.exe <- "phantomjs.exe"
  
  # Pandoc Defaults
  pandoc.version <- "pandoc-1.19.2.1-windows"
  pandoc.downloader <- "https://github.com/jgm/pandoc/releases/download/1.19.2.1/"
  pandoc.exe <- paste0(pandoc.version, ".msi")
  
  
  # MikTeX Defaults
  miktex.version <- "basic-miktex-x64.exe"
  miktex.downloader <- "http://ctan.space-pro.be/tex-archive/systems/win32/miktex/setup/"
  miktex.exe <- paste0(miktex.version, ".exe")
  
  # packs - Packages to download
  if (!exists('programs') || is.null(programs)){
    return(print("log: No Programs to download and/or install"))
  }
  
  ##############################################################################
  #
  #                        PROGRAMS PATH
  #
  ##############################################################################
  
  # packs.loc - Set the location where package are to be downloaded
  while (!exists("programs.loc") || is.null(programs.loc)){
    print("log: Please, select one folder where programs are (or should be) located.")
    programs.loc <- choose.dir(caption = "Select folder")
  }
  if (is.na(programs.loc)){
    return(print(paste("log: No location for programs selected. ",
                       "Therefore the download cannot be completed")))
  }
  
  ##############################################################################
  #
  #                       CHECK IF THE PROGRAMS ARE AVAILABLE
  #
  ##############################################################################
  for (i in 1:length(programs)){
    program <- programs[i]
    program.path <- file.path(programs.loc, program)
    
    ############################################################################
    #
    #                       PHANTOM
    #
    ############################################################################
    if (tolower(program) == "phantom"){
      # Create directory if it does not exist
      dir.create(program.path, recursive = T, showWarnings = FALSE)
      
      # Check if the executable file is already available
      if (file.exists(file.path(program.path, phantom.exe))){
        print(paste0("log: '", phantom.exe, "' file already exists"))
      }else{
        # Download exe file from the Internet
        print(paste0("'", phantom.exe, "' file does not exist"))
        url <- phantom.downloader
        program.zip <- paste0(phantom.version, ".zip")
        program.zip.path <- file.path(program.path, program.zip)
        downloader::download(paste0(url,program.zip), program.zip.path)
        
        if (file.exists(program.zip.path)){
          print(paste0("log: ", program.zip, " downloaded. Unzipping..."))
          
          unzip(program.zip.path,
                files = file.path(phantom.version,"bin", phantom.exe),
                exdir = program.path, junkpaths = T)
          unlink(program.zip.path)
          print("log: Unzip finished")
        }else{
          print(paste0("log: ", program.zip, "could not be downloaded"))
        }
      }
    }
    
    ############################################################################
    #
    #                                 PANDOC
    #
    ############################################################################
    if (tolower(program) == "pandoc"){
      # Create directory if it does not exist
      dir.create(program.path, recursive = T, showWarnings = FALSE)
      # Check if the program is installed
      program.installed <- system('pandoc -v') == 0
      if (program.installed == T){
        print("log: PANDOC already installed in the computer")
      }else{
        # Check if the executable file is already available
        if (file.exists(file.path(program.path, pandoc.exe))){
          print(paste0("log: '", pandoc.exe, "' file already exists"))
          # Remove any executable file
          exe.files <- list.files(program.path, ".msi") 
          id <- !exe.files %in% miktex.exe
          rm.files <- exe.files[id]
          unlink(file.path(program.path, rm.files))
          # Execute the file
          system(file.path(program.path, "install_pandoc.bat"))
        }else{
          # Download the executable file from the internet and run it
          url <- paste0(pandoc.downloader, pandoc.exe)
          install.URL(url, keep_install_file = T, download_dir = program.path)
        }
      }
    }
    
    ############################################################################
    #
    #                                 MIKTEX
    #
    ############################################################################
    if (tolower(program) == "miktex"){
      # Create directory if it does not exist
      dir.create(program.path, recursive = T, showWarnings = FALSE)
      # Check if the program is installed
      program.installed <- Sys.which("mo")
      if (program.installed != ""){
        print("log: MIKTEX already installed in the computer")
      }else{
        # Check if the executable file is already available
        if (file.exists(file.path(program.path, miktex.exe))){
          print(paste0("log: '", miktex.exe, "' file already exists"))
          # Remove any executable file
          exe.files <- list.files(program.path, ".exe") 
          id <- !exe.files %in% miktex.exe
          rm.files <- exe.files[id]
          unlink(file.path(program.path, rm.files))
          # Execute the file
          system(file.path(program.path, "install_miktex.bat"))
        }else{
          # Download the executable file from the internet and run it
          url <- paste0(miktex.downloader, miktex.exe)
          install.URL(url, keep_install_file = T, download_dir = program.path)
        }
      }
    }
  }
}
