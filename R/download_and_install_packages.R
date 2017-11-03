################################################################################
#' @export download_and_install_packages
#'
#' @title Download and install packages
#'
#' @description The function allows the download and later installation of 
#' packages in a desired path. The user can select whether to download 
#' the packages in a "source" type or in a "win.binary" type
#' 
#' @param packs string. Package(s) to be downloaded and installed. 
#' 
#' @param packs.loc string. Path where the packages should be downloaded. If
#' empty the user will be asked to select a folder interactively.
#' 
#' @param lib.loc string. Path where the libraries should be installed. If
#' empty the user will be asked to select a folder interactively.
#' 
#' @param type string. Type of package to download (either "source" or 
#' "win.binary"). Default is "win.binary"
#'
#'
#' @details This function will download not only the packages given by 
#' the user, but also the additional packages required by the given ones (i.e.
#' it checks the dependencies of each package)
#'
#' @examples 
#' download_and_install_packages(packs = c("shiny", "dygraphs"), 
#'                               packs.loc = "C:/Users/username/R_pkgs/",
#'                               lib.loc = "C:/Users/username/Documents/R/",
#'                               type = "win.binary")
#'
################################################################################

download_and_install_packages <- function(packs=NULL,
                                          packs.loc = NULL,
                                          lib.loc = NULL,
                                          type = "win.binary"){
  
  ##############################################################################
  #
  #                        SET UP REPOSITORY
  #
  ##############################################################################
  local({
    r <- getOption("repos")
    r["CRAN"] <- 'http://cran.us.r-project.org'
    options(repos = r)
  })
  
  ##############################################################################
  #
  #                        R-PACKAGES
  #
  ##############################################################################
  # packs - Packages to download
  if (!exists('packs') || is.null(packs)){
    return(print(paste0("log: No R-packages selected. " ,
                        "No download/installation will be carried out")))
  }
  
  ##############################################################################
  #
  #                        R-PACKAGES PATH
  #
  ##############################################################################
  
  # packs.loc - Set the location where package are to be downloaded
  while (!exists("packs.loc") || is.null(packs.loc)){
    print("log: Please, select one folder where the package should be downloaded.")
    packs.loc <- choose.dir(caption = "Select folder")
  }
  if (is.na(packs.loc)){
    return(print(paste("log: No location for the R-packages selected. ",
                       "Therefore the download cannot be completed")))
  }
  
  if (dir.exists(packs.loc)){
    print(paste0("log: Path '", packs.loc, "' already exists"))
  }else{
    print(paste0("log: Path '", packs.loc, "' will be created"))
    dir.create(packs.loc)
  }
  
  ##############################################################################
  #
  #                        LIBRARY PATH
  #
  ##############################################################################
  while (!exists("lib.loc") || is.null(lib.loc)){
    print("log: Please, select one folder where the package should be unpacked.")
    lib.loc <- choose.dir(caption = "Select folder")
  }
  if (is.na(lib.loc)){
    return(print(paste("log: No location for the R-libraries selected. ",
                       "Therefore the installation cannot be completed")))
  }
  
  ##############################################################################
  #
  #                        TYPE OF COMPRESSIOn
  #
  ##############################################################################
  
  # type - Type of compression (zip.type)
  if (type == "win.binary"){
    zip.type <- ".zip"
  }else if(type == "source"){
    zip.type <- ".tar.gz"
  }else{
    print("log: No valid 'type' of compression given (not 'win.binary', nor 'source'")
    print("log: Please check the file 'main.R'")
    return()
  }
  
  ##############################################################################
  #
  #                       CHECK DEPENDENCIES
  #
  ##############################################################################
  # Get dependencies from all required packages
  if (!is.null(packs)){
    pkgs <- unlist(
      tools::package_dependencies(packs,
                                  available.packages(),
                                  which=c("Depends", "Imports","LinkingTo"),
                                  recursive=TRUE)
    )
    print(paste("log: Dependencies: ",paste(pkgs, collapse = ",")))
    pkgs <- union(packs, pkgs)
    
    ############################################################################
    #
    #                        CHECK IF PACKAGES ARE INSTALLED
    #
    ############################################################################
    pkgs.to.be.installed <- NA
    while(length(pkgs.to.be.installed)!=0){
      pkgs.installed <- utils::installed.packages(
        lib.loc = c(lib.loc, .libPaths()[length(.libPaths())]))[,1]
      
      # Remove pkgs already installed from the list
      id00 <- which(pkgs %in% pkgs.installed)
      
      if (length(id00) == 0){
        print("No R-packages are installed")
        pkgs.to.be.installed <- pkgs
      }else{
        pkgs2 <- sort(pkgs[id00])
        pkgs.to.be.installed <- pkgs[-id00]
        print(paste0("log: '", pkgs2, "' already installed"))
      }
      
      ##########################################################################
      #
      #                        CHECK IF PACKAGES ARE DOWNLOADED
      #
      ##########################################################################
      pkgs.to.be.downloaded <- NA
      while(length(pkgs.to.be.downloaded)!=0){
        # list of files available in packs.loc
        files.lst <- list.files(file.path(packs.loc))
        id01 <- grep(zip.type, files.lst)
        files <- files.lst[id01]
        
        if (length(files) == 0){
          print(paste0("log: No packages found in '", packs.loc, "'"))
          id03 <- NULL
        }else{
          # Get files name
          position <- sapply(1:length(files), 
                             function(i){
                               gregexpr("_", files[i])[[1]][1]
                             }
          )
          files.name <- substr(files, 1, position-1)
          
          id02 <- which(files.name %in% pkgs)
          # Downloaded packages
          downloaded.pkgs <- files.name[id02]
          
          # Remove pkgs already downloaded from the list
          id03 <- which(pkgs.to.be.installed %in% downloaded.pkgs)
        }
        if (length(id03) == 0){
          pkgs.to.be.downloaded <- pkgs.to.be.installed
        }else{
          pkgs2 <- sort(pkgs.to.be.installed[id03])
          pkgs.to.be.downloaded <- pkgs.to.be.installed[-id03]
        }
        
        ########################################################################
        #
        #                        DOWNLOAD PACKAGES
        #
        ########################################################################
        # Download packages
        if (length(pkgs.to.be.downloaded)>0){
          #if(!is.na(pkgs.to.be.downloaded)){
          print(paste0("log: Packages to download: ", 
                       paste(pkgs.to.be.downloaded, collapse = ",")))
          download.packages(pkgs = pkgs.to.be.downloaded,
                            destdir = packs.loc, type = type)
        }else{
          print("log: No packages to download")
          pkgs.to.be.downloaded <- NULL
        }
      }
      
      ##########################################################################
      #
      #                        INSTALL PACKAGES
      #
      ##########################################################################
      files.lst <- list.files(file.path(packs.loc))
      id01 <- grep(zip.type, files.lst)
      files <- files.lst[id01]
      
      # Get files name
      position <- sapply(1:length(files), 
                         function(i){
                           gregexpr("_", files[i])[[1]][1]
                         }
      )
      files.name <- substr(files, 1, position-1)
      id02 <- which(files.name %in% pkgs.to.be.installed)
      if (length(id02)==0){
        print("log: No R-packages to install")
      }else{
        print(paste0("log: Packages to install: ", 
                     paste(files.name[id02], collapse = ",")))
        install.packages(pkgs = file.path(packs.loc, files[id02]), 
                         repos = NULL, 
                         type = type,
                         lib = lib.loc)
      }
    }
    print("log: All R-packages ready to be used!")
  }else{
    print("Installation R-packages not required")
  }
}
