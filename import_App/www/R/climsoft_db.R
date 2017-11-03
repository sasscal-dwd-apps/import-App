##########################################################################
#' @export climsoft_db
#'
#' @title Display options when CLIMSOFT is selected
#'
#' @description It displays the options available when CLIMSOFT is selected as
#' 'Data Source'. It calls secondary functions such as
#' \code{\link{climsoft_db_mariadb}} and \code{\link{climsoft_db_access}}.
#'
#' @param input shiny object. It is a list-like object.
#' It stores the current values of all of the widgets in the
#' application. The Shiny App uses the \code{input}
#' to receive user input from the client web browser.
#' @param output shiny object. It is a list-like object that stores
#' instructions for building the R objects in the Shiny application.
#' Shiny application.
#' @param session shiny object. It is a special object that is used for finer
#' control over a user's app session.
#'
#' @details This function will be called by the function
#' \code{\link[ACD]{server.R}}.
#'
#' @note This function has been created specifically for the
#' \href{https://github.com/rposn/ACD-App}{'ACD-App'} and does not work
#' outside the ACD-App environment.
#'
#' @author Rafael Posada (SASSCAL/DWD), November 2016

climsoft_db <- function(input,output, session){
  # A) CLIMSOFT
  library(RODBC)
  #print(paste0("This is the path I am working with: ", getwd()))
  print("log: CLIMSOFT option selected")
  
  ###########################################################################
  #
  #                          SIDE PANEL
  #
  ###########################################################################
  # Create uiOutputs to be placed in the sidePanel
  uid <- substr(UUIDgenerate(FALSE),1, 4)
  uiDbType <<- paste("uiDbType_", uid)              # Type of database
  uiDns <<- paste0("uiDns_", uid)                  # DNS
  uiConnect <<- paste0("uiConnect_", uid)          # Connection
  
  # Create uiOutputs for the "sidePanel"
  output$UIsSidePanel <-  renderUI({
    uiOutputs <- list(
      uiOutput(uiDbType),
      uiOutput(uiDns),
      uiOutput(uiConnect),
  
    )
    do.call(tagList, uiOutputs)
  })
  
  ###########################################################################
  # Type of database
  list_dbType <<- c("", "access", "mariadb")
  dbType <<- paste0("dbType_", uid)
  output[[uiDbType]] <- renderUI({
    selectInput(dbType, label = (textSidePanel01),
                choices = list_dbType,selected = "")
  })
  
  climsoft_db_disconnect(input, output, session)
  channel2 <<- -1
  
    observe({
      if (!is.null(input[[dbType]]) && input[[dbType]] == 'mariadb'){
        climsoft_db_disconnect(input, output, session)
        channel2 <<- -1
        #######################################################################
        tablesInfo <<- getTables(input[[dbType]])
        for (i1 in c(1:length(tablesInfo))){
          assign(names(tablesInfo[i1]),tablesInfo[[i1]], envir = .GlobalEnv)
        }
        output <- climsoft_db_mariadb(input,output, session)
      } else if (!is.null(input[[dbType]]) && input[[dbType]] == 'access'){
        climsoft_db_disconnect(input, output, session)
        channel2 <<- -1
        #######################################################################
        print(getwd())
        tablesInfo <<- getTables(input[[dbType]])
        for (i1 in c(1:length(tablesInfo))){
          assign(names(tablesInfo[i1]),tablesInfo[[i1]], envir = .GlobalEnv)
        }
        output <- climsoft_db_access(input,output, session)
      }
    })
  return(output)
}
