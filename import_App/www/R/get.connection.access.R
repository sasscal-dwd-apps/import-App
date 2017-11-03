#' @title Connect to a MS-Access database.
#'
#' @description \code{get.connection.access}
#' Allows the connection to a MS-Access database.
#'
#' The connection is done through the ODBC. Therefore, a Data Source
#' Name (DSN) that links to the MS-Access database has to be defined
#' previously in the ODBC manager.
#'
#' @param db.name character. Name of the DSN to connect to
#' the MS-Access database. If empty, the \code{db.name} has to be
#' typed in manually in the console.
#'
#' @param db.user character. User name. If not given, the
#' \code{db.user} has to be typed in manually in the console.
#'
#' @param db.pass character. User password to access
#' to the database. If empty, a new window will pop up to
#' enter the \code{db.pass} manually.
#'
#' @return  \code{channel} ODBC connection details.
#' @examples
#' # Get the connection to a MS-Access database defined in the ODBC
#' manager with the DSN "CLIMSOFT"
#' channel <- get.connection.access("CLIMSOFT","admin","admin")


##############################################################
get.connection.access <-
  function(db.name=NULL,db.user=NULL,db.pass=NULL){
    ############################################################
    # PACKAGES
    library(RODBC)
    library(tcltk)
    
    # CHECK THE EXISTENCE OF A DEFAULT FILE WITH CONNECTION DETAILS
    
    filename <- file.path(path.package('data.from.climsoft.db'),
                          'data','db_connection.csv')
    var.options<- c("yes","no")
    # CHECK THE EXISTENCE OF THE INPUT ARGUMENTS;
    # db.name
    if (is.null(db.name)){
      print("Please type the name of the database:")
      db.name <- readline("db.name: ")
      if (nchar(db.name)==0){
        # Default database name;
        db.name <- "CLIMSOFT"
        print("No db name given, default value will be used: ")
        print(paste("Default database: '",db.name))
      }
    }
    
    # db.user
    if (is.null(db.user)){
      tt2 <- tktoplevel()
      pass2 <- tclVar()
      tkpack(tklabel(tt2,text='Username:'))
      tkpack(tkentry(tt2,textvariable=pass2))
      tkpack(tkbutton(tt2,text="Done",command=function()tkdestroy(tt2)))
      tkwait.window(tt2)
      db.user <- tclvalue(pass2)
    }
    
    
    ############################################################
    # ASK FOR THE PASSWORD
    # To hide the password, the package "tcltk" will be used. In
    # this way a window will pop up to write down the password
    # db.pass
    if (is.null(db.pass)){
      tt <- tktoplevel()
      pass <- tclVar()
      tkpack(tklabel(tt,text='Password:'))
      tkpack(tkentry(tt,textvariable=pass,show='*'))
      tkpack(tkbutton(tt,text="Done",command=function()tkdestroy(tt)))
      tkwait.window(tt)
      db.pass <- tclvalue(pass)
    }
    
    ############################################################
    # CONNECT TO ACCESS
    channel <- odbcConnect(db.name,
                           uid=db.user,pwd=db.pass);
    
    return(channel)
  }
