##########################################################################
#' @export getTables
#'
#' @title Get tables and attributes of MS-Access & Mariadb CLIMSOFT databases
#'
#' @description Funtion to identify tables and attributes of a CLIMSOFT
#' database. It gives the correct name of the tables and attributes that
#' will be used along the App to make requests to the CLIMSOFT database.
#'
#' @param db.type character. Type of database selected.
#' There are two possible options: \code{'access'} or \code{'mariadb'}.
#'
#' @return tablesInfo list. A list containing all the tables and attributes
#'  used in the App.
#'
#' @details This function is run directly by the function
#' \code{\link{climsoft_db}}.
#'
#' @author Rafael Posada (SASSCAL/DWD), November 2016
#'
getTables <- function(db.type){
  #filename <- system.file("extdata", 'db_conversion_updated_by_hand.csv', package = "ACD")
  filename <- file.path(".","www","data", 'db_conversion_updated_by_hand.csv')
  tables <- read.csv(filename)
  db.type.table <- paste0(db.type, ".table")
  db.type.attr <- paste0(db.type, ".attr")

  # Get default names
  default.tables <- tables$table.names
  default.attr <- tables$attr.names
  default.fields <- paste0(default.tables, ".", default.attr)

  # Get current "table name"
  table.names <- unique(default.tables)
  for (i1 in (1:length(table.names))){
    id1 <- which(default.tables == table.names[i1])
    table.name <- unique(with(tables, get(db.type.table))[id1])
    assign(as.character(table.names[i1]), as.character(table.name))

    # Get current "attribute names"
    attr.names <- tables$attr.names[id1]
    for (i2 in (1:length(attr.names))){
      id2 <- which(attr.names == attr.names[i2])
      attr.name <- with(tables[id1,], get(db.type.attr))[id2]
      assign(paste0(table.names[i1], ".", as.character(attr.names[i2])),
             paste0(table.name, ".", as.character(attr.name)))
    }
  }
  variables <- ls(environment())
  tablesInfo <- c()

  for (i4 in variables){
    tablesInfo<-c(tablesInfo,list(get(i4)))
  }
  names(tablesInfo) <- variables
  return(tablesInfo)
}
