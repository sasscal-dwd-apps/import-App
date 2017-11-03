#############################################################################
from_database_import <- function(db1.type, db1.name, db2.type, db2.name){
  ###########################################################################
  #isolate({# Libraries
  library(RODBC)
  library(RMySQL)
  library(shiny)
  
  tablesInfo <<- getTables(db2.type)
  for (i1 in c(1:length(tablesInfo))){
    assign(names(tablesInfo[i1]),tablesInfo[[i1]], envir = .GlobalEnv)
  }
  
  ###########################################################################
  #
  #                            TABLES TO IMPORT
  #
  ###########################################################################
  filename <- file.path("www", "data", 'db_conversion_updated_by_hand.csv')
  tables <- read.csv(filename)
  order.tables <- c("station", "station_location", "stationid_alias",
  "obs_element", "observation")
  
  ###########################################################################
  #
  #                               CONNECTION
  #
  ###########################################################################
  # DB1 - connection
  db1.channel <- odbcConnect(db1.name)
  db1.info <- odbcGetInfo(db1.channel)
  if (db1.type == "access"){
    db1.dbms <- as.character(tolower(db1.info['DBMS_Name']))
  }else{
    db1.dbms <- "mariadb"
  }
  
  # DB2 - connection
  db2.channel <- odbcConnect(db2.name)
  db2.info <- odbcGetInfo(db2.channel)
  if (db2.type == "access"){
    db2.dbms <- as.character(tolower(db2.info['DBMS_Name']))
  }else{
    db2.dbms <- "mariadb"
  }
  
  # If database are not "MS-Access" nor "mariadb", return;
  id00 <- which(data.frame(db1.dbms, db2.dbms) == "mariadb" |
                  data.frame(db1.dbms, db2.dbms) == "access")
  if (length(id00) < 2){
    return()
  }
  
  #########################################################################
  #
  #                             MODIFY MARIADB
  #
  #########################################################################
  if (db2.type == "mariadb"){
    modify_mariadb(db2.channel)
    db2.dbms <- db2.type
  }
  if (db1.type == "mariadb"){
    modify_mariadb(db1.channel)
    db1.dbms <- db1.type
  }
  
  ###########################################################################
  #
  #                           GET TABLES & ATTRIBUTES
  #
  ###########################################################################
  for (i in c(1:length(order.tables))){
    # DB1 & DB2 - Get table names
    table.name <- order.tables[i]
    id <- which(tables$table.names == table.name)
    db1.table <- unique(with(tables,get(paste0(db1.dbms,".table")))[id])
    db2.table <- unique(with(tables,get(paste0(db2.dbms,".table")))[id])
    db1.attr.prev <- with(tables,get(paste0(db1.dbms,".attr")))[id]
    db2.attr.prev <- with(tables,get(paste0(db2.dbms,".attr")))[id]
    
    tt <- paste0(textImporting, "'", db1.table, "' ",textImportingTo, "'", db2.table,"...'")
    message(tt)
    
    #########################################################################
    # DB1 & DB2 - Get attributes
    id2 <- which(!is.na(db1.attr.prev) & !is.na(db2.attr.prev))
    db1.attr <- db1.attr.prev[id2]
    db2.attr <- db2.attr.prev[id2]
    
    #########################################################################
    # DB1 - Get datatype
    db1.info.schema <- getColumns(db1.dbms, db1.channel,db1.table)
    db1.columns <- db1.info.schema$Field
    db1.datatype <- tolower(db1.info.schema$Type)
    # Select only the available columns
    id3 <- match(db1.attr, db1.columns)
    db1.columns <- db1.columns[id3]
    db1.datatype <- db1.datatype[id3]
    
    
    
    #########################################################################
    #
    #                         GET DATA FROM DB1
    #
    #########################################################################
    request <- paste0("SELECT ",
                      paste0(db1.columns, collapse = ","),
                      " FROM ", db1.table,";")
    data <- getQuery(db1.dbms, db1.channel, request)
    
    #############################################################################
    # Re-scale "obs_value" in observation table
    if ("obs_value" %in% colnames(data)){
      codes00 <- as.numeric(unique(data$described_by))
      request00 <- paste0("SELECT code, element_scale FROM obs_element WHERE code = ", 
                          paste0(codes00, collapse =" OR code = "), ";")
      result00 <- getQuery(db1.dbms, db1.channel, request00)
      for (i000 in c(1:length(codes00))){
        id <- which(data$described_by == result00$code[i000])
        data$obs_value[id] <- as.numeric(data$obs_value[id])*result00$element_scale[i000]
      }
    }
    if ("obsValue" %in% colnames(data)){
      codes00 <- as.numeric(unique(data$describedBy))
      request00 <- paste0("SELECT elementId, elementScale FROM obsElement WHERE elementId = ", 
                          paste0(codes00, collapse =" OR elementId = "), ";")
      result00 <- getQuery("mariadb", db1.channel, request00)
      for (i000 in c(1:length(codes00))){
        id <- which(data$describedBy == result00$elementId[i000])
        data$obsValue[id] <- as.numeric(data$obsValue[id])*result00$elementScale[i000]
      }
    }
    
    #########################################################################
    # Change headers of "data" to match those of the db2
    colnames(data) <- db2.attr
    
    #########################################################################
    #
    #                       INSERT VALUES IN DB2
    #
    #########################################################################
    insert.values(tables, db2.dbms, db2.channel, order.tables[i], data)
    
    message(textImportFinalMessage)
    message("<br>")
  }
}
