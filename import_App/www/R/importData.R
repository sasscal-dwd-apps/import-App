#############################################################################
importData <- function(input, output, session){
  ###########################################################################
  # Libraries
  library(RODBC)
  library(RMySQL)
  library(shiny)
  
  ###########################################################################
  #
  #                            TABLES TO IMPORT
  #
  ###########################################################################
  # Inputs
  db2.type <- input$toDbase
  db2.name <- input$dbase2
  db2.username <- input$db2.username
  db2.password <- input$db2.passwd
  db2.port <- input$db2.port
  
  db1.type <- input$fromDbase
  db1.name <- input$dbase1
  db1.username <- input$db1.username
  db1.password <- input$db1.passwd
  db1.port <- input$db1.port
  
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
  if (db1.type == "access"){
    db1.channel <<- odbcConnect(db1.name,"admin","admin")
    db1.info <<- odbcGetInfo(db1.channel)
    db1.dbms <<- as.character(tolower(db1.info['DBMS_Name']))
  }else{
    db1.channel <<- dbConnect(MySQL(), db1.name, db1.username,
                              db1.password,
                              port = db1.port)
    db1.dbms <<- "mariadb"
  }
  
  # DB2 - connection
  if (db2.type == "access"){
    db2.channel <<- odbcConnect(db2.name,"admin","admin")
    db2.info <<- odbcGetInfo(db2.channel)
    db2.dbms <<- as.character(tolower(db2.info['DBMS_Name']))
  }else{
    db2.channel <<- dbConnect(MySQL(), db2.name, db2.username,
                              db2.password,
                              port = db2.port)
    db2.dbms <<- "mariadb"
  }
  
  # If database are not "MS-Access" nor "mariadb", return;
  id00 <- which(data.frame(db1.dbms, db2.dbms) == "mariadb" |
                  data.frame(db1.dbms, db2.dbms) == "access")
  if (length(id00) < 2){
    return()
  }
  
  #########################################################################
  #
  #                         MODIFY MARIADB
  #
  #########################################################################
  if (db2.type == "mariadb"){
    # Remove foreign key in "stationlocationhistory"
    # to drop the Index "history"
    requests <- c(
      paste("ALTER TABLE stationlocationhistory DROP FOREIGN KEY ",
            "FK_mysql_climsoft_db_v4_station_stationLocationHistory;"),
      "DROP INDEX history ON stationlocationhistory;",
      paste("ALTER TABLE stationlocationhistory ADD CONSTRAINT",
            "FK_mysql_climsoft_db_v4_station_stationLocationHistory",
            "FOREIGN KEY(belongsTo) REFERENCES `station` (`stationId`);"),
      paste("ALTER TABLE stationidalias DROP FOREIGN KEY ",
            "FK_mysql_climsoft_db_v4_station_stationIdAlias;"),
      paste("ALTER TABLE stationidalias ADD CONSTRAINT ",
            "FK_mysql_climsoft_db_v4_station_stationIdAlias ",
            "FOREIGN KEY(refersTo) REFERENCES `station` (`stationId`);"),
      paste("ALTER TABLE `observationinitial`",
            "CHANGE COLUMN `mark` `mark` VARCHAR(50) NULL DEFAULT NULL ", "AFTER `capturedBy`;"),
      paste("ALTER TABLE `observationfinal`",
            "CHANGE COLUMN `mark` `mark` VARCHAR(50) NULL DEFAULT NULL ", "AFTER `capturedBy`;")
    )
    
    for (i00 in c(1:length(requests))){
      print(i00)
      request00 <- requests[i00]
      # check if it gives an error
      testing <- tryCatch({getQuery(db2.type, db2.channel, request00);
        TRUE}, error=function(...)FALSE)
      if (testing == F){
        print(paste("Error when sending Request:", request00))
      }
    }
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
    # DB2 - Get datatype
    db2.info.schema <- getColumns(db2.dbms, db2.channel,db2.table)
    db2.columns <- db2.info.schema$Field
    db2.datatype <- tolower(db2.info.schema$Type)
    # Select only the available columns
    id3 <- match(db2.attr, db2.columns)
    db2.columns <- db2.columns[id3]
    db2.datatype <- db2.datatype[id3]
    
    #########################################################################
    #
    #                         GET DATA FROM DB1
    #
    #########################################################################
    request <- paste0("SELECT ",
                      paste0(db1.columns, collapse = ","),
                      " FROM ", db1.table,";")
    data <- getQuery(db1.dbms, db1.channel, request)
    
    #########################################################################
    #
    #                   CHECK & INSERT DATA INTO DB2
    #
    #########################################################################
    # Values to insert
    values_to_insert <- as.data.frame(
      setNames(
        replicate(
          length(db2.columns),numeric(0), simplify = F),
        db2.columns)
    )
    
    #########################################################################
    # Get row data and go row by row
    for (i0 in c(1:nrow(data))){
      tt <- paste0(textImportingFrom, " '", db1.table, "' ",
                   textImportingTo," '", db2.table,"'")
      incProgress(1/(nrow(data)*length(order.tables)),
                  detail = tt)
      # Remove strange characters
      row_values <- as.matrix(
        as.data.frame(
          lapply(data[i0,], function(x){
            if(is.character(x) | is.factor(x)){
              gsub("'","",x)
            }else{x}}
          )
        )
      )
      
      # Get "colnames" as those of db2
      colnames(row_values) <- db2.columns
      
      # Set NA row values as NULL
      row_values[is.na(row_values)] <- "NULL"
      
      # Define values to insert
      row_values.to.insert <- row_values
      
      #######################################################################
      # Values to check
      if (table.name == "obs_element"){
        obs_element.fields <- c("code")
        id01 <- which(tables$table.names == table.name &
                        tables$attr.names %in% obs_element.fields)
        row_values.colnames <- with(tables,get(paste0(db2.dbms,".attr")))[id01]
        
        row_values.to.check <- row_values[, as.character(row_values.colnames),
                                          drop = F]
      }else{
        row_values.to.check <- row_values.to.insert
      }
      
      values_to_check <- as.data.frame(
        setNames(
          replicate(
            length(row_values.to.check),numeric(0), simplify = F),
          colnames(row_values.to.check))
      )
      
      #######################################################################
      #
      #                        DELIMITERS AND EXPRESSIONS
      #
      #######################################################################
      # Check values
      for(i1 in c(1:ncol(row_values.to.check))){
        # Check the delimiters and expressions
        delimiters.function <- get(paste0(db2.dbms, ".delimiters"))
        delimiters <- delimiters.function(row_values.to.check[i1],
                                          db2.datatype[i1])
        values_to_check[1,i1] <- paste(delimiters$expression,
                                       delimiters$delimiter,
                                       row_values.to.check[i1],
                                       delimiters$delimiter,
                                       sep="")
      }
      
      #######################################################################
      # Insert values
      for(i1 in c(1:length(db2.columns))){
        # Check the delimiters and expressions
        delimiters.function <- get(paste0(db2.dbms, ".delimiters"))
        delimiters <- delimiters.function(row_values.to.insert[i1],
                                          db2.datatype[i1])
        
        values_to_insert[1,i1] <- paste(delimiters$delimiter,
                                        row_values.to.insert[i1],
                                        delimiters$delimiter,
                                        sep="")
      }
      
      #######################################################################
      #
      #               CHECK IF DATA ALREADY AVAILABLE IN DB2
      #
      #######################################################################
      # Check if values for specific columns are available in the database
      col.names <- c("obs_value","captured_by", "obsValue", "capturedBy")
      
      id000 <- which(colnames(values_to_check) %in% col.names)
      if (length(id000)>0){
        values_to_check2 <- values_to_check[,-id000]
      }else{
        values_to_check2 <- values_to_check
      }
      # Check the values in the database
      caraca03 <- paste(colnames(values_to_check2),as.matrix(values_to_check2),
                        sep="")
      request03 <- paste("SELECT * FROM ", db2.table,
                         " WHERE ", paste(caraca03,collapse=" AND "),"",sep="")
      result03 <- getQuery(db2.dbms, db2.channel, request03)
      
      #######################################################################
      # If there is already a record
      if (nrow(result03) > 0){
        text <- paste(values_to_insert,collapse=" ")
        print(paste(i0," - One entry to ",text," does already exist\n"))
        
        # Check if the col.names are available in the given table
        id001 <- which(colnames(result03) %in% col.names)
        if (length(id001)==0){}else{
          old_values <- as.matrix(result03[,id001])
          old_values2 <- result03[,id001]
          as_class <- paste0("as.",sapply(old_values2,class))
          col.names.new <- colnames(result03[id001])
          new_values <- gsub("'","",values_to_insert[col.names.new])
          
          # Change the class of the new_values
          tj <- sapply(1:2,function(i){
            tj1 <-as.character(sapply(new_values[i],as_class[i]))
            tj2 <- rbind(tj1)
            tj2
          })
          new_values <- tj
          
          checking <- as.data.frame(rbind(old_values,new_values))
          # check for duplicates
          diff_values <- lapply(lapply(lapply(checking,levels),table),length)
          id002 <- which(diff_values==2)
          if (length(id002)>0){
            # Missmatch
            
            print("There is already a record in the database:")
            text <- paste("Do you want to replace",
                          colnames(checking[id002]), "=",
                          old_values[id002], "with",
                          colnames(checking[id002]), "=",
                          new_values[id002], collapse = "\n")
            print(text)
            answer.option <- c("yes","no")
            answer <- readline(print("\nPlease type: 'yes' or 'no'\n"))
            
            while (length(grep("n",tolower(answer))!=1)==0 &&
                   length(grep("y",tolower(answer))!=1)==0){
              answer <- readline(print("\nPlease type: 'yes' or 'no'\n"))
            }
            
            if (length(grep("n",tolower(answer))==1)==0){
              text <- paste(values_to_insert,collapse=" ")
              print(paste(i0," - One entry to ",text," does already exist\n"))
            }
          }
        }
      }
      
      #######################################################################
      #
      #                         INSERT A NEW RECORD
      #
      #######################################################################
      # If the record is not yet available in the database
      if (nrow(result03)==0){
        # Insert the values into the database
        caraca04 <- apply(values_to_insert,1,paste,collapse=",")
        columns_to_insert <- paste(colnames(values_to_insert),collapse=",")
        request04<-paste("INSERT INTO ", db2.table,
                         " (",columns_to_insert,") ",
                         " VALUES (",caraca04,")",sep = "")
        result04 <- getQuery(db2.dbms, db2.channel,request04)
      }
    }
  }
}
