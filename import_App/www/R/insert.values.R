insert.values <- function(tables, db2.dbms, db2.channel, order.tables, data){
  if(db2.dbms == "mysql"){
    db2.dbms <<-"mariadb"
  }
  
  data33 <<- data
  
  #############################################################################
  # Re-scale "obs_value" in observation table
  if ("obs_value" %in% colnames(data)){
    codes00 <- as.numeric(unique(data$described_by))
    request00 <- paste0("SELECT code, element_scale FROM obs_element WHERE code = ", 
                        paste0(codes00, collapse =" OR code = "), ";")
    result00 <- getQuery(db2.dbms, db2.channel, request00)
    for (i000 in c(1:length(codes00))){
      id <- which(data$described_by == result00$code[i000])
      data$obs_value[id] <- round(as.numeric(data$obs_value[id])/result00$element_scale[i000])
    }
  }
  if ("obsValue" %in% colnames(data)){
    codes00 <- as.numeric(unique(data$describedBy))
    request00 <- paste0("SELECT elementId, elementScale FROM obsElement WHERE elementId = ", 
                        paste0(codes00, collapse =" OR elementId = "), ";")
    result00 <- getQuery("mariadb", db2.channel, request00)
    for (i000 in c(1:length(codes00))){
      id <- which(data$describedBy == result00$elementId[i000])
      data$obsValue[id] <- round(as.numeric(data$obsValue[id])/result00$elementScale[i000])
    }
  }
  
  for (i in c(1:length(order.tables))){
    # DB1 & DB2 - Get table names
    table.name <- order.tables[i]
    id <- which(tables$table.names == table.name)
    
    db2.table <- unique(with(tables,get(paste0(db2.dbms,".table")))[id])
    #     print(order.tables)
    #     print(table.name)
    #     print(db2.table)
    db2.attr.prev <- with(tables,get(paste0(db2.dbms,".attr")))[id]
    
    #########################################################################
    # Get attributes
    id2 <- which(!is.na(db2.attr.prev))
    db2.attr <- db2.attr.prev[id2]
    
    #########################################################################
    # DB2 - Get datatype
    print(db2.dbms)
    print(db2.table)
    db2.info.schema <- getColumns(db2.dbms, db2.channel, db2.table)
    db2.columns <<- db2.info.schema$Field
    db2.datatype <<- tolower(db2.info.schema$Type)
    # Select only the available columns
    id3 <- match(db2.attr, db2.columns)
    db2.columns <<- db2.columns[id3]
    db2.datatype <<- db2.datatype[id3]
    
    #########################################################################
    # DB2 - Get datatype for the columns available in 'data'
    columns.available <- colnames(data)
    id4 <- match(columns.available, db2.columns)
    db2.columns <<- db2.columns[id4]
    db2.datatype <<- db2.datatype[id4]
    
    ###########################################################################
    # FOREIGN KEYS
    # Check which of the columns are foreign keys
    if (db2.dbms == "access"){
      foreign_keys <- access.foreignKeys(db2.channel, db2.table)
    }else if(db2.dbms == "mariadb"){
      foreign_keys <- mariadb.foreignKeys(db2.channel, db2.table)
    }else{
      print("No foreign key identified. No import can be carried out")
      return()
    }
    
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
      #tt <- paste0("From '", db1.table, "' to '", db2.table,"'")
            incProgress(1/(nrow(data)*length(order.tables)))
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
        
        row_values.to.check <<- row_values[, as.character(row_values.colnames),
                                           drop = F]
      }else{
        row_values.to.check <<- row_values.to.insert
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
        id000 <- which(db2.columns == names(as.data.frame(row_values.to.check)[i1]))
        delimiters.function <- get(paste0(db2.dbms, ".delimiters"))
        delimiters <- delimiters.function(row_values.to.check[i1],
                                          db2.datatype[id000])
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
      
      ########################################################################
      # CHECK IF "values_to_check" belong to a FK
      if (nrow(foreign_keys)>0){
        id01<-match(colnames(data),foreign_keys$szColumn)
        id01 <- id01[!is.na(id01)]
        
        for (i01 in c(1:length(id01))){
          referenced_table <- foreign_keys$szReferencedObject[i01]
          referenced_column <- foreign_keys$szReferencedColumn[i01]
          fk_column <- foreign_keys$szColumn[i01]
          
          request01 <-  paste("select * from ",
                              referenced_table,
                              " where ", referenced_column,
                              with(values_to_check,
                                   get(as.character(fk_column))),"",sep="")
          
          result01 <- sqlQuery(db2.channel,request01)
          if (is.null(nrow(result01)) || nrow(result01)==0){
            request02 <- paste("INSERT INTO ", referenced_table, 
                               " (",referenced_column,") ",
                               " VALUES (",
                               with(values_to_insert,
                                    get(as.character(fk_column))),")",sep="")
#             print(paste0(referenced_table, ": " , referenced_column, " --> ",
#                          request02, collapse = ","))
            result02 <- sqlQuery(db2.channel,request02)
          }
        }
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
      #observe({
      if (nrow(result03) > 0){
        text <- paste(values_to_check, collapse=" ")
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
          diff_values <- lapply(checking, function(x)length(unique(x)))
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
            answer.option <- c("yes","no")
            cat("\nPlease type: 'yes' or 'no'\n")
            # Check if the session is interactive or not 
            # (interactive means that the script is run in the R-Command window)
            if (interactive() == T){
              answer <- readline() 
            }else{
              answer <- readLines(con = "stdin", 1)
            }
            
            #answer <- readline(cat("\nPlease type: 'yes' or 'no'\n"))
            
            while (length(grep("n",tolower(answer))!=1)==0 &&
                   length(grep("y",tolower(answer))!=1)==0){
              cat("\nPlease type: 'yes' or 'no'\n")
              if (interactive() == T){
                answer <- readline() 
              }else{
                answer <- readLines(con = "stdin", 1)
              }
            }
            
            if (length(grep("n",tolower(answer))==1)==0){
              text <- paste(values_to_insert,collapse=" ")
              if (db2.dbms == "access"){
                request05 <- paste("UPDATE ", observation, 
                                   "SET ", observation.obs_value, " = ", values_to_insert$obs_value, 
                                   "WHERE ", paste(colnames(values_to_check2), 
                                                   as.matrix(values_to_check2), 
                                                   collapse =" AND ")) 
              }else{
                request05 <- paste("UPDATE ", observation, 
                                   "SET ", observation.obs_value, " = ", values_to_insert$obsValue, 
                                   "WHERE ", paste(colnames(values_to_check2), 
                                                   as.matrix(values_to_check2), 
                                                   collapse =" AND ")) 
              }
              result05 <- getQuery(db2.dbms, db2.channel, request05)
              print("Value updated")
              print(result05)
            }else{
            }
          }
        }
      }
      # })
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
  print("Values inserted")
}