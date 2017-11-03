from_keyEntry_import <- function(input, output, session, df1) {
  #########################################################################
  #
  #                     IMPORT data TO CLIMSOFT db
  #
  #########################################################################
  temp1<<-df1
  
  # DATABASE CONNECTION
  db2.type <<- input$toDbase
  db2.name <<- input$dbase2
  db2.channel <<- odbcConnect(db2.name)
  db2.info <<- odbcGetInfo(db2.channel)
  db2.dbms <<- as.character(tolower(db2.info['DBMS_Name']))
  channel <<- db2.channel
  
  #########################################################################
  # MODIFY "mariadb" DB
  if (db2.type == "mariadb"){
    modify_mariadb(db2.channel)
    db2.dbms <- db2.type
  }
  tablesInfo <<- getTables(db2.type)
  for (i1 in c(1:length(tablesInfo))){
    assign(names(tablesInfo[i1]),tablesInfo[[i1]], envir = .GlobalEnv)
  }
# print(head(df1))
  
  #observation1<<-na.omit(observation1)
  ##  import into database
  print(paste(station.ident," is being imported"))
  insert.values(tables, db2.dbms, db2.channel, "observation", df1)
  
  
  message(textImportFinalMessage)
  }
