db_conversion <- function(){
  #library(data.from.climsoft.db)
  channel1 <- get.connection.access("CLIMSOFT_outros", "admin", "admin")
  #request <- "SELECT * FROM MSysObjects WHERE Type=1;"
  result <- sqlTables(channel1)
  result1 <- result[which(result$TABLE_TYPE == "TABLE"),]$TABLE_NAME


  #library(data.from.climsoft.db)
  channel2 <- get.connection.access("mariadb_climsoft_test_db_v4", "root", "climsoft")
  result2 <- sqlTables(channel2)$TABLE_NAME


  # Remove "_" from table names
  tables1 <- gsub("_",  "", result1)
  tables2 <- gsub("_", "", result2)

  # Compare table names
  id1 <- which(tables1 %in% tables2)
  id2 <- which(tables2 %in% tables1)
  match.table.names <- data.frame(access = result1[id1], mariadb = result2[id2])


  # Add new tables to "match.table.names"
  match.table.names <- rbind(match.table.names, data.frame(access = "observation", mariadb = "observationinitial"))
  match.table.names <- rbind(match.table.names, data.frame(access = "observation", mariadb = "observationfinal"))
  match.table.names <- rbind(match.table.names, data.frame(access = "station_location", mariadb = "stationlocationhistory"))


  # Get Attributes
  test <- data.frame()
  for (i in c(1:nrow(match.table.names))){
    table1 <- match.table.names[i, 1]
    table2 <- match.table.names[i, 2]

    attr1_old <- colnames(sqlFetch(channel1, table1))
    attr2_old <- tolower(sqlQuery(channel2, paste0("DESC ", table2, ";"))$Field)

    # Remove "_" from table names
    attr1 <- tolower(gsub("_",  "", attr1_old))
    attr2 <- tolower(gsub("_", "", attr2_old))

    # Compare attribute names
    id1 <- which(attr1 %in% attr2)
    id2 <- which(attr2 %in% attr1)
    if (length(id1) > 0){
      test <- rbind(test,
                    data.frame(access.table = table1,
                               access.attr = attr1_old[id1],
                               mariadb.table = table2,
                               mariadb.attr = attr2_old[id2]))
    }

    # Compare attribute names
    id01 <- which(!attr1 %in% attr2)
    id02 <- which(!attr2 %in% attr1)
    if (length(id01) > 0){
      test <- rbind(test,
                    data.frame(access.table = table1,
                               access.attr = attr1_old[id01],
                               mariadb.table = table2,
                               mariadb.attr = NA))
    }
    if (length(id02) > 0){
      test <- rbind(test,
                    data.frame(access.table = table1,
                               access.attr = NA,
                               mariadb.table = table2,
                               mariadb.attr = attr2_old[id02]))
    }
  }
  # Save data.frame
  filepath <- path.expand(getSrcDirectory(function(x) {x}))
  print(filepath)
  filename <- paste0(file.path(filepath, "../inst/"), "db_conversion.csv")
  print(filename)
  write.table(test, filename, sep = ";", col.names = T, row.names = F)
}
