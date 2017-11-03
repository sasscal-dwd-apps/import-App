mariadb.foreignKeys <- function(channel, table.name){
  db.name <- as.character(as.matrix(sqlQuery(channel, "SELECT DATABASE() FROM DUAL;")))
  request0 <- paste0("SELECT TABLE_NAME, ",
                    "COLUMN_NAME, ",
                    "REFERENCED_TABLE_NAME, ", 
                    "REFERENCED_COLUMN_NAME ",
                    "FROM ",
                    "INFORMATION_SCHEMA.KEY_COLUMN_USAGE ",
                    "WHERE ",
                    "REFERENCED_TABLE_SCHEMA = '", db.name, "' AND ",
                    "TABLE_NAME = '", table.name, "';")
  
  result <- sqlQuery(channel, request0)
  
  colnames(result) <- c("szObject", "szColumn", "szReferencedObject",
                   "szReferencedColumn")
  
  foreign_keys <- result
  return(foreign_keys)
}


