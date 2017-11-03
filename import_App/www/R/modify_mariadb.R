modify_mariadb <- function(channel){
  #########################################################################
  #
  #                         MODIFY MARIADB
  #
  #########################################################################
  db2.type <- "mariadb"
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
    testing <- tryCatch({getQuery(db2.type, channel, request00);
      TRUE}, error=function(...)FALSE)
    if (testing == F){
      print(paste("Error when sending Request:", request00))
    }
  }
}