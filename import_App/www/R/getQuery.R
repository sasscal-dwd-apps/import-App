#############################################################################
# Get new functions
getQuery <- function(db.type,channel, query){
  #if (db.type == "access"){
    result <- sqlQuery(channel, query)

  #}
 # if (db.type == "mariadb"){
 #   result <- dbGetQuery(channel, query)
 # }
  return(result)
}
