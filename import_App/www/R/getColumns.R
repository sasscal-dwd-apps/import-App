#############################################################################
getColumns <- function(db.type, channel, table.name){

  #if (db.type == "access"){
    result.prev <- sqlColumns(channel, table.name)
    result <- result.prev[c("COLUMN_NAME", "TYPE_NAME")]
    result$TYPE_NAME <- tolower(result$TYPE_NAME)
    colnames(result) <- c("Field", "Type")
  #}
  #if (db.type == "mariadb"){
#     statement <- paste("DESCRIBE ",as.character(table.name),sep="")
#     result.prev <- dbGetQuery(channel, statement)[,1:2]
#     trt <- strsplit(result.prev$Type,split = "[(]")
#     tpa <- sapply(1:length(trt), function(i){
#       trt[[i]][1]
#     })
#     result <- result.prev
#     result$Type <- tpa
#   }
  return(result)
}
