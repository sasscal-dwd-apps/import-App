get_db_availability <- function(){
  list_ODBC <- odbcDataSources(type = c("user"))
  drivers_prev <- as.character(list_ODBC)
  dns_prev <- row.names(as.data.frame(list_ODBC))
  dns_id <- (grep("climsoft", tolower(dns_prev)))
  dns <- dns_prev[dns_id]
  return(dns)
}