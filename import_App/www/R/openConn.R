openConn <- function(conn)
  if (class(conn)[1] == 'RODBC' | class(conn)[1] == 'numeric'){
  tryCatch({odbcGetInfo(conn);TRUE},error=function(...)FALSE)
  }else if(class(conn)[1] == 'MySQLConnection'){
    tryCatch({dbGetInfo(conn);TRUE},error=function(...)FALSE)
  }
