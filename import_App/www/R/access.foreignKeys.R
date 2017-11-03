access.foreignKeys <- function(channel, table.name){
request0 <-paste("select szColumn,",
                 "szObject,",
                 "szReferencedObject,",
                 "szReferencedColumn ",
                 "from MSysRelationships ",
                 "where szObject = '",table.name,"'",
                 sep="")

foreign_keys <- sqlQuery(channel,request0)
return(foreign_keys)
}