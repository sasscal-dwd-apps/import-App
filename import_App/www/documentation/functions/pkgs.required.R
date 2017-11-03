pkgs.required <- function(){
pkgs.online <- sort(c("jsonlite", 
                          "gdata", 
                          "shinyBS", 
                          "shiny", 
                          "rhandsontable", 
                          "rmarkdown",
                          "RODBC", 
                          "RMySQL", 
                          "uuid", 
                          "shinyjs", 
                          "XLConnect"))

return(list(pkgs.online))
}