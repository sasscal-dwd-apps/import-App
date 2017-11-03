database_destination <- function(input, output, session){
  ##########################################################################
  # dbase names are reactive values
  dbases2.prev <- reactiveValues(d = NULL)
  
  ##########################################################################
  # Create SIDE PANEL INPUTS
  output$dbDestination <- renderUI({
    uiOutput <- list(
      conditionalPanel(
        condition = "output.cond1 == true",
        h3(textDbDestination),
        selectInput("toDbase",
                    label = textDbType,
                    choices = c("access", "mariadb"),
                    selected = ""),
        conditionalPanel(
          condition = "input.toDbase2 !== ''",
          uiOutput("uiDbase2")
        )
      )
    )
    do.call(tagList, uiOutput)
  })
  
  ##########################################################################
  # Select if it as an "access" or a "mariadb" db
  observe({
    # If MS-Access
    if (!is.null(input$toDbase)){
      if (input$toDbase == "access"){
        dns_type <- "access"
      }
      if (input$toDbase == "mariadb"){
        dns_type <- "mysql"
      }
      list_ODBC <- odbcDataSources(type = c("user"))
      drivers_prev <<- as.character(list_ODBC)
      dns_prev <- row.names(as.data.frame(list_ODBC))
      dns_id <- (grep("climsoft", tolower(dns_prev)))
      dns <- dns_prev[dns_id]
      drivers <- drivers_prev[dns_id]
      id <- grep(dns_type, tolower(drivers))
      dbases2 <<- dns[id]
      dbases2.prev$d <- dbases2
    }
  })
  output$uiDbase2 <- renderUI({
    selectInput("dbase2",
                label = textDNS,
                choices = c("", dbases2.prev$d),
                selected = "")
  })
}