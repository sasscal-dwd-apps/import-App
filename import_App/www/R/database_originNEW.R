database_originNEW <- function(input, output, session){
  ##########################################################################
  # dbase names are reactive values
  dbases1.prev <- reactiveValues(d = NULL)
  uid <- substr(UUIDgenerate(), 1, 4)
  fromDbase <- paste0("fromDbase_", uid)
  condition <- paste0("input.", fromDbase, "!== ''")
  
  ##########################################################################
  # Create SIDE PANEL INPUTS
  output$dbOrigin <- renderUI({
    uiOutput <- list(
      #conditionalPanel(
      #  condition = "output.cond1 == true",
        h3(textDbOrigin),
        selectInput(fromDbase,
                    label = textDbType,
                    choices = c("access", "mariadb"),
                    selected = ""),
        conditionalPanel(
          condition = condition,
          uiOutput("uiDbase1")
        )
      #)
    )
    do.call(tagList, uiOutput)
  })
  
  ##########################################################################
  # Select if it as an "access" or a "mariadb" db
  observe({
    # If MS-Access
    if (!is.null(input[[fromDbase]])){
      if (input[[fromDbase]] == "access"){
        dns_type <- "access"
      }
      if (input$fromDbase == "mariadb"){
        dns_type <- "mysql"
      }
      list_ODBC <- odbcDataSources(type = c("user"))
      drivers_prev <<- as.character(list_ODBC)
      dns_prev <- row.names(as.data.frame(list_ODBC))
      dns_id <- (grep("climsoft", tolower(dns_prev)))
      dns <- dns_prev[dns_id]
      drivers <- drivers_prev[dns_id]
      id <- grep(dns_type, tolower(drivers))
      dbases1 <<- dns[id]
      dbases1.prev$d <- dbases1
    }
  })
  output$uiDbase1 <- renderUI({
    selectInput("dbase1",
                label = textDNS,
                choices = c("", dbases1.prev$d),
                selected = "")
  })
}