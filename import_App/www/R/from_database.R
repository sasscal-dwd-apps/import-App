from_database <- function(input,output, session){
  print("log: 'From Database' option selected")
  cleanUp(input, output, session)
  button <- paste0("button_", substr(UUIDgenerate(), 1,4))
  
  output$UIsSidePanel <- renderUI({
    uiOutputs <- list(
      # Create Inputs
      uiOutput("dbOrigin"),
      uiOutput("dbDestination"),
      conditionalPanel(
        condition = "output.cond2 == true",
        actionButton(button, h3(textImportButton),
                     align = "center")
      )
    )
    do.call(tagList, uiOutputs)
  })
  
  output$UIsMainPanel <- renderUI({
    uiOutputs <- list(#shinyjs::useShinyjs()
      textOutput("text"))#)#,
    #uiOutput(mainOutput))
    do.call(tagList, uiOutputs)
  })
  
  ###########################################################################
  #
  #                           DATABASE OF ORIGIN
  #
  ###########################################################################
  database_origin(input, output, session)
  
  ## Condition 1
  observe({
    if (!is.null(input$dbase1) && input$dbase1 != ""){
      #print("Proceed to select second database")
      # create a condition you use in the ui
      output$cond1 <- reactive({
        id0 <- which(input$dbase1 != "")
        length(id0) > 0
      })
      outputOptions(output, "cond1", suspendWhenHidden = FALSE)
    }
  })
  
  ###########################################################################
  #
  #                               DATABASE OF DESTINATION
  #
  ###########################################################################
  observe({
    # If MS-Access
    if (!is.null(input$dbase1)){
        database_destination(input, output, session)
      
    }
  })
  
  # condition2
  observe({
    if (!is.null(input$dbase2) && input$dbase2 != ""){
      # create a condition you use in the ui
      output$cond2 <- reactive({
        id0 <- which(input$dbase2 != "")
        length(id0) > 0
      })
      outputOptions(output, "cond2", suspendWhenHidden = FALSE)
    }
  })
  
  ###########################################################################
  # PRESS BUTTON
  observeEvent(input[[button]],{
    isolate({
      withProgress(message = textImportMessage, value = 0, {
        withCallingHandlers({
          shinyjs::html("text", "")
          db2.type <- input$toDbase
          db2.name <- input$dbase2
          db1.type <- input$fromDbase
          db1.name <- input$dbase1
          from_database_import(db1.type, db1.name,db2.type, db2.name)
        },
        message = function(m) {
          shinyjs::html(id = "text", html = paste(m$message, "<br>"), add = TRUE)
        })
      })
    })
  })
  
  ###########################################################################
  #
  #           CLOSE ALL CONNECTIONS
  #
  ###########################################################################
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) dbDisconnect(con)
  odbcCloseAll()
  
  # Remove temporary files when session ends
  session$onSessionEnded(function(){
    print("importApp closed")
  })
  return(output)
}
