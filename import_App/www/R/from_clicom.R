from_clicom <- function(input,output, session){
  cleanUp(input, output, session)
  print("log: 'From CLICOM' option selected")
  uid <- substr(UUIDgenerate(), 1, 4)
  button <- paste0("button_", uid)

  ###########################################################################
  #
  #                           CREATE SIDEPANEL INPUTS
  #
  ###########################################################################
  output$UIsSidePanel <- renderUI({
    uiOutputs <- list(
      # Create Inputs
      #h3("Database of origin"),
      textInput("elementCode",
                label = "Select Element Code"),
      conditionalPanel(
        condition = "input.elementCode !== ''",
        fileInput("filesCLICOM", 'Choose CLICOM File',
                  accept=c('.DLY'),
                  multiple = T),
        helpText("The file must be an '.DLY' file"),
        tags$hr()
      ),
      uiOutput("dbDestination"),
      conditionalPanel(
        condition = "output.cond2 == true",
        actionButton(button, h4("Start Import"),
                     align = "center")
      )
    )
    do.call(tagList, uiOutputs)
  })
  
  output$UIsMainPanel <- renderUI({
    uiOutputs <- list(shinyjs::useShinyjs(),
                      textOutput("text"))#,
    #uiOutput("mainOutput"))
    do.call(tagList, uiOutputs)
  })
  
  #########################################################################
  # CONDITION 1: IF "filesCLICOM" & "elementCode" ARE NOT EMPTY
  observe({
    if (!is.null(input$filesCLICOM) && input$filesCLICOM != "" && 
        !is.null(input$elementCode) && input$elementCode != ""){
      database_destination(input, output, session)
      # create a condition you use in the ui
      output$cond1 <- reactive({
        id0 <- which(input$filesCLICOM != "" && input$elementCode != "")
        length(id0) > 0
      })
      outputOptions(output, "cond1", suspendWhenHidden = FALSE)
    }
  })
  
  #########################################################################
  # CONDITION 2: IF "database" selected
  observe({
    if (!is.null(input$filesCLICOM) && input$filesCLICOM != "" && 
        !is.null(input$elementCode) && input$elementCode != "" &&
        !is.null(input$dbase2) && input$dbase2 != ""){
      # create a condition you use in the ui
      output$cond2 <- reactive({
        id0 <- which(input$dbase2 != "")
        length(id0) > 0
      })
      outputOptions(output, "cond2", suspendWhenHidden = FALSE)
    }
  })
  
  ##########################################################################
  # Press action button
  observeEvent(input[[button]], {
    isolate({
      ########################################################################
      #
      #                           CREATE MAINPANEL INPUTS
      #
      ########################################################################
      withProgress(message = textImportMessage, value = 0, {
        withCallingHandlers({
          shinyjs::html("text", "")
          elementCode <- input$elementCode
          filesCLICOM <- input$filesCLICOM
          db2.type <<- input$toDbase
          db2.name <<- input$dbase2
          from_clicom_import(elementCode, filesCLICOM, db2.type, db2.name)
        },
        message = function(m) {
          shinyjs::html(id = "text", html = paste(m$message, "<br>"), add = TRUE)
        })
      })
    })
  })
  
  return(output)
}
