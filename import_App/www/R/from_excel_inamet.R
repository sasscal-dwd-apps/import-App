from_excel_inamet <- function(input,output, session){
  cleanUp(input, output, session)
  print("log: 'From INAMET Excel Files' option selected")
  button <<- paste0("button_", substr(UUIDgenerate(), 1, 4))
  
  ###########################################################################
  #
  #                           CREATE SIDEPANEL INPUTS
  #
  ###########################################################################
  output$UIsSidePanel <- renderUI({
    uiOutputs <- list(
      fileInput("fileInventory", textChooseInventory, accept=c('.xls'), multiple = T),
      helpText(textChooseInventoryHelp),
      conditionalPanel(
        condition = "input.fileInventory !== ''",
        fileInput("filesExcel", textChooseFiles, accept=c('.xls'), multiple = T),
        helpText(textChooseFilesHelp)
      ),
      uiOutput("dbDestination"),
      conditionalPanel(
        condition = "output.cond2 == true",
        uiOutput("uiButton")
      )
    )
    do.call(tagList, uiOutputs)
  })
  
  #########################################################################
  # CONDITION 1: IF "fileInventory" & "filesExcel" ARE NOT EMPTY
  observe({
    if (!is.null(input$filesExcel) && input$filesExcel != "" && 
        !is.null(input$fileInventory) && input$fileInventory != ""){
      database_destination(input, output, session)
      # create a condition you use in the ui
      output$cond1 <- reactive({
        id0 <- which(input$filesExcel != "" && input$fileInventory != "")
        length(id0) > 0
      })
      outputOptions(output, "cond1", suspendWhenHidden = FALSE)
    }
  })
  #########################################################################
  # CONDITION 2: IF "database" selected
  observe({
    if (!is.null(input$filesExcel) && input$filesExcel != "" && 
        !is.null(input$fileInventory) && input$fileInventory != "" && 
        !is.null(input$dbase2) && input$dbase2 != ""){
      # create a condition you use in the ui
      output$cond2 <- reactive({
        id0 <- which(input$dbase2 != "")
        length(id0) > 0
      })
      outputOptions(output, "cond2", suspendWhenHidden = FALSE)
      output$uiButton <- renderUI({
        actionButton(button, h4(textImportButton),
                     align = "center")
      })
      print(paste0("1:", button))
    }else{
      output$uiButton <- renderUI({})
    }
  })
  
  observeEvent(input[[button]], {
    ########################################################################
    #
    #                           CREATE MAINPANEL INPUTS
    #
    ########################################################################
    output$UIsMainPanel <- renderUI({
      uiOutputs <- list(shinyjs::useShinyjs(),
                        textOutput("text"),
                        uiOutput("mainOutput"))
      do.call(tagList, uiOutputs)
    })
    
    db2.type <- input$toDbase
    db2.name <- input$dbase2
    db2.channel <- odbcConnect(db2.name)
    db2.info <- odbcGetInfo(db2.channel)
    df1 <- input$filesExcel
    df11 <- input$fileInventory
    output$mainOutput <- renderUI({
      withProgress(message = textImportMessage, value = 0, {
        # START IMPORT
        from_excel_inamet_import(db2.type, db2.name, db2.channel, df1, df11)
      })
    })
  })
  return(output)
}
