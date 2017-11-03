keyEntry <- function(input, output, session){
  print("log: 'From From Key Entry From selected' option selected")
  
  ###########################################################################
  #
  #                           CREATE UI OUTPUTS
  #
  ###########################################################################
  uid <- substr(UUIDgenerate(FALSE),1, 4)
  isolate({
    # Action Buttons - uiOutputs
    okButtonId <- paste0("action_", uid)
    resetButtonId <- paste0("reset_", uid)
    
    output$UIsSidePanel <- renderUI({
      uiOutputs <- list(uiOutput("uiLoadFiles"),
                        uiOutput("dbDestination"),
                        conditionalPanel(
                          condition = "output.cond2 == true",
                          actionButton("button", h4(textLoadButton),
                                       align = "center"),
                          uiOutput("uiButton")
                        )
      )
      do.call(tagList, uiOutputs)
    })
    
    ###########################################################################
    #
    #                             SIDE PANEL
    #
    ###########################################################################
    # Load files (uiLoadFiles)
    output$uiLoadFiles <- renderUI({
      conditionalPanel(
        condition = paste0("input.dbase == '", textDbChoices03,"'"),
        fileInput("Key_entry", textChooseKeyEntry01,
                  accept=c('.xlsx', '.xls'),
                  multiple = T),
        helpText(textChooseKeyEntry02),
        tags$hr()
      )
    })
    
    #########################################################################
    # CONDITION 1: IF "KeyEntry" IS NOT EMPTY
    observe({
      if (!is.null(input$Key_entry) && input$Key_entry != ""){
        database_destination(input, output, session)
        # create a condition you use in the ui
        output$cond1 <- reactive({
          id0 <- which(input$Key_entry != "")
          length(id0) > 0
        })
        outputOptions(output, "cond1", suspendWhenHidden = FALSE)
      }
    })
    
    #########################################################################
    # CONDITION 2: IF "database" selected
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
    
    observeEvent(input$button, {
      ########################################################################
      #
      #                           CREATE MAINPANEL INPUTS
      #
      ########################################################################
      df3 <<- load_keyEntry_form(input, output, session)
      output$UIsMainPanel <- renderUI({
        Tabs <- lapply(1:2, function(i){
          tabName <- c("Data", "Info")
          tabId <- c("tab1", "tab2")
          tabPanel(tabName[i], uiOutput(tabId[i]))
        })
        do.call(tabsetPanel,Tabs)
      })
      
      output$tab1 <- renderUI({
        rHandsontableOutput("table")
      })
      
      output$uiButton <- renderUI({
        actionButton("button2", 
                     h4(textImportButton),
                     align = "right")
      })
      
      output$table <- renderRHandsontable({
        hot <- rhandsontable(df3, 
                             selectCallback = TRUE,
                             readOnly = T)
        hot
      })
    })
  })
  
  observeEvent(input$button2,{
    isolate({
      withProgress(message = textImportMessage, value = 0, {
        withCallingHandlers({
          shinyjs::html("text", "")
          db2.type <- input$toDbase
          db2.name <- input$dbase2
          db1.type <- input$fromDbase
          db1.name <- input$dbase1
          from_keyEntry_import(input,output, session, df1 = df3)
        },
        message = function(m) {
          shinyjs::html(id = "text", html = paste(m$message, "<br>"), add = TRUE)
        })
      })
    })
    
  })
  
  return(output)
}