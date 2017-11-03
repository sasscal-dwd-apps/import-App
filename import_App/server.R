###########################################################################
#
#                           IMPORT standalone server
#
###########################################################################
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  ###################################################################
  observe({
    # Create Progress bar ("Loading")
    withProgress(message = "Loading...", value = 0, {
      # Remove unused variables
      rm(list= ls(all=TRUE)[!(ls() %in% c('input','output', 'session'))])
      
      # A) From a database
      observe({
        if (input$dbase==textDbChoices01){
          output <- from_database(input, output, session)
        }
      })
      
      # B) From CLICOM
      observe({
        if (input$dbase==textDbChoices04){
          from_clicom(input, output, session)
        }
      })
      
      # C) From an Excel File
      observe({
        if (input$dbase==textDbChoices02){
          from_excel_inamet(input, output, session)
        }
      })
      
      # D) From a Form
      observe({
        if (input$dbase==textDbChoices03){
          #output$UIsMainPanel <- renderUI({})
          keyEntry(input, output, session)
        }
      })
      
      # Remove temporary files when session ends
      session$onSessionEnded(function(){
        print("Import-App closed")
      })
    })
  })
})
