
cleanUp <- function(input, output, session){
  # Remove all variables beside "input", "output", and "session"
  ls()[!(ls() %in% c('input','output', 'session'))]
#   updateSelectInput(session, "dbase1", textDNS, NULL)
#   updateSelectInput(session, "dbase2", textDNS, "")
  output$dbDestination <- renderUI({})
  output$dbOrigin <- renderUI({})
  output$UIsSidePanel <- renderUI({})
  
  output$UIsMainPanel <- renderUI({})
  return(output)
}
