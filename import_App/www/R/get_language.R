##########################################################################
#' @export get_language
#'
#' @title Translation of the 'Import-App'
#'
#' @description Used to translate the 'Import-App' into English or Portuguese.
#'
#' @param language character. Language under which the 'Import-App' has to run.
#' There are two possible options: \code{'english'} or \code{'portuguese'}.
#' Usually the 'ACD-App' will detect the language of the local machine
#' automatically and set up the text of the App accordingly.
#'
#' @return textInfo list. A list containing all the text used in the App.
#' The text is saved in different R objects which names begin with the word
#' \code{text}. E.g. \code{textMap00} contains the text that has to be use
#' to label the input \code{checkMap}.
#' @details This function is run directly by the \code{ui.R} file of the
#' 'ACD-App'.
#'
#' @author Rafael Posada (SASSCAL/DWD), November 2016
#'
get_language <- function(language){
  
  ############################################################################
  #
  #                         ENGLISCH
  #
  ############################################################################
  if (language == "english"){
    ##########################################################################
    # Side Panel
    textTitlePanel <- "Import into CLIMSOFT (Import_App)"
    textSidePanel00 <- "Import Options"
    
    textChooseInventory <- "Choose Inventory"
    textChooseInventoryHelp <- "The file(s) must in '.xls' format" 
    textChooseFiles <- "Choose file(s) to import"
    textChooseFilesHelp <- "The file(s) must in '.xls' format" 
    textProcess <- "Upload complete"
    
    textDbOrigin <- "Database of origin"
    textDbDestination <- "Database of destination"
    textDbType <- "Type of database:"
    textDNS <- "Data Name Source (DNS):"
    
    textImportButton <- "Start Import"
    textLoadButton <- "Load Data"
    
    ########################################################################## 
    # Main Panel
    textImportMessage <- 'Importing data...'
    textImporting <- "Importing from: "
    textImportingFrom <- "from: "
    textImportingTo <- "to: "
    textImportWarning01 <- 'There is already a record in the database:'
    textImportWarning02 <- 'Do you want to replace'
    textImportWarning03 <- "with"
    textImportWarning04 <- " - One entry to "
    textImportWarning05 <- " does already exist\n"
    textImportAnswer <- "Please type: 'yes' or 'no'"
    textImportFinalMessage <- "Import completed"
    textSidePanel01 <- "From:"
    textSidePanel02 <- "Select database:"
    textSidePanel03 <- "Select database (DNS):"
    textConnectButton <- "Connect"
    textDisconnectButton <- "Disconnect"
    textLoginUser <- "Username:"
    textLoginPassword <- "Password:"
    textClearButton <- "Clear"
    textOKButton <- "OK"
    textConnectMessage01 <- "Connected!"
    textConnectMessage02 <- "Connection failed!"
    textDisconnectMessage <- "Disconnected!"
    textODBCMessage <- "ODBC connection not successfull"
    textWrongLogin <- "Wrong username/password"
    
    ##########################################################################  
    # Database choices
    textDbChoices01 <- "From a database"
    textDbChoices02 <- "From INAMET Excel files"
    textDbChoices03 <- "From a key-entry Form"
    textDbChoices04 <- "From CLICOM"
    textChooseKeyEntry01 <-"Choose Key_Entry File"
    textChooseKeyEntry02 <-"The file has to be an Excel file (.xlsx)" 
  }
  
  ############################################################################
  #
  #                         PORTUGUESE
  #
  ############################################################################
  if (language == "portuguese"){
    # Side Panel
    textTitlePanel <- "Importe para CLIMSOFT (Import_App)"
    textSidePanel00 <- "Op\u{00E7}\u{00F5}es de importa\u{00E7}\u{00E3}o"
    
    textChooseInventory <- "Escolha o invent\u{00E1}rio"
    textChooseInventoryHelp <- "Os arquivos devem ser no formato '.xls'"
    textChooseFiles <- "Escolha os arquivos para importar"
    textChooseFilesHelp <- "Os arquivos devem ser no formato '.xls'"
    textProcess <- "Download completa"
    
    textDbOrigin <- "Base de dados de origem"
    textDbDestination <- "Banco de dados do destino"
    textDbType <- "Tipo de banco de dados:"
    textDNS <- "Data Name Source (DNS):"
    
    textImportButton <- "Iniciar Importa\u{00E7}\u{00E3}o"
    textLoadButton <- "Carregar dados"
    ############################################################### 
    # Main Panel
    textImportMessage <- 'Importando dados ...'
    textImporting <- "Importando de:"
    textImportingFrom <- "desde: "
    textImportingTo <- "para: "
    textImportWarning01 <- 'J\u{00E1} existe um registro no banco de dados:'
    textImportWarning02 <- 'Voc\u{00EA} deseja substituir'
    textImportWarning03 <- "com"
    textImportWarning04 <- "- Uma entrada para"
    textImportWarning05 <- "j\u{00E1} existe \ n"
    textImportAnswer <- "Por favor, digite: 'yes' ou 'no'"
    textImportFinalMessage <- "Importa\u{00E7}\u{00E3}o conclu\u{00ED}da"
    textSidePanel01 <- "De:"
    textSidePanel02 <- "Selecionar banco de dados:"
    textSidePanel03 <- "Selecionar banco de dados (DNS):"
    textConnectButton <- "Conectar"
    textDisconnectButton <- "Desconectar"
    textLoginUser <- "Nome de usu\u{00E1}rio:"
    textLoginPassword <- "Password:"
    textClearButton <- "Limpar"
    textOKButton <- "OK"
    textConnectMessage01 <- "Conectado!"
    textConnectMessage02 <- "Falha na conex\u{00E3}o!"
    textDisconnectMessage <- "Desconectado!"
    textODBCMessage <- "Conex\u{00E3}o ODBC n\u{00E3}o bem sucedida"
    textWrongLogin <- "Nome de usu\u{00E1}rio / senha errados"
    
    textSidePanel02 <- "Selecionar banco de dados:"
    textSidePanel03 <- "Selecionar banco de dados (DNS):"
    textConnectButton <- "Conectar"
    textDisconnectButton <- "Disconnect"
    textLoginUser <- "Usu\u{00E1}rio:"
    textLoginPassword <- "Password:"
    textClearButton <- "Limpar"
    textOKButton <- "OK"
    textConnectMessage01 <- "Conectado!"
    textConnectMessage02 <- "Falha na conex\u{00E3}o!"
    textDisconnectMessage <- "Desconectado!"
    textODBCMessage <- "Conex\u{00E3}o ODBC sem sucesso"
    textWrongLogin <- "Nome de usu\u{00E1}rio e senha errados"
    
    ##########################################################################  
    # Database choices
    textDbChoices01 <- "A partir de um banco de dados"
    textDbChoices02 <- "A partir de arquivos antigos do INAMET"
    textDbChoices03 <- "A partir de um formulario"
    textDbChoices04 <- "A partir do CLICOM"
    textChooseKeyEntry01 <- "Selecione o formulario"
    textChooseKeyEntry02 <- "O arquivo deve ser um arquivo do Excel (.xlsx)"
  }
  
  variables <- grep("text", ls(), value=TRUE)
  textInfo <- vector("list", length(variables))
  names(textInfo) <- variables
  for (ii in 1:length(variables)){
    textInfo[[ii]] <- get(variables[ii])
  }
  return(textInfo)
}
