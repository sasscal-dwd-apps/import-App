from_excel_inamet_import <- function(db2.type, db2.name, db2.channel, df1, df11){
  
  withCallingHandlers({
    shinyjs::html("text", "")
    #########################################################################
    # MODIFY "mariadb" DB
    if (db2.type == "mariadb"){
      modify_mariadb(db2.channel)
    }
    db2.dbms <- db2.type
    
    #########################################################################
    # GET TABLE & ATTRIBUTE NAMES
    tablesInfo <- getTables(db2.type)
    for (i1 in c(1:length(tablesInfo))){
      assign(names(tablesInfo[i1]), tablesInfo[[i1]], envir = .GlobalEnv)
    }
    
    #########################################################################
    # STEP 1 - READ & IMPORT 'inventory' FILES
    from_excel_inamet_import_metadata(db2.channel, db2.dbms, df11)
    
    #########################################################################
    # STEP 2 - READ & IMPORT 'xls' FILES
    from_excel_inamet_import_data(db2.channel, db2.dbms, df1)
    
    #########################################################################
    # STEP 3 - REMOVE TMP FILES
    id <- grep("tmp_inamet", dir("www"))
    dirs <- dir("www", full.names = T)[id]
    print("Proceeding to delete temporary directories...")
    unlink(dirs, recursive = T)
    print("Delete process succeeded")
  },
  message = function(m) {
    shinyjs::html(id = "text", html = paste(m$message, "<br>"), add = TRUE)
  })
}