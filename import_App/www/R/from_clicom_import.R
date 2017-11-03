from_clicom_import <- function(elementCode, filesCLICOM, db2.type, db2.name){
  
#   withCallingHandlers({
#     shinyjs::html("text", "")
#     
    #########################################################################
    #
    #                      STEP 1 - READ '.DLY' FILES
    #
    #########################################################################
    # Get climsoft Code
    #elementCode <- input$elementCode
    code2 <<- elementCode
    
    # Copy the files into the local machine
    tmpDir <<- tempdir()
    df1 <<- filesCLICOM
    path <<- df1$datapath 
    filenames <<- df1$name
    toFiles <<- file.path(tmpDir, filenames)
    file.copy(path, toFiles)
    files <<- toFiles
    
    for (i0 in c(1:length(files))){
      file.input <<- files[i0]
      file.name <<- basename(file.input) # to find out the name of the file
      incProgress(1/length(files))
      
      ############################################################
      # STATION ID
      station_id <<- strsplit(file.name, split = "\\.")[[1]][1] 
      
      ############################################################
      # GET DATA
      # Read the first line and check whether it has a elementCode or not
      line.length <- nchar(readLines(file.input, n = 1))
      if (line.length == 235){
        widths <- c(10, 1, 4, 1, 2, line.length)
      }else{
        widths <- c(8, 1, 3, 1, 4, 1, 2, line.length)
      }
      data <- read.fwf(file.input, widths)
      
      ############################################################
      # GET METADATA
      metadata <- as.data.frame(data[, c(1:length(widths)-1)])
      # remove the NA
      metadata2 <- metadata[, apply(!is.na(metadata), 2, all)]
      
      ############################################################
      # GET OBS_VALUES
      values <- data[, length(widths)]
      con <- textConnection(as.character(values))
      values2 <- as.data.frame(setNames(replicate(31, numeric(0), simplify = F), 
                                        paste0("X", c(1:31))))
      for (i in 1:nrow(metadata)){
        line <- strsplit(as.character(values[i]), " ")
        line2 <- sapply(line, as.numeric)
        line3 <- line2[!is.na(line2)]
        values2[i, ] <- line3
      }
      
      #################################################################
      # PASTE METADATA & VALUES
      data <- cbind(metadata2, values2)
      data2 <<- data
      
      #################################################################
      # DATES AND VALUES FOR DAILY DATA
      # Check the unique pairs of year and month:
      table.new <- data.frame()
      
      # Check the number of columns in data
      ncol.data <- ncol(data)
      
      if (ncol.data==35){
        data2 <- data[,-2]
        data <- data2
      }
      col_names <- paste0("V",c(1:34))
      colnames(data) <- col_names
      
      month <- c(1:12)
      hh <- 06
      
      
      for (i00 in c(1:nrow(data))){
        yyyy <- data$V2[i00]
        mm <- data$V3[i00]
        # get the number of days for each month
        time1 <- as.Date(paste(yyyy,mm,"01",sep="-"))
        if (mm <12){
          time2 <- as.Date(paste(yyyy,as.numeric(mm)+1,"01",sep="-"))
        }else{
          time2 <- as.Date(paste(as.numeric(yyyy)+1,"01","01",sep="-"))
        }
        dd <- c(1:diff(seq(time1,time2,by = "month")))
        
        var_data <- as.numeric(data[i00,c(4:(length(dd)+3))])
        
        table <- data.frame(station_id,yyyy,mm,dd,hh,var_data)
        table.new <- rbind(table.new,table)
      }
      
      # Add the "elementCode" as header of var_data;        
      colnames(table.new)[colnames(table.new)=="var_data"] <- elementCode
      
      #########################################################################
      #
      #                      STEP 2 - SAVE DATA IN '.CSV' FILES
      #
      #########################################################################
      # Name of 'csv' file
      file <- basename(files[i0])
      path <- dirname(files[i0])
      #path1 <- file.path(path, "..")
      path2 <- "csv_files"
      path.output <- file.path(tmpDir, path2)
      dir.create(path.output,showWarnings=FALSE)
      file.output <- paste(substr(file,1,8),"_daily","_",
                           sprintf("%03d",as.numeric(elementCode)),".csv",sep="")
      
      # Create 'csv' file
      write.table(table.new,file=file.path(path.output,file.output),
                  append=FALSE,quote=FALSE,
                  sep=",",col.names=TRUE,row.names=FALSE)
      
      # Message
      tt <- paste0("The File '",file,"' has been converted in .csv format")
      message(tt)
      print(tt)
    }
    
    #########################################################################
    #
    #                     STEP 3 - IMPORT .CSV TO CLIMSOFT db
    #
    #########################################################################
    # DATABASE CONNECTION
    #db2.type <- input$toDbase
    #db2.name <- input$dbase2
    db2.channel <<- odbcConnect(db2.name)
    db2.info <<- odbcGetInfo(db2.channel)
    db2.dbms <<- as.character(tolower(db2.info['DBMS_Name']))
    channel <<- db2.channel
    
    #########################################################################
    # MODIFY "mariadb" DB
    if (db2.type == "mariadb"){
      modify_mariadb(db2.channel)
      db2.dbms <- db2.type
    }
    #########################################################################
    # GET TABLE & ATTRIBUTE NAMES
    tablesInfo <- getTables(db2.type)
    for (i1 in c(1:length(tablesInfo))){
      assign(names(tablesInfo[i1]),tablesInfo[[i1]], envir = .GlobalEnv)
    }
    
    #########################################################################
    # SELECT PATH WITH '.csv' FILES
    files <- list.files(path.output, full.names = T)
    if (length(files)==0){
      message("No files have been selected")
    }else{
      message("Import has started...")
      # Attributes to be necessary for "observation" table;
      info_schema <- sqlColumns(channel, observation)
      columns <- info_schema$COLUMN_NAME
      
      #########################################################################
      # Files to import
      for (i0 in c(1:length(files))){
        file_input <- files[i0]
        rawdata <<- read.csv(file_input)
        unlink(file_input)
        # Date/time
        yyyy <- rawdata$yyyy
        mm <- sprintf("%02d",rawdata$mm)
        dd <- sprintf("%02d",rawdata$dd)
        hh <- sprintf("%02d",rawdata$hh)
        mn <- "00"
        
        # Station id
        recorded_from <- rawdata$station_id
        
        ##### CHECK IF THE STATION EXISTS
        # Check if the station is present;
        request111 <- paste("SELECT * FROM ", station, 
                            " WHERE ", station.id , " = ",
                            "'",unique(recorded_from),"'",sep="")
        result111 <- sqlQuery(channel, request111)
        if (nrow(result111)==0){
          message(paste("Station: ",unique(recorded_from), "not available in metadata"))
        }else{
          # Recorded at (date)
          recorded_at <- paste(yyyy,"-",mm,"-",dd," ",hh,":",mn,":00",sep="")
          # Elements contained in the data
          elements <- as.numeric(gsub("X","",colnames(rawdata[6:ncol(rawdata)])))
          elements <- elements[!is.na(elements)]
          # Select the dataset information
          
          i1 <- 1
          
          # dataset.new <- dataset[which(dataset$climsoft_code==elements[i1]),]
          #check.values(channel,dataset.new)
          
          # Find the element in the db, and multiply by the scalator;
          request0 <- paste("SELECT ", obs_element.element_scale, 
                            " FROM ", obs_element ,
                            " WHERE ", obs_element.code, " = ",
                            elements[i1],";",sep="")
          element_scale <- as.numeric(sqlQuery(channel, request0))
          
          described_by <- elements[i1]
          # recorded_at <- paste(yyyy,"-",mm,"-",dd," ",hh,":00:00",sep="")
          obs_value <- with(rawdata,get(paste("X",elements[i1],sep="")))/element_scale
          made_at <- "surface"
          flag <- NA
          observation <- data.frame(recorded_from,
                                    described_by,
                                    recorded_at,
                                    obs_value,
                                    made_at,
                                    flag)
          
          # Add some other attributes (e.g. captured_by)
          observation$captured_by <- Sys.getenv("USERNAME")
          
          # Rename Colnames
          newNamesVar <- paste0("observation.", colnames(observation))
          newNames <- sapply(1:length(newNamesVar), function(i){
            varNames <- get(newNamesVar[i])
            varNamesNew <- unlist(strsplit(varNames, "\\."))[2]
            varNamesNew
          })
          
          colnames(observation) <- newNames
          
          # Remove the "NA" values and place "NULL" instead
          matrix <- as.matrix(observation)
          matrix[is.na(matrix)]<-"NULL"
          a <- as.data.frame(matrix)
          id <-match(columns,colnames(a))
          assign("observation",a[colnames(a[id[is.na(id)=="FALSE"]])])
          
          # Remove the "NULL" observations in order to save space in the db
          obs_value <- unlist(strsplit(observation.obs_value, "\\."))[2]
          flag <- unlist(strsplit(observation.flag, "\\."))[2]
          id0012 <-which((observation[[obs_value]]=="NULL") ||
                           (as.numeric(as.matrix(observation[[obs_value]])) < -99))
          observation[[flag]] <- sapply(observation[[flag]],as.character)
          observation[[flag]][id0012] <- "M"
          observation[[obs_value]] <- sapply(observation[[obs_value]],as.character)
          observation[[obs_value]][id0012] <- "NULL"
          
          if (nrow(observation)==0){
            print(paste("No data for element id:",elements[i1]))
          }else{
            print(paste(basename(file_input)," is being imported"))
            insert.values(tables, db2.dbms, db2.channel, "observation", observation)
          }
          
          #file.remove(file.path(path,files))
        }
      }
      # Pop up message
      message(paste("The program has finished")) 
    }
#   },
#   message = function(m) {
#     shinyjs::html(id = "text", html = paste(m$message, "<br>"), add = TRUE)
#   })
}