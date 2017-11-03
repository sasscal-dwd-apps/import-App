from_excel_inamet_import_data <- function(db2.channel, db2.dbms, df1){
  ############################################################
  # Copy the files into the local machine
  tmpDir <- normalizePath(file.path(".", "www", "tmp_inamet_data"))
  csvDir <- file.path(".", "www", "tmp_inamet_csv")
  dir.create(tmpDir, showWarnings = F, recursive = T)
  path <- df1$datapath 
  filenames <- df1$name
  toFiles <- file.path(tmpDir, filenames)
  file.copy(path, toFiles)
  files_prev <- toFiles
  
  # Rename the files selected
  files <- rename_inamet_files(files_prev)
  
  # Remove old files selected
  unlink(files_prev)
  dataValues <- list()
  for (i0 in c(1:length(files))){
    file.input <- files[i0]
    file.name <- basename(file.input) # to find out the name of the file
    incProgress(1/length(files))
    
    ############################################################
    # STATION ID
    start <- gregexpr("INA",file.name)[[1]][1]
    stop <- start+5
    station_id <- substr(file.name,start, stop)
    
    ############################################################
    # PERIOD
    # Find out which period (either "monthly" or "daily")
    wb <- loadWorkbook(file.input)
    tmp <- readWorksheet(wb,sheet=1)
    
    # Find the adequate headers
    patterns <- c("Ano","Dias")
    id000 <- which(apply(tmp,1,function(x) any(grepl(patterns[1],x))))
    if (length(id000)==0){
      id000 <- which(apply(tmp,1,function(x) any(grepl(patterns[2],x))))
      if(length(id000)==0){}
    }else{
      colnames(tmp) <- tmp[id000,]
      # Remove the row that has the headers
      tmp <- tmp[-id000,]
    }
    # Remove unused columns
    id000 <- which(is.na(colnames(tmp)) || colnames(tmp)=="NA" || colnames(tmp)=="Col1")
    if (length(id000)>0){
      tmp <- tmp[,-id000]
    }
    
    ############################################################
    # DAILY
    if (colnames(tmp)[1]=="Dias"){
      period <- "diario"
      pattern <- "Dias"
      ano <- substr(file.name,8,nchar(file.name)-4)
      
      file_output <- paste(station_id,"_",ano,"_",period,".csv",sep="")
      variables <- c("Precipitacao","Temp_max","Temp_min","Temp_media",
                     "Pressao","Humidade","Vento_velocidade","Vento_direcao",
                     "Vento_velocidade_max","Vento_forca","Radiacao_solar",
                     "Insolacao","Evaporacao",
                     "Punto_rocio","Pressao_nm","Visibilidad")
      climsoft_code <- c(5,2,3,4,
                         47,17,56,1019,
                         60,58,1042,
                         1039,18,
                         14,48,110)
    }
    
    ############################################################
    # MONTHLY
    if (colnames(tmp)[1]=="Ano"){
      # Name of the variables to be saved
      period <- "mensal"
      pattern <- "Ano"
      file_output <- paste(station_id,"_",period,".csv",sep="")
      variables <- c("Precipitacao","Temp_max","Temp_min","Temp_media",
                     "Pressao","Humidade","Vento_velocidade","Vento_direcao",
                     "Radiacao_solar","Insolacao","Evaporacao",
                     "E_potencial_mm","Temp_max_abs","Temp_min_abs",
                     "Precipitacao_dias","Pressao_nm","Pressao_vapor","Geopotencial",
                     "Quintil","Temp_std")
      
      climsoft_code <- c(1003,1013,1011,1001,
                         1010,1006,1110,1109,
                         1041,1008,1016,
                         1203,1014,1012,
                         1002,1004,1005,1007,
                         1009,1015)
    }
    var_table <- data.frame(variables,climsoft_code)
    
    ############################################################
    # Get the data for each variable;
    for (i4 in c(1:length(variables))){
      if (existsSheet(wb,as.character(var_table$variables[i4]))){
        data_prev <- readWorksheet(wb,sheet=as.character(var_table$variables[i4]))
        #print(i4)
        # Replace "," by "."
        data_prev <- sapply(data_prev,gsub,pattern = ",",replacement= ".")
        
        # Find the adequate headers
        id000 <- which(apply(data_prev,1,function(x) any(grepl(pattern,x))))
        if (length(id000)==0){
        }else{
          colnames(data_prev) <- data_prev[id000,]
          # Remove the row that has the headers
          data_prev <- data_prev[-id000,]
        }
        
        # Remove unused columns
        id000 <- which(is.na(colnames(data_prev))|| colnames(data_prev)=="NA")
        if (length(id000)==0){}else{
          data_prev <- data_prev[,-id000]
        }
        id000 <- which(grepl("Col",colnames(data_prev))==T)
        if (length(id000)==0){}else{
          data_prev <- data_prev[,-id000]
        }
        data <- data_prev
        id00 <- which(!is.na(as.numeric(as.character(data[,1]))))
        if (length(id00)>0){
          data2 <- data[id00,]
          data <- as.data.frame(data2)
          data3 <- data
          code <- var_table$climsoft_code[i4]
          month <- c(1:12)
          hh <- 0
          # Take the dates only for the first element
          date <- data.frame()
          var_data <- c()
          
          #################################################################
          # DATES AND VALUES FOR MONTHLY DATA
          if (period=="mensal"){
            yyyy <- as.numeric(as.matrix(data$Ano))
            dd <- 1
            for (mm in month){
              date <- rbind(date,data.frame(station_id,yyyy,mm,dd,hh))
              var_data <- c(var_data,as.numeric(as.matrix(data[,mm+1])))
            }
            
            # Since the data available are not for the exact same dates,
            # it is necessary to make sure that the whole time variability
            # is covered
            time.seq <- seq(from=as.Date("1900-01-01"),to=Sys.Date(),by="month")
            #print(length(time.seq))
            date2 <- as.Date(paste(date[,2],# year
                                   sprintf("%02d",date[,3]), # month
                                   sprintf("%02d",date[,4]), # day
                                   sep="-"))
            all.dates.frame <- data.frame(list(date2=time.seq))
            merged.data <- merge(all.dates.frame,data.frame(date2,var_data),all=T)
            yyyy <- format(merged.data$date2,"%Y")
            mm <- format(merged.data$date2,"%m")
            dd <- format(merged.data$date2,"%d")
            date <- data.frame(station_id,yyyy,mm,dd,hh)
            var_data <- merged.data$var_data
          }
          
          #################################################################
          # DATES AND VALUES FOR DAILY DATA
          if (period=="diario"){
            file.name_prev <- strsplit(file.name,"\\.")[[1]][1]
            yyyy <- as.numeric(substr(file.name,8,11))
            
            for (mm in month){
              # get the number of days for each month
              time1 <- as.Date(paste(yyyy,mm,"01",sep="-"))
              if (mm <12){
                time2 <- as.Date(paste(yyyy,as.numeric(mm)+1,"01",sep="-"))
              }else{
                time2 <- as.Date(paste(as.numeric(yyyy)+1,"01","01",sep="-"))
              }
              dd <- c(1:diff(seq(time1,time2,by = "month")))
              
              # date
              date <- rbind(date,data.frame(station_id,yyyy,mm,dd,hh))
              # Values of the element
              var_data <- c(var_data,as.numeric(as.matrix(data[dd,mm+1])))
            }
          }
          
          if (i4 == 1){
            table <- data.frame(date,var_data)
            colnames(table)[colnames(table)=="var_data"] <- code
          }else{
            table <- cbind(table,var_data)
            colnames(table)[colnames(table)=="var_data"] <- code
          }
        }else{
          print(paste("No data for",var_table$variables[i4],"in station",
                      station_id))
        }
      }
    }
    #######################################################################
    # REMOVE RECORDS
    # Remove records that do not have any data
    table3 <- table[,6:length(table)]
    if (is.null(table3)){
      table.new <- NULL
    }else{
      if(ncol(table)==6){
        table.new <- table[!is.na(table[,6]),]
      }else{
        table.new <- table[apply(table3,1,function(x) any(!is.na(x))),]
      }
    }
    
    #######################################################################
    # SAVE THE DATA IN AN CSV FILE
    # path_output <- file.path(path_prev,"dados_csv")
    path_output <- csvDir
    if (file.exists(path_output)){
    }else{
      dir.create(path_output,recursive=TRUE,showWarnings=FALSE)
    }
    # Save the observation values in ".csv" file;
    write.table(table.new,file.path(path_output,file_output),
                row.names=FALSE,append=FALSE,sep=",")
    print(paste(file_output,"has been created"))    
    file.name <- basename(file.input) # to find out the name of the file
    #incProgress(1/length(files))
    
  }
  
  #########################################################################
  #
  #                     STEP 3 - IMPORT .CSV TO CLIMSOFT db
  #
  #########################################################################
  # SELECT PATH WITH '.csv' FILES
  
  files <- list.files(csvDir, full.names = T)
  if (length(files)==0){
    print("No files have been selected")
  }else{
    # Attributes to be necessary for "observation" table;
    info_schema <- sqlColumns(db2.channel, observation)
    columns <- info_schema$COLUMN_NAME
    
    #########################################################################
    # Files to import
    for (i0 in c(1:length(files))){
      file_input <- files[i0]
      rawdata <- read.csv(file_input)
      
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
      result111 <- sqlQuery(db2.channel, request111)
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
        
        # Find the element in the db, and multiply by the scalator;
        request0 <- paste("SELECT ", obs_element.element_scale, 
                          " FROM ", obs_element ,
                          " WHERE ", obs_element.code, " = ",
                          elements[i1],";",sep="")
        element_scale <- as.numeric(sqlQuery(db2.channel, request0))
        
        described_by <- elements[i1]
        # recorded_at <- paste(yyyy,"-",mm,"-",dd," ",hh,":00:00",sep="")
        obs_value <- with(rawdata,get(paste("X",elements[i1],sep="")))/element_scale
        made_at <- "surface"
        observation <- data.frame(recorded_from,
                                  described_by,
                                  recorded_at,
                                  obs_value,
                                  made_at)
        
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
        id0012 <-which((observation[[obs_value]]=="NULL") |
                         (as.numeric(as.matrix(observation[[obs_value]])) < -99))
        observation[[obs_value]] <- sapply(observation[[obs_value]],as.character)
        observation[-id0012,]
        
        
        
        if (nrow(observation)==0){
          print(paste("No data for element id:",elements[i1]))
        }else{
          observation2 <- observation
          print(paste(basename(file_input)," is being imported"))
          insert.values(tables, db2.dbms, db2.channel, "observation", observation)
        }
      }
    }
    # Pop up message
    print(paste("The program has finished")) 
  }
}