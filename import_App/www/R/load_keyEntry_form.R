load_keyEntry_form <- function(input, output, session) {
  print("log: 'Key_Entry import function started")
  
  
  tmpDir <<- file.path(".", "www", "tmp_files")
  if(!dir.exists(tmpDir)){
    dir.create(tmpDir)
  }
  df1 <<- input$Key_entry
  path <<- df1$datapath 
  filenames <<- df1$name
  print(path)
  toFiles <<- file.path(tmpDir, filenames)
  file.copy(path, toFiles, overwrite = T)
  print(path)
  files <- toFiles
  wb <<- XLConnect::loadWorkbook(files)
  unlink(files)
  metadata <- readWorksheet(wb,1)
  raw.data   <<- readWorksheet(wb,2)
  limits <<- readWorksheet(wb, "limits")
  
  
  # delete Max. Min. Sum and mean from rows
  raw.data   <<- raw.data[-c(which(raw.data[,1]=="MAX."),which(raw.data[,1]=="MIN.")
                             ,which(raw.data[,1]=="SUM"),which(raw.data[,1]=="MEAN")),]
  raw.time.scale <<- raw.data[,1]
  obs.table <<-raw.data[,2:length(raw.data[1,])]
  
  climsoft.data <- readWorksheet(wb,4)
  
  ###################################################################################
  ## check if there are climsoft elements defined for the data
  
  temp.df<-data.frame("element_abbr" = NA ,"climsoft_code" = NA)
  for(i001 in 1:length(climsoft.data[,1])){
    if(is.na(climsoft.data$climsoft_code[i001])){
      temp.df<-rbind(temp.df,c(climsoft.data$element_abbr[i001],climsoft.data$climsoft_code[i001]))
    }
  }
  
  temp.df<-temp.df[2:(length(temp.df[,1])),]
  
  print(paste("No climsoft code has been defined for the following elements: ",paste(temp.df$element_abbr, collapse = ",")))
  
  ###################################################################################
  #rename data with corresponding climsoft code
  names(obs.table) <<-  climsoft.data$climsoft_code
  scale_factors <<- limits$scaleFactor
  
  ###################################################################################
  #remove all entries where climsoft code is not definded 
  id <- which(!is.na(names(obs.table)))
  obs.table <<- obs.table[, id]
  times <<- climsoft.data$time[id]
  scale_factors <<- scale_factors[id]
  
  ###################################################################################
  #get meta information 
  station.id <<-  metadata$value[which(metadata$inputId == "id")]
  station.ident <<- strsplit(station.id, split = " - ")[[1]][1]
  made_at <<- "surface"
  acquisition <<- as.character(metadata$value[which(metadata$inputId == "form")])
  captured_by <<- metadata$value[which(metadata$inputId == "captured_by")]
  ## need to check if the 2400 value belongs to the date or to following day
  observation1 <-data.frame(recorded_from = NA, # done
                           described_by = NA,
                           recorded_at = NA,
                           obs_value = NA,
                           made_at = NA,
                           captured_by = NA,
                           data_form = NA)
  
  
  ############################################################################
  #  
  #                               DAILY FORMS
  #
  ############################################################################
  if(metadata$value[which(metadata$inputId == "form_type")]=="daily"){
    print("daily")
    yyyy <- metadata$value[which(metadata$inputId == "yyyy")]
    mm  <-  metadata$value[which(metadata$inputId == "mm")]
    dd  <-  raw.time.scale
    Datetime<<-paste(yyyy,mm,dd,sep="-")
    
    ##########################################################################
    #  
    #                               HOURLY FORMS
    #
    ##########################################################################
  }else if(metadata$value[which(metadata$inputId == "form_type")]=="hourly"){
    print("hourly")
    yyyy <- metadata$value[which(metadata$inputId == "yyyy")]
    mm  <-  metadata$value[which(metadata$inputId == "mm")]
    dd <- metadata$value[which(metadata$inputId == "dd")]
    obsDate<-paste(yyyy,mm,dd, sep = "-")
    
    time.seq<- sort(seq(
      from=as.POSIXlt(paste(obsDate,"01:00:00"), tz=""),
      to=as.POSIXlt(paste(obsDate,"24:00:00")), tz="", by = "1 hour")) 
    Datetime<<-as.character(time.seq[1:24])
    ##########################################################################
    #  
    #                               MONTHLY FORMS
    #
    ##########################################################################
  }else if(metadata$value[3]=="monthly"){
    
    
  }else{
    print("No type of form could be identified")
    return()
  }
  
  ############################################################################
  #
  #           BUILD DATAFRAME TO WRITE INTO THE DB
  #
  ############################################################################
  k <- 1
  
  for(i1 in 1:(length(names(obs.table)))){
    ##########################################################################
    # check if there is a specific time to the value, and if so replace this value
    if(metadata$value[which(metadata$inputId == "form_type")]=="daily"){
      if(is.na(times[i1])){
        hours <-"00:00:00"
      }
      else{
        hours <-paste(times[i1],":00",sep="")
        
        #Datetime<-gsub("00:00:00",paste(climsoft.data$time[i1],":00",sep=""),as.character(obs.table$Datetime))
      }
      if(is.factor(Datetime)){
        Datetime<-as.character(Datetime)
        print("datetime is factor!")
      }
      Datetime1 <- c()
      for(i in 1:length(Datetime)){
        if(!is.na(Datetime[i])||hours){
          Datetime1[i]<-paste(Datetime[i],hours,sep=" ")
        }else{
          print(str(Datetime))
          break
        }
      }
      
    }else{
      Datetime1<<-Datetime
    }
    
    for(j1 in 1:length(obs.table[,1])){
      datum <<-Datetime1[j1]
      if(is.na(datum)){
        print("Datum incorrect")
        break
      }
      
      if(!is.na(obs.table[j1,i1]) && is.numeric(obs.table[j1, i1])){
        print(paste0("log: ", obs.table[j1, i1], "is a NA value"))
        codes <<- floor(as.numeric(names(obs.table)))
        print(codes)
        obs.entry<<-c(station.ident, codes[i1],
                      datum,obs.table[j1,i1]*scale_factors[i1], 
                      made_at, captured_by,acquisition)
        print(head(obs.entry))
        print(i1)
        print(j1)
        #print(obs.entry)
        observation1[(k),] <- obs.entry
        k=k+1
      }
    }
  }
  rm(k)
  #########################################################################
  #
  #                     IMPORT data TO CLIMSOFT db
  #
  #########################################################################
  
  # DATABASE CONNECTION
  db2.type <<- input$toDbase
  db2.name <<- input$dbase2
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
  tablesInfo <- getTables(db2.type)
  for (i1 in c(1:length(tablesInfo))){
    assign(names(tablesInfo[i1]),tablesInfo[[i1]], envir = .GlobalEnv)
  }
  
  # Rename Colnames
  newNamesVar <- paste0("observation.", colnames(observation1))
  newNames <- sapply(1:length(newNamesVar), function(i){
    varNames <- get(newNamesVar[i])
    varNamesNew <- unlist(strsplit(varNames, "\\."))[2]
    varNamesNew
  })
  #temp1<<-observation1
  colnames(observation1) <- newNames
  #observation1<<-na.omit(observation1)
  ##  import into database
  print(paste(station.ident," is loaded for uploading to database"))
  observation2 <<- observation1
  
  # Multiply by "scale_factor"
  
  
  return(observation1)
}
