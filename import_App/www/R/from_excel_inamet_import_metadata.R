from_excel_inamet_import_metadata <- function(db2.channel, db2.dbms, df11){
  # Copy the files into the local machine
  table.names.default <- c("station", "station_location", "stationid_alias")
  table.names <- sapply(1:length(table.names.default), 
                         function(i){get(table.names.default[i])}
                         )
  tmpDir <- file.path(".", "www", "tmp_inamet_meta")
  dir.create(tmpDir, showWarnings = F, recursive = T)
  path <- df11$datapath 
  filenames <- df11$name
  toFiles <- file.path(tmpDir, filenames)
  file.copy(path, toFiles)
  files <- toFiles
  inventories <- list()
  for (i0 in c(1:length(files))){
    print(table.names)
    file.input <- files[i0]
    file.name <- basename(file.input) # to find out the name of the file
    #############
    # GET THE METADATA
    file_path <- files[i0]
    # read the inventory;
    metadata <- readWorksheetFromFile(
      file_path,sheet="inventario",header=FALSE,startRow = 3)
    
    # read the headers;
    headers <- readWorksheetFromFile(
      file_path,sheet="inventario",header=FALSE,startRow = 2,endRow=2)
    
    # Convert "headers" into the name of the columns 
    # (colnames);
    todel <- as.character(headers)
    todel <- gsub("\"", "", todel)
    todel <- gsub("c\\(", "", todel)
    todel <- gsub("\\?", "", todel)
    todel <- gsub("\\?", "", todel)
    todel <- gsub(", NA", "", todel)
    todel <- gsub("\\)", "", todel)
    todel <- gsub("\\(", "", todel)
    todel <- gsub("NA, ", "", todel)
    todel <- gsub(", ", "_", todel)
    todel <- gsub(" ", "_", todel)
    todel <- gsub("\\º", "", todel)
    
    colnames(metadata) <- todel
    
    if (length(metadata$alias1>0)){
      # Remove "metacharacters" from metadata;
      metadata$alias1 <- gsub("\\(", " ", metadata$alias1)
      metadata$alias1 <- gsub("\\)", " ", metadata$alias1)
    }
    if (length(metadata$alias2>0)){
      metadata$alias2 <- gsub("\\(", " ", metadata$alias2)
      metadata$alias2 <- gsub("\\)", " ", metadata$alias2)
    }
    
    if (length(metadata$alias3>0)){
      # Remove "metacharacters" from metadata;
      metadata$alias3 <- gsub("\\(", " ", metadata$alias1)
      metadata$alias3 <- gsub("\\)", " ", metadata$alias1)
    }
    if (length(metadata$alias4>0)){
      metadata$alias4 <- gsub("\\("," ",metadata$alias2)
      metadata$alias4 <- gsub("\\)"," ",metadata$alias2)
    }
    if (length(metadata$alias5>0)){
      # Remove "metacharacters" from metadata;
      metadata$alias5 <- gsub("\\(", " ", metadata$alias1)
      metadata$alias5 <- gsub("\\)", " ", metadata$alias1)
    }
    
    ############################
    # GET THE VARIABLES FROM THE INVENTORY
    id <- metadata[,2]
    
    b <- data.frame(id)
    
    b$station_name <- metadata$nome_stacoes
    b$longitude <- NA
    b$latitude <- NA
    #############################################################
    if (is.null(metadata$lon_sec)){
      b$longitude <- sapply(1:length(metadata$lon_grad), function(i){
        # Check whether the longitude is negative
        if (!is.na(metadata$lon_grad[i]) & metadata$lon_grad[i] < 0){
          b$longitude[i] <- metadata$lon_grad[i] - (metadata$lon_min[i]/60) 
        }else{
          b$longitude[i] <- metadata$lon_grad[i] + (metadata$lon_min[i]/60)
        }
      })
    }else{
      # Check whether the longitude is negative
      b$longitude <- sapply(1:length(metadata$lon_grad), function(i){
        if (metadata$lon_grad[i] < 0){
          b$longitude[i] <- metadata$lon_grad[i] - (metadata$lon_min[i]/60) - 
            (metadata$lon_sec[i]/3600) 
        }else{
          b$longitude[i] <- metadata$lon_grad[i] + (metadata$lon_min[i]/60) + 
            (metadata$lon_sec[i]/3600)
        }
      })
    }
    #############################################################
    # LATITUDE
    # Check if "seconds" of latitude exist
    if (is.null(metadata$lat_sec)){
      b$latitude <- sapply(1:length(metadata$lat_grad), function(i){
        # Check whether the latitude is negative
        if (!is.na(metadata$lat_grad[i]) & metadata$lat_grad[i] < 0){
          b$latitude[i] <- metadata$lat_grad[i] - (metadata$lat_min[i]/60) 
        }else{
          b$latitude[i] <- metadata$lat_grad[i] + (metadata$lat_min[i]/60)
        }
      })
    }else{
      # Check whether the latitude is negative
      b$latitude <- sapply(1:length(metadata$lat_grad), function(i){
        if (metadata$lat_grad[i] < 0){
          b$latitude[i] <- metadata$lat_grad[i] - (metadata$lat_min[i]/60) - 
            (metadata$lat_sec[i]/3600) 
        }else{
          b$latitude[i] <- metadata$lat_grad[i] + (metadata$lat_min[i]/60) + 
            (metadata$lat_sec[i]/3600)
        }
      })
    }
    
    b$district <- metadata$provincia
    b$country <- "ANGOLA"
    b$authority <- "INAMET"
    b$occupied_by <- id
    b$refers_to <- id
    b$id_alias <- metadata$wmo_id
    b$belongs_to <- "wmo_id"
    b$begin_datetime <- format(as.Date(ISOdate(metadata$inicio,1,1)),
                               "%d-%m-%Y")
    b$end_datetime <- format(as.Date(ISOdate(metadata$fim,1,1)),
                             "%d-%m-%Y")
    b$elevation <- metadata$altitude_m
    
    # Remove the "NA" values and place "NULL" instead;
    matrix <- as.matrix(b)
    matrix[is.na(matrix)]<-"NULL"
    b <- as.data.frame(matrix)
    
    for (i00 in 1:length(table.names)){
      table.name <- table.names[i00]
      table.name.default <- table.names.default[i00]
      
      info.schema <- sqlColumns(db2.channel, table.name)
      columns <- info.schema$COLUMN_NAME
      
      # Rename Colnames
      #newNamesVar <- paste0(table.name.default,".", colnames(b))
      newNamesVar <- ls(pattern = paste0("^", table.name.default,"\\."), 
                         envir = .GlobalEnv)
      print(table.name.default)
      print(newNamesVar)
      oldNamesVar <- paste0(table.name.default, ".", colnames(b))
      id00 <- match(newNamesVar, oldNamesVar)
      id000 <- id00[!is.na(id00)]
      b.new <- b[, id000]
      
      colnames(b.new) <- oldNamesVar[id000]
      
      newNames <- sapply(1:ncol(b.new), function(i){
        varNames <- get(colnames(b.new)[i])
        varNamesNew <- unlist(strsplit(varNames, "\\."))[2]
        varNamesNew
      })
      colnames(b.new) <- newNames
      
      # CREATE THE TABLES
      # Create an empty vector for each attribute;
      for (col.name in columns){
        assign(col.name,vector())
      }
      
      id0 <-match(columns, colnames(b.new))
      assign(table.name, b.new[colnames(b.new[id0[is.na(id0)=="FALSE"]])])
      
      # INSERT TABLES IN THE DATABASE
      insert.values(tables, db2.dbms, db2.channel, table.name.default, get(table.name))
      
      # Create list with all the inventories
      inventories[[i0]] <- b.new
      names(inventories)[[i0]] <- paste0("inventory_", i0)
    }
  }
}