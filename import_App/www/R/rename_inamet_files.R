rename_inamet_files <- function(list = NULL){
  # get the old path
  path_old <- getwd()
  # Select the files to be renamed
  if (is.null(list)){
    list = choose.files(caption = "Select the files to be renamed")
  }
  # Get the path where the files are located
  path = unique(dirname(list))
  path_new = file.path(path_old, "www", "tmp_inamet_xls")
  dir.create(path_new, showWarnings = F, recursive = T)
  setwd(path_new)
  
  # Rename automatically
  for (i in c(1:length(list))){
    list[i]
    list1 = list[i]
    
    # Clean up list1
    list1 = gsub(pattern = "-",replacement = "",x = list1)
    list1 = gsub(pattern = " ",replacement = '',x = list1)
    list1 = gsub(pattern ='.xls',replacement ='',x= list1)
    list1 = gsub(pattern ='x',replacement ='',x= list1)
    
    # Get the station_id
    initial = regexpr(pattern = "INA",text = list1)[1]
    final =  initial+5
    station_id = substr(x = list1,start = initial,stop = final)
    
    # Get year
    initial = nchar(list1)-3
    final =  initial+3
    year = substr(x=list1,start=initial,stop=final)
    
    # Generate the new file name (list2)
    list2 = paste0(station_id,"_",year,'.xls')
    
    # Check if the file already exists
    list3 <- paste0(station_id,"_",year)
    list4 <- dir()
    id <- which(grepl(list3,list4))
    if (length(id)>0){
      list2 = paste0(station_id,"_",year,"_",toupper(letters[length(id)+1]),'.xls')
    }
    
    # Rename
    print("copying...")
    file.copy(from=list[i],to = file.path(path_new,list2))
    print(list[i])
    print(file.path(path_new,list2))
    print(list2)
  }
  #unlink(list)
  print(path_new)
  list_new <- list.files(path_new, full.names = T)
  setwd(path_old)
  return(list_new)
}