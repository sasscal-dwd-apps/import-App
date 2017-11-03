#########################################################################
#' @export access.delimiters

access.delimiters <- function(row_values, datatype){
  if (datatype=="varchar" ||
      datatype=="text" ||
      datatype=="longchar" ||
      datatype=="datetime") {
    if (row_values=="NULL"){
      delimiter <- "";
      expression <- " IS ";}else{
        delimiter <- "'";
        expression <- "=";
        if (datatype=="datetime"){
          delimiter <- "#"
        }
      }
  }else{
    delimiter <-"";
    if (row_values=="NULL"){
      expression <- " IS "
    }else{
      expression <- "=";
    }
  }
  delimiters <- data.frame(delimiter, expression)
  return(delimiters)
}
