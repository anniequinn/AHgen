read_vInfo <- function(filename, sheet = 1) {
  
  require(readxl)
  
  if(!str_detect(filename, ".xlsx")) stop(".xlsx missing in filename")
  
  output <- readxl::read_xlsx(filename, col_types = NULL)
  
  colNames <- output %>% names
  index <- which(colNames == "vName")
  
  if(sum(index) != 1) stop("Column vName not found")
  
  return(output)
  
}