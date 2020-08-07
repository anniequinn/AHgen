read_nodeInfo_csv <- function(filename) {
  
  if(!str_detect(filename, ".csv")) stop(".csv missing in filename")
  
  output <- read_csv(filename, col_types = cols())
  
  colNames <- output %>% names
  index <- which(colNames == "nodeName")
  
  if(sum(index) != 1) stop("Column nodeName not found")
  
  return(output)
  
}

read_nodeInfo_xlsx <- function(filename, sheet = 1) {
  
  require(readxl)
  
  if(!str_detect(filename, ".xlsx")) stop(".xlsx missing in filename")
  
  output <- readxl::read_xlsx(filename, col_types = NULL)
  
  colNames <- output %>% names
  index <- which(colNames == "nodeName")
  
  if(sum(index) != 1) stop("Column nodeName not found")
  
  return(output)
  
}