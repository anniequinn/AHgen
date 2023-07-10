read_vInfo <- function(filename, sheet = 1) {
  
  require(stringr)
  require(readxl)
  
  if(!stringr::str_detect(filename, ".xlsx")) stop(".xlsx missing in filename")
  
  output <- readxl::read_xlsx(filename, col_types = NULL, sheet = sheet)
  
  colNames <- output %>% names
  
  index <- colNames %in% c("level", "levelName_full", "levelName", "Node")
  
  output <- output[,index]
  
  if(sum(colNames == "level") == 1) { 
    
    output <- output %>% mutate(level = as.numeric(level)) 
    
    }
  
  return(output)
  
}