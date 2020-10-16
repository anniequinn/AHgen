read_adjMat <- function(filename, sheet = 1) {
  
  require(readxl)
  
  if(!str_detect(filename, ".xlsx")) stop(".xlsx missing in filename")
  
  colNames <- readxl::read_xlsx(filename, col_types = NULL) %>% names
  index <- str_detect(colNames, "level|levelName|vName")
  
  if(sum(index) != 3) stop("One (or more) of columns level, levelName and/or vName not found")
  
  nCols <- colNames %>% length
  colClass <- rep("numeric", nCols)
  colClass[index] <- "text"
  
  output <- 
    readxl::read_xlsx(filename, col_types = colClass) %>%
    select(level, levelName, vName, everything()) %>% 
    mutate(level = as.numeric(level))
  
  output[is.na(output)] <- 0 # Replace any NA values with zero
  
  return(output)
  
}