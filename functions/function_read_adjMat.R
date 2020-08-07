function_read_adjMat_csv <- function(filename) {
  
  if(!str_detect(filename, ".csv")) stop(".csv missing in filename")
  
  colNames <- read_csv(filename, col_types = cols()) %>% names
  index <- str_detect(colNames, "layer|layerName|nodeName")
  
  if(sum(index) != 3) stop("One (or more) of columns layer, layerName and/or nodeName not found")
  
  nCols <- colNames %>% length
  colClass <- rep("n", nCols)
  colClass[index] <- "c"
  colClass <- paste0(colClass, collapse = "") # Collapse vector to string
  
  output <- 
    read_csv(filename, col_types = colClass) %>% 
    select(layer, layerName, nodeName, everything()) %>% 
    mutate(layer = as.numeric(layer))
  return(output)
  
}

function_read_adjMat_xlsx <- function(filename, sheet = 1) {
  
  require(readxl)
  
  if(!str_detect(filename, ".xlsx")) stop(".xlsx missing in filename")
  
  colNames <- readxl::read_xlsx(filename, col_types = NULL) %>% names
  index <- str_detect(colNames, "layer|layerName|nodeName")
  
  if(sum(index) != 3) stop("One (or more) of columns layer, layerName and/or nodeName not found")
  
  nCols <- colNames %>% length
  colClass <- rep("numeric", nCols)
  colClass[index] <- "text"
  
  output <- 
    readxl::read_xlsx(filename, col_types = colClass) %>%
    select(layer, layerName, nodeName, everything()) %>% 
    mutate(layer = as.numeric(layer))
  return(output)
  
}
