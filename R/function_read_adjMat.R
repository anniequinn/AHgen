read_adjMat <- function(filename, sheet = 1, rescale = FALSE) {
  
  require(readxl)
  
  if(!str_detect(filename, ".xlsx")) stop(".xlsx missing in filename")
  
  # Prepare column names and classes
  colNames <- readxl::read_xlsx(filename, col_types = NULL) %>% names
  index <- colNames %in% c("level", "levelName", "vName")
  
  nCols <- colNames %>% length
  colClass <- rep("numeric", nCols)
  colClass[index] <- "text"
  
  
  # Read in adjMat
  output <- 
    readxl::read_xlsx(filename, col_types = colClass) %>%
    select(any_of(c("level", "levelName", "vName")), everything()) 
  
  
  # Adjust values
  output[is.na(output)] <- 0 # Replace any NA values with zero
  
  if(rescale == TRUE) { 
    output <- output %>% mutate_if(is.numeric, ~(if_else(. != 0, ./2, .))) 
  }
  
  if(sum(colNames == "level") == 1) { output <- output %>% mutate(level = as.numeric(level)) }
  
  
  # Return output
  return(output)
  
}