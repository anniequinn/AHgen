check_diff <- function(edgesNew, edgelist) {
  
  step1 <- edgesNew %>% select(from, to)
  
  step2 <- edgelist %>% select(from, to)
  
  output <- dplyr::setdiff(step1, step2)
  
  if(nrow(output) == 0) {
    
    print("All edges match. Proceed!")
    
  } else if(nrow(output) > 0) {
    
    return(output)
    
  }
  
}