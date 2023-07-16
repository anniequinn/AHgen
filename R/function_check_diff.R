check_diff <- function(edgesNew, edgelist) {
  
  step1 <- edgesNew %>% select(from, to)
  
  step2 <- edgelist %>% select(from, to)
  
  diff <- setdiff(step1, step2)
  
  if(nrow(diff) == 0) {
    
    print("All edges match. Proceed!")
    
  } else if(nrow(diff) > 0) {
    
    return(diff)
    
  }
  
}