check_duplicates <- function(edges) {
  
  edges <- edges %>% select(from, to)
  
  ind <- duplicated(edges[,1:2])
  
  output <- edges[ind,]
  
  if(nrow(output) == 0) {
    
    print("There are no duplicate edges. Proceed!")
    
  } else if(nrow(output) > 0) {
    
    return(output)
    
  }
  
}