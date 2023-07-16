check_duplicates <- function(edges) {
  
  edges <- edges %>% select(from, to)
  
  ind <- duplicated(edges[,1:2])
  
  dup <- edges[ind,]
  
  if(nrow(dup) == 0) {
    
    print("There are no duplicate edges. Proceed!")
    
  } else if(nrow(dup) > 0) {
    
    return(dup)
    
  }
  
}