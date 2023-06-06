checkDuplicates <- function(edgelist) {
  
  edges <- edgelist %>% select(from, to)
  
  ind <- duplicated(edges[,1:2])
  
  dup <- edges[ind,]
  
  if(isTRUE(nrow(dup)) == 0) {
    
    print("There are no duplicate edges. Proceed!")
    
  } else if(isTRUE(nrow(dup)) != 0) {
    
    return(dup)
    
  }
  
}