checkDiff <- 
  function(edgesNew, 
           edgelist) {
  
  step1 <- edgesNew %>% select(layer, from, to)
  
  step2 <- edgelist %>% select(layer, from, to)
  
  diff <- setdiff(step1, step2)
  
  if(nrow(diff) == 0) {
    
    print("All edges match. Proceed!")
    
  } else {
    
    return(diff)
    
  }
  
}