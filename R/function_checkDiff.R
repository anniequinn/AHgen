checkDiff <- 
  function(edgesNew, 
           edgelist) {
  
  step1 <- edgesNew %>% select(layer, from, to)
  
  step2 <- edgelist %>% select(layer, from, to)
  
  diff <- setdiff(step1, step2)
  
  n <- diff %>% nrow %>% as.character # as.character is a workaround fix to "avoid argument is of length 0" error
  
  if(n == "0") {
    
    print("All edges match. Proceed!")
    
  } else {
    
    return(diff)
    
  }
  
  }