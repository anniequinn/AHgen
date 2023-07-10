check_symmetry <- function(adjMat) {
  
  require(stats)
  require(data.table)
    
    Nodes <- adjMat %>% pull(Node)
    
    unsymmetrical <- list()
    
    for(x in Nodes) {
      # add new column to identify if from rowwise or columnwise in adjMat - would then need to only select duplicated(step3) without this column
      step1 <- 
        adjMat %>% 
        select(x) %>% 
        t() %>% 
        as.data.frame %>% 
        stats::setNames(Nodes) %>% 
        data.table::setDT(keep.rownames = TRUE) %>% 
        rename(Node = rn)
      
      step2 <- 
        adjMat %>% 
        select(-level, -levelName_full, -levelName) %>% 
        filter(Node == x)
      
      step3 <- step1 %>% rbind(step2)
      
      indDuplicatedVec <- 
        duplicated(step3) | duplicated(step3, fromLast = TRUE)
      
      unsymmetrical[[paste0("unsymmetrical ", x)]] <- step3[!indDuplicatedVec, ]
      
    }
    
    unsymmetrical
    
    unsymmetrical <- unsymmetrical %>% do.call("rbind", .)
    
    if (nrow(unsymmetrical) < 1) {
      
      print("All edges are symmetrical. Proceed!")
      
    } else {
      
      print("Not all edges are symmetrical. Inspect 'check' object and make corrections in adjMat input file.")
      
      return(unsymmetrical)
      
    }
    
  }