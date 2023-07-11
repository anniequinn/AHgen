check_symmetry <- function(adjMat) {
  
  require(stats)
  require(data.table)
  require(vctrs)
  
  Nodes <- adjMat %>% pull(Node)
  
  unsymmetrical_step1 <- list()
  
  for(x in Nodes) {
    # add new column to identify if from rowwise or columnwise in adjMat - would then need to only select duplicated(step3) without this column
    prep_step1 <- 
      adjMat %>% 
      select(x) %>% 
      t() %>% 
      as.data.frame %>% 
      stats::setNames(Nodes) %>% 
      data.table::setDT(keep.rownames = TRUE) %>% 
      rename(Node = rn)
    
    prep_step2 <- 
      adjMat %>% 
      select(-level, -levelName_full, -levelName) %>% 
      filter(Node == x)
    
    prep_step3 <- prep_step1 %>% rbind(prep_step2)
    
    indDuplicatedVec <- 
      duplicated(prep_step3) | duplicated(prep_step3, fromLast = TRUE)
    
    unsymmetrical_step1[[paste0("unsymmetrical ", x)]] <- 
      prep_step3[!indDuplicatedVec, ]
    
  }
  
  unsymmetrical_step1
  
  unsymmetrical_step2 <- unsymmetrical_step1 %>% vctrs::list_drop_empty
  
  unsymmetrical_step3 <- 
    lapply(unsymmetrical_step2, function(x) 
      x %>% select(where(is.character) | where(~n_distinct(.) > 1)))
  
  unsymmetrical_step4 <- 
    unsymmetrical_step3 %>% data.table::rbindlist(fill = TRUE) 
  
  nodes_unsymmetrical <- 
    adjMat %>% 
    select(level, Node) %>% 
    arrange(level, Node) %>% 
    filter(Node %in% colnames(unsymmetrical_step4)) %>% 
    pull()
  
  new_order <- c("Node", nodes_unsymmetrical)
  
  unsymmetrical <- unsymmetrical_step4 %>% select(all_of(new_order))
  
  if (nrow(unsymmetrical) < 1) {
    
    print("All edges are symmetrical. Proceed!")
    
  } else {
    
    print("Not all edges are symmetrical. Inspect 'check' object and make corrections in adjMat input file.")
    
    return(unsymmetrical)
    
  }
  
}