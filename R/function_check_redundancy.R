check_redundancy <- function(adjMat) {
  
  require(igraph)
  
  # Create vector of nodeNames
  nodeNames <- adjMat %>% pull(Node)
  
  # Create basic vInfo
  vInfo_template <- adjMat %>% select(level, levelName_full, levelName, Node)
  
  # Create igraph object
  igraph <- adjMat %>% adjMat_to_igraph(vInfo = vInfo_template)
  
  # Create list object to populate with each vertex's neighbors
  neighbors <- list()
  
  # Populate list with each vertex's neighbors
  for(x in nodeNames) {
    
    neighbors[[x]] <- igraph %>% igraph::neighbors(x) %>% as.vector %>% as.matrix
    
  }
  
  # Return neighbors list
  neighbors
  
  # Find vertices with duplicate neighbors
  indDuplicatedVec <- 
    duplicated(neighbors) | duplicated(neighbors, fromLast = TRUE)
  
  # Return nodeNames of these vector numbers; these vertices have redundant edge sets
  output <- nodeNames[indDuplicatedVec]
  
  if (length(output) > 0) {
    
    print(paste0("The following redundant edge sets have been found: ", output))
    print("Make corrections in adjMat input file. Either combine vertices with redundant edge sets, or adjust linking to reflect differences between vertices.")
    
  } else {
    
    print("All edge sets are unique. Proceed!")
    
  }
  
}