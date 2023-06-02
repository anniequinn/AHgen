# Function to check for and return any nodes which have redundant sets of edge
# connections so that they can be combined into a single vertex in the adjMat
checkRedundancy <- 
  function(adjMat) {
    
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
      
      neighbors[[x]] <- igraph %>% neighbors(x) %>% as.vector %>% as.matrix
      
    }
    
    # Return neighbors list
    neighbors
    
    # Find vertices with duplicate neighbors
    indDuplicatedVec <- 
      duplicated(neighbors) | duplicated(neighbors, fromLast = TRUE)
    
    # Return nodeNames of these vector numbers
    redundant <- nodeNames[indDuplicatedVec]
    
    if (length(redundant) > 0) {
      
      print(paste0("The following redundant edge sets have been found: ", redundant))
      print("Make corrections in adjMat input file. Either combine vertices with redundant edge sets, or adjust linking to reflect differences between vertices.")
      
    } else {
      
      print("All edge sets are unique. Proceed!")
      
    }
    
  }