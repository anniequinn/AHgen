adjMat_to_igraph <- function(adjMat, vInfo) { 
  
  layer <- (adjMat %>% adjMat_to_edgelist(vInfo))$layer
  
  output <- 
    adjMat %>% 
    select(-level, -levelName_full, -levelName, -Node) %>% 
    as.matrix() %>% 
    graph.adjacency(mode = "undirected", weighted = TRUE) 
  
  E(output)$layer <- layer
  V(output)$level <- adjMat$level
  V(output)$levelName_full <- adjMat$levelName_full
  V(output)$levelName <- adjMat$levelName
  
  return(output)
  
}