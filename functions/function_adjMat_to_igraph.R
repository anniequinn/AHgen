adjMat_to_igraph <- function(adjMat) { 
  
  layers <- (adjMat %>% adjMat_to_edgelist)$layers
  
  output <- 
    adjMat %>% 
    select(-layer, -layerName, -nodeName) %>% 
    as.matrix() %>% graph.adjacency(mode = "undirected", weighted = TRUE) 
  
  E(output)$layers <- layers
  
  return(output)
  
}