adjMat_to_igraph <- function(adjMat) { 
  adjMat %>% 
    select(-layer, -layerName, -nodeName) %>% 
    as.matrix() %>% graph.adjacency(mode = "undirected", weighted = TRUE) }