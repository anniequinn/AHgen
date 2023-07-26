igraph_to_adjMat <- function(igraph) {
  
  require(igraph)
  require(tibble)

  output <- 
    igraph %>%
    igraph::get.adjacency() %>% 
    as.matrix() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Node") %>% 
    as_tibble()
  
  output <- 
    output %>% 
    tibble::add_column(level = igraph::V(igraph)$level, 
                       levelName_full = igraph::V(igraph)$levelName_full,
                       levelName = igraph::V(igraph)$levelName, 
                       .before = 1)
  
  return(output)
  
}