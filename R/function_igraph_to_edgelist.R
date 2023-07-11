igraph_to_edgelist <- function(igraph) { 
  
  require(igraph)
  require(tibble)
  
  igraph %>% 
    igraph::get.data.frame() %>% 
    tibble::as_tibble() %>% 
    select(layer, from, to, everything())
  
}