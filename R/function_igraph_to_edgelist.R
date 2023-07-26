igraph_to_edgelist <- function(igraph) { 
  
  require(igraph)
  require(tibble)
  
  output <-
    igraph %>% 
    igraph::get.data.frame() %>% 
    tibble::as_tibble() %>% 
    select(layer, from, to, everything())
  
  return(output)
  
}