igraph_to_edgelist <- function(igraph) { 
  
  igraph %>% 
    get.data.frame %>% 
    as_tibble() %>% 
    select(layer, from, to, everything())
  
}