igraph_to_adjMat <- function(igraph) {
  
  output <- 
    igraph %>%
    get.adjacency() %>% 
    as.matrix %>% 
    as.data.frame %>% 
    rownames_to_column("Node") %>% 
    as_tibble()
  
  output <- 
    output %>% 
    add_column(level = V(igraph)$level, 
               levelName_full = V(igraph)$levelName_full,
               levelName = V(igraph)$levelName, 
               .before = 1)
  
  return(output)
  
}