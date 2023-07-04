calc_EC <- function(igraph, vInfo) {
  
  require(igraph)
  
  output <- (igraph %>% eigen_centrality(directed = TRUE))$vector
  
  output <- 
    output %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>% 
    stats::setNames(c("Node", "centrality")) %>%
    inner_join(vInfo) 
  
  return(output)
  
}