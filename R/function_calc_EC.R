calc_EC <- function(igraph, vInfo) {
  
  require(igraph)
  require(tibble)
  require(stats)
  
  output <- (igraph %>% igraph::eigen_centrality(directed = TRUE))$vector
  
  output <- 
    output %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>% 
    stats::setNames(c("Node", "EC")) %>%
    inner_join(vInfo) %>%
    relocate(level, levelName_full, levelName, Node)
  
  return(output)
  
}