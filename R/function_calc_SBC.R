calc_SBC <- function(igraph, vInfo) {
  
  require(igraph)
  require(tibble)
  require(stats)
  
  options(digits = 15) # Ensure R global options can account ~15 decimal points for inverted proxyWeight 1.9999999999
  
  igraph_invertedWeight <- 
    igraph %>% 
    igraph_to_edgelist() %>%
    dplyr::mutate(weight = 2 - weight) %>%
    edgelist_to_igraph(vInfo)
  
  output <- 
    sbc_norm(igraph = igraph_invertedWeight, undirected = TRUE, normalize = TRUE)
  
  output <- 
    output %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>% 
    stats::setNames(c("Node", "SBC", "SBC_norm")) %>%
    inner_join(vInfo)
  
  options(digits = 7) # Return R global options for 7 decimal points
  
  return(output)
  
}