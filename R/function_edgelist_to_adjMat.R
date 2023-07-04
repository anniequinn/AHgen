edgelist_to_adjMat <- function(edgelist, vInfo) {
  
  edgelist %>% edgelist_to_igraph(vInfo) %>% igraph_to_adjMat
  
}