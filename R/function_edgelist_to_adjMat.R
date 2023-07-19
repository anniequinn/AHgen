edgelist_to_adjMat <- function(edgelist, vInfo) {
  
  output <-
    edgelist %>% 
    edgelist_to_igraph(vInfo = vInfo) %>% 
    igraph_to_adjMat()
  
  return(output)
  
}