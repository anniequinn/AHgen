edgelist_to_igraph <- function(edgelist, vInfo) { 
  
  internal_edgelist_to_igraph <- 
    function(edgelist) { 
      
      edgelist %>% 
        select(from, to, weight) %>% 
        graph.data.frame(directed = FALSE)
      
    }
  
  internal_add_layerAttribute <- 
    function(igraph, edgelist) { 
      
      igraph %>% set_edge_attr(name = "layer", value = edgelist$layer)
      
    }
  
  internal_add_weightAttribute <- 
    function(igraph, edgelist) {
      
      igraph %>% set_edge_attr(name = "weight", value = edgelist$weight)
      
    }
  
  output <- 
    edgelist %>% 
    internal_edgelist_to_igraph %>% 
    internal_add_layerAttribute(edgelist = edgelist) %>%
    internal_add_weightAttribute(edgelist = edgelist)
  
  V(output)$level <- vInfo$level
  V(output)$levelName_full <- vInfo$levelName_full
  V(output)$levelName <- vInfo$levelName
  
  return(output)
  
}