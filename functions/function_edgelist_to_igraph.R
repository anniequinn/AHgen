edgelist_to_igraph <- function(edgelist, outputList = FALSE) { 
  
  internal_edgelist_to_igraph <- function(edgelist) { 
    edgelist %>% 
      select(from, to, weight) %>% 
      graph.data.frame(directed = FALSE)
  }
  
  internal_add_layerAttribute <- function(igraph, edgelist) { 
    igraph %>% set_edge_attr(name = "layers", value = edgelist$layers)
  }
  
  if(outputList == TRUE) { 
    input <- edgelist %>% split(., .$layers)
    output <- lapply(input, function(x) { x %>% internal_edgelist_to_igraph %>% internal_add_layerAttribute(., x) })
  } 
  
  if(outputList == FALSE) {
    
    output <- edgelist %>% internal_edgelist_to_igraph %>% internal_add_layerAttribute(edgelist = edgelist)
    
  }
  
  return(output)
  
}