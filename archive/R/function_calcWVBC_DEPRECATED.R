calcWVBC <- # Unstable version
  function(igraph, 
           vInfo) { 
    
    source("functions/functions_internal_calc.R", local = TRUE)
    
    require(tnet)
    
    tnet <- 
      igraph %>% 
      as_adjacency_matrix(attr = "weight") %>%
      as.matrix
    
    tnet2 <- 
      tnet %>%   
      symmetrise_w() %>%
      betweenness_w(directed = TRUE, alpha = 0.5) %>%
      as_tibble %>%
      mutate(Node = rownames(tnet)) %>%
      select(Node, WVBC = betweenness)
    
    output <- function_merge_vInfo(dt = tnet2, vInfo = vInfo)
    
    return(output)
    
  }


calcUWVBC <- # Unstable version
  function(igraph, 
           vInfo) { 
    
    source("functions/functions_internal_calc.R", local = TRUE)
    
    require(igraph)
    
    igraph %>% 
      remove.edge.attribute("weight") %>%
      igraph::betweenness() %>%
      function_igraphResultFormatting(name = "UWVBC") %>%
      function_merge_vInfo(vInfo)
    
  }