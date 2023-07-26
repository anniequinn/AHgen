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