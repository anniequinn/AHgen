# FROM ADJ MAT ------------------------------------------------------------
adjMat_to_edgelist <- 
  function(adjMat, 
           vInfo) {
  
  colNames <- names(adjMat)
  index <- which(colNames %in% c("vName", "level", "levelName"))
  
  mat <- adjMat %>% select(-index)
  mat[upper.tri(mat, diag = TRUE)] <- NA
  
  source("functions/function_melt2.R", local = TRUE)
  
  step1 <- 
    adjMat %>% 
    select(vName) %>% 
    cbind(mat) %>% 
    as_tibble() %>%
    melt2(measure.vars = c(2:ncol(.))) %>% 
    filter(value == 1)
  
  source("functions/object_levelKey.R", local = TRUE)
  
  step2 <- 
    adjMat %>% 
    select(-any_of(c("level", "levelName"))) %>%
    left_join(vInfo, by = "vName") %>%
    select(level, levelName, vName) %>% 
    left_join(levelKey, by = c("level", "levelName")) %>% 
    select(abbr, vName)
  
  output <- 
    step1 %>%
    select(from = variable, to = vName, weight = value) %>%
    left_join(step2 %>% select(levelFrom = abbr, from = vName), by = "from") %>%
    left_join(step2 %>% select(levelTo = abbr, to = vName), by = "to") %>%
    mutate(layer = paste(levelFrom, levelTo, sep = "_")) %>%
    select(layer, from, to, weight) %>%
    arrange(layer, from, to)
  
  return(output)
  
}


adjMat_to_igraph <- 
  function(adjMat, 
           vInfo) { 
  
  layer <- (adjMat %>% adjMat_to_edgelist(vInfo))$layer
  
  output <- 
    adjMat %>% 
    select(-level, -levelName, -vName) %>% 
    as.matrix() %>% graph.adjacency(mode = "undirected", weighted = TRUE) 
  
  E(output)$layer <- layer
  V(output)$level <- adjMat$level
  V(output)$levelName <- adjMat$levelName
  
  return(output)
  
}


# FROM EDGE LIST ----------------------------------------------------------
edgelist_to_igraph <- 
  function(edgelist, 
           vInfo) { 
  
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
    function(igraph, 
             edgelist) {
    
    igraph %>% set_edge_attr(name = "weight", value = edgelist$weight)
      
  }
  
  output <- 
    edgelist %>% 
    internal_edgelist_to_igraph %>% 
    internal_add_layerAttribute(edgelist = edgelist) %>%
    internal_add_weightAttribute(edgelist = edgelist)
  
  V(output)$level <- vInfo$level
  V(output)$levelName <- vInfo$levelName
  
  return(output)
  
}


edgelist_to_adjMat <- 
  function(edgelist, 
           vInfo) {
  
  edgelist %>% edgelist_to_igraph(vInfo) %>% igraph_to_adjMat
  
}


# FROM IGRAPH -------------------------------------------------------------
igraph_to_adjMat <- 
  function(igraph) {
  
  output <- 
    igraph %>%
    get.adjacency() %>% 
    as.matrix %>% 
    as.data.frame %>% 
    rownames_to_column("vName") %>% 
    as_tibble()
  
  output <- 
    output %>% 
    add_column(level = V(igraph)$level, 
               levelName = V(igraph)$levelName, 
               .before = 1)
  
  return(output)
  
}


igraph_to_edgelist <- 
  function(igraph) { 
  
  igraph %>% 
    get.data.frame %>% 
    as_tibble() %>% 
    select(layer, from, to, everything())
  
}