function_mergeNodeInfo <- function(dt, nodeInfo) {
  
  nodeInfo %>% 
    select(layer, layerName, nodeName) %>%
    right_join(dt, by = "nodeName")
  
}

function_igraphResultFormatting <- function(igraphresult, name) { 
  
  igraphresult %>% 
    as.data.frame %>% 
    rownames_to_column %>% 
    setNames(c("nodeName", name)) %>%
    as_tibble()
  
}
  
calcWVBC <- function(igraph, nodeInfo) {
  
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
    mutate(nodeName = rownames(tnet)) %>%
    select(nodeName, WVBC = betweenness)
  
  output <- function_mergeNodeInfo(dt = tnet2, nodeInfo = nodeInfo)

  return(output)
  
}

calcUWVBC <- function(igraph, nodeInfo) { 
  
  require(igraph)
  
  igraph::betweenness(igraph) %>%
    function_igraphResultFormatting(name = "UWVBC") %>%
    function_mergeNodeInfo(nodeInfo)
  
}
  
calcMetrics <- function(igraph, nodeInfo, metrics) {
  
  output <- 
    
    lapply(metrics, function(x) {
    
    calc <- get(paste0("calc", x))
    
    calc(igraph = igraph, nodeInfo = nodeInfo)
    
  })
  
  output <- 
    output %>% 
    reduce(full_join, by = c("layer", "layerName", "nodeName")) %>%
    arrange(layer, layerName, nodeName)
  
  return(output)
  
}

calcChange <- function(before, after, metric) { 
  
  before <- 
    before %>% 
    select(layer, layerName, nodeName, matches(metric)) %>% 
    setNames(c("layer", "layerName", "nodeName", "before"))
  after <- 
    after %>% 
    select(layer, layerName, nodeName, matches(metric)) %>% 
    setNames(c("layer", "layerName", "nodeName", "after"))
  
  output <- 
    list(before, after) %>% 
    reduce(full_join, by = c("layer", "layerName", "nodeName")) %>%
    mutate(absChange_afterMinusBefore = after-before,
           pctChange = absChange_afterMinusBefore/before * 100)
  
  output <- 
    output %>% 
    mutate(pctChange = ifelse(before == 0 & after == 0, 0, pctChange))
  
  return(output)
  
}