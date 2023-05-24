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
    mutate(vName = rownames(tnet)) %>%
    select(vName, WVBC = betweenness)
  
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


calcEC <- 
  function(igraph, 
           vInfo) {
  
  require(igraph)
  
  output <- (igraph %>% eigen_centrality(directed = TRUE))$vector
  
  output <- 
    output %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>% 
    stats::setNames(c("vName", "centrality")) %>%
    inner_join(vInfo) 
  
  return(output)
  
}


calcSBC <- 
  function(igraph, 
           vInfo) {
  
  require(igraph)
  
  options(digits = 15) # Ensure R global options can account ~15 decimal points for inverted proxyWeight 1.9999999999
  
  igraph_invertedWeight <- 
    igraph %>% 
    igraph_to_edgelist() %>%
    dplyr::mutate(weight = 2 - weight) %>%
    edgelist_to_igraph(vInfo)
  
  output <- 
    sbc_norm(graph = igraph_invertedWeight, 
             undirected = TRUE, 
             normalize = TRUE)
  
  output <- 
    output %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>% 
    stats::setNames(c("vName", "SBC", "SBC_norm")) %>%
    inner_join(vInfo)
  
  options(digits = 7) # Return R global options for 7 decimal points
  
  return(output)
    
}


calcMetrics <- 
  function(igraph, 
           vInfo, 
           metrics) {
  
  source("functions/functions_internal_calc.R", local = TRUE)
  
  output <- 
    
    lapply(metrics, function(x) {
    
    calc <- get(paste0("calc", x))
    
    calc(igraph = igraph, vInfo = vInfo)
    
  })
  
  output <- 
    output %>% 
    reduce(full_join, by = c("level", "levelName", "vName")) %>%
    arrange(level, levelName, vName)
  
  return(output)
  
}


calcChange <- 
  function(before, 
           after, 
           metric) { 
  
  source("functions/functions_internal_calc.R", local = TRUE)
  
  before <- 
    before %>% 
    select(level, levelName, vName, matches(metric)) %>% 
    stats::setNames(c("level", "levelName", "vName", "before"))
  
  after <- 
    after %>% 
    select(level, levelName, vName, matches(metric)) %>% 
    stats::setNames(c("level", "levelName", "vName", "after"))
  
  output <- 
    list(before, after) %>% 
    reduce(full_join, by = c("level", "levelName", "vName")) %>%
    mutate(absChange_afterMinusBefore = after-before,
           pctChange = absChange_afterMinusBefore/before * 100)
  
  output <- 
    output %>% 
    mutate(pctChange = ifelse(before == 0 & after == 0, 0, pctChange))
  
  return(output)
  
}


calcChangeNewEC <- 
  function(before, 
           after, 
           metric) { 
  
  source("functions/functions_internal_calc.R", local = TRUE)
  
  before <- 
    before %>% 
    select(level, levelName, vName, matches(metric)) %>% 
    stats::setNames(c("level", "levelName", "vName", "before")) %>%
    mutate(sumEC_before = sum(before),
           nodePropBefore = (before / sumEC_before) * 100)
  
  after <- 
    after %>% 
    select(level, levelName, vName, matches(metric)) %>% 
    stats::setNames(c("level", "levelName", "vName", "after")) %>%
    mutate(sumEC_after = sum(after),
           nodePropAfter = (after / sumEC_after) * 100)
  
  output <- 
    list(before, after) %>% 
    reduce(full_join, by = c("level", "levelName", "vName")) %>%
    mutate(absChange_afterMinusBefore = nodePropAfter - nodePropBefore,
           pctChange = (absChange_afterMinusBefore/nodePropBefore) * 100) %>%
    select(level, levelName, vName, before, after, absChange_afterMinusBefore, pctChange)
  
  output <- 
    output %>% 
    mutate(pctChange = ifelse(before == 0 & after == 0, 0, pctChange))
  
  return(output)
  
}
