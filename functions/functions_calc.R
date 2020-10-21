calcWVBC <- function(igraph, vInfo) {
  
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

calcUWVBC <- function(igraph, vInfo) { 
  
  source("functions/functions_internal_calc.R", local = TRUE)
  
  require(igraph)
  
  igraph %>% 
    remove.edge.attribute("weight") %>%
    igraph::betweenness() %>%
    function_igraphResultFormatting(name = "UWVBC") %>%
    function_merge_vInfo(vInfo)
  
}
  
calcMetrics <- function(igraph, vInfo, metrics) {
  
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

calcChange <- function(before, after, metric) { 
  
  source("functions/functions_internal_calc.R", local = TRUE)
  
  before <- 
    before %>% 
    select(level, levelName, vName, matches(metric)) %>% 
    setNames(c("level", "levelName", "vName", "before"))
  
  after <- 
    after %>% 
    select(level, levelName, vName, matches(metric)) %>% 
    setNames(c("level", "levelName", "vName", "after"))
  
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