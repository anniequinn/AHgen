calcMetrics <- function(igraph, vInfo, metrics) {
  
  source("functions/functions_internal_calc.R", local = TRUE)
  
  output <- 
    
    lapply(metrics, function(x) {
    
    calc <- get(paste0("calc", x))
    
    calc(igraph = igraph, vInfo = vInfo)
    
  })
  
  output <- 
    output %>% 
    reduce(full_join, by = c("level", "levelName_full", "levelName", "Node")) %>%
    arrange(level, levelName_full, levelName, Node)
  
  return(output)
  
}


calcChange <- 
  function(before, after, metric) { 
  
  source("functions/functions_internal_calc.R", local = TRUE)
  
  before <- 
    before %>% 
    select(level, levelName_full, levelName, Node, matches(metric)) %>% 
    stats::setNames(c("level", "levelName_full", "levelName", "Node", "before"))
  
  after <- 
    after %>% 
    select(level, levelName_full, levelName, Node, matches(metric)) %>% 
    stats::setNames(c("level", "levelName_full", "levelName", "Node", "after"))
  
  output <- 
    list(before, after) %>% 
    reduce(full_join, by = c("level", "levelName_full", "levelName", "Node")) %>%
    mutate(absChange_afterMinusBefore = after-before,
           pctChange = absChange_afterMinusBefore/before * 100)
  
  output <- 
    output %>% 
    mutate(pctChange = ifelse(before == 0 & after == 0, 0, pctChange))
  
  return(output)
  
}


calcChangeNewEC <- function(before, after, metric) { 
  
  source("functions/functions_internal_calc.R", local = TRUE)
  
  before <- 
    before %>% 
    select(level, levelName_full, levelName, Node, matches(metric)) %>% 
    stats::setNames(c("level", "levelName_full", "levelName", "Node", "before")) %>%
    mutate(sumEC_before = sum(before),
           nodePropBefore = (before / sumEC_before) * 100)
  
  after <- 
    after %>% 
    select(level, levelName_full, levelName, Node, matches(metric)) %>% 
    stats::setNames(c("level", "levelName_full", "levelName", "Node", "after")) %>%
    mutate(sumEC_after = sum(after),
           nodePropAfter = (after / sumEC_after) * 100)
  
  output <- 
    list(before, after) %>% 
    reduce(full_join, by = c("level", "levelName_full", "levelName", "Node")) %>%
    mutate(absChange_afterMinusBefore = nodePropAfter - nodePropBefore,
           pctChange = (absChange_afterMinusBefore/nodePropBefore) * 100) %>%
    select(level, levelName_full, levelName, Node, 
           before, after, absChange_afterMinusBefore, pctChange)
  
  output <- 
    output %>% 
    mutate(pctChange = ifelse(before == 0 & after == 0, 0, pctChange))
  
  return(output)
  
}