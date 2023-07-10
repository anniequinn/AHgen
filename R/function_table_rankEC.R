table_rankEC <- function(results, 
                         singleScenario = TRUE, 
                         compareLocations = FALSE, 
                         compareScenarios = FALSE) {
  
  require(stringr)
  
  if(singleScenario == TRUE &
     compareLocations == FALSE &
     compareScenarios == FALSE) {
    
    step1 <-
      results %>%
      filter(metric %in% 
               c("nodeDegree_down2", "nodeDegree_down3", "nodeDegree_down4")) %>%
      filter(level <= 3) %>%
      select(level, Node, metric, value) %>%
      pivot_wider(names_from = metric, values_from = value) %>%
      mutate(nodeDegree_PO = 
               case_when(
                 level == 1 ~ nodeDegree_down4,
                 level == 2 ~ nodeDegree_down3,
                 level == 3 ~ nodeDegree_down2)) %>%
      select(Node, nodeDegree_PO)
    
    output <-
      results %>%
      filter(metric == "EC") %>%
      filter(level <= 3) %>%
      full_join(step1, by = "Node") %>%
      mutate(Level = stringr::str_c(level, " - ", levelName), 
             Node = paste0(Node, " (", value %>% round(5), ", ", nodeDegree_PO, ")")) %>% # bear in mind if from USAH_x$results, not allScenarios_compared, this is value before being amplified
      select(Level, rank_byLevel, Node) %>%
      arrange(Level, rank_byLevel) %>%
      rename(`Ranked Within Level by Eigenvector Centrality` = rank_byLevel)
    
  } 
  
  if(singleScenario == FALSE &
     compareLocations == TRUE &
     compareScenarios == FALSE) {
    
    step1 <-
      results %>%
      filter(metric %in% 
               c("nodeDegree_down2", "nodeDegree_down3", "nodeDegree_down4")) %>%
      filter(level <= 3) %>%
      select(location, level, Node, metric, value) %>% 
      group_by(location) %>%
      pivot_wider(names_from = metric, values_from = value) %>%
      mutate(nodeDegree_PO = 
               case_when(
                 level == 1 ~ nodeDegree_down4,
                 level == 2 ~ nodeDegree_down3,
                 level == 3 ~ nodeDegree_down2)) %>%
      select(location, Node, nodeDegree_PO) 
    
    output <-
      results %>%
      filter(metric == "EC") %>%
      filter(level <= 3) %>%
      full_join(step1, by = c("location", "Node")) %>% 
      mutate(value_table = paste0(Node, " (", value_amp %>% round(5), ", ", nodeDegree_PO, ")")) %>%
      select(location, level, levelName_full, levelName, value_table, rank_byLevel) %>% 
      pivot_wider(names_from = location, values_from = value_table) %>% 
      arrange(level, rank_byLevel) %>%
      rename(Level = level,
             `Level Name` = levelName,
             `Ranked Within Level by Eigenvector Centrality` = rank_byLevel,
             `Baseline: Generic UK City Template` = template)
    
  }
  
  if(singleScenario == FALSE &
     compareLocations == FALSE &
     compareScenarios == TRUE) {
    
    step1 <-
      results %>%
      filter(metric %in% 
               c("nodeDegree_down2", "nodeDegree_down3", "nodeDegree_down4")) %>%
      filter(level <= 3) %>%
      select(scenario, level, Node, metric, value) %>% 
      group_by(scenario) %>%
      pivot_wider(names_from = metric, values_from = value) %>%
      mutate(nodeDegree_PO = 
               case_when(
                 level == 1 ~ nodeDegree_down4,
                 level == 2 ~ nodeDegree_down3,
                 level == 3 ~ nodeDegree_down2)) %>%
      select(scenario, Node, nodeDegree_PO) 
    
    output <-
      results %>%
      filter(metric == "EC") %>%
      filter(level <= 3) %>%
      full_join(step1, by = c("scenario", "Node")) %>% 
      mutate(value_table = paste0(Node, " (", value_amp %>% round(5), ", ", nodeDegree_PO, ")")) %>%
      select(scenario, level, levelName_full, levelName, value_table, rank_byLevel) %>% 
      pivot_wider(names_from = scenario, values_from = value_table) %>% 
      arrange(level, rank_byLevel) %>%
      rename(Level = level,
             `Level Name` = levelName,
             `Ranked Within Level by Eigenvector Centrality` = rank_byLevel)
    
  } 
  
  if(singleScenario == FALSE &
     compareLocations == TRUE &
     compareScenarios == TRUE) {
    
    step1 <-
      results %>%
      filter(metric %in% 
               c("nodeDegree_down2", "nodeDegree_down3", "nodeDegree_down4")) %>%
      filter(level <= 3) %>%
      select(location, scenario, level, Node, metric, value) %>% 
      group_by(location, scenario) %>%
      pivot_wider(names_from = metric, values_from = value) %>%
      mutate(nodeDegree_PO = 
               case_when(
                 level == 1 ~ nodeDegree_down4,
                 level == 2 ~ nodeDegree_down3,
                 level == 3 ~ nodeDegree_down2)) %>%
      select(location, scenario, Node, nodeDegree_PO) 
    
    output <-
      results %>%
      filter(metric == "EC") %>%
      filter(level <= 3) %>%
      full_join(step1, by = c("location", "scenario", "Node")) %>% 
      mutate(value_table = paste0(Node, " (", value_amp %>% round(5), ", ", nodeDegree_PO, ")")) %>%
      select(location, scenario, level, levelName_full, levelName, value_table, rank_byLevel) %>% 
      pivot_wider(names_from = scenario, values_from = value_table) %>% 
      arrange(location, level, rank_byLevel) %>%
      rename(Level = level,
             `Level Name` = levelName,
             `Ranked Within Level by Eigenvector Centrality` = rank_byLevel)
    
  }
  
  return(output)
  
}