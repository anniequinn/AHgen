table_rankDegree <- function(results, 
                             levels = "all", 
                             singleScenario = TRUE, 
                             compareLocations = FALSE,
                             compareScenarios = FALSE) {
  
  require(stringr)
  
  if(singleScenario == TRUE) {
    results <- results %>% mutate(prefix = NA)
  } else if(singleScenario == FALSE &
            compareLocations == TRUE &
            compareScenarios == FALSE) {
    results <- results %>% mutate(prefix = stringr::str_c(location, " - "))
  } else if(singleScenario == FALSE &
            compareLocations == FALSE &
            compareScenarios == TRUE) {
    results <- results %>% mutate(prefix = stringr::str_c(scenario, " - "))
  } else if(singleScenario == FALSE &
            compareLocations == TRUE &
            compareScenarios == TRUE) {
    results <- results %>% mutate(prefix = stringr::str_c(location, " ", scenario, " - "))
  }
  
  if(levels == "all") {
    
    output <-
      results %>%
      filter(metric %in% 
               c("nodeDegree_up1", "nodeDegree_down1", "nodeDegree_total1")) %>%
      mutate(Level = stringr::str_c(level, " - ", levelName),
             metric = case_when(metric == "nodeDegree_up1" ~ "Up Degree",
                                metric == "nodeDegree_down1" ~ "Down Degree",
                                metric == "nodeDegree_total1" ~ "Total Degree"),
             metric = factor(metric, levels = c("Up Degree", "Down Degree", "Total Degree")),
             metric_viz = stringr::str_c(prefix, metric)) %>%
      arrange(location, scenario, metric) %>%
      select(Level, Node, metric_viz, value) %>%
      pivot_wider(names_from = metric_viz, values_from = value)
    
  } else if(levels == "Purposes") {
    
    output <-
      results %>%
      filter(metric %in% 
               c("nodeDegree_down1", "nodeDegree_down2", 
                 "nodeDegree_down3", "nodeDegree_down4")) %>%
      filter(level == 1) %>%
      mutate(Level = stringr::str_c(level, " - ", levelName),
             metric = case_when(metric == "nodeDegree_down1" ~ "Outcomes",
                                metric == "nodeDegree_down2" ~ "Tasks",
                                metric == "nodeDegree_down3" ~ "Processes",
                                metric == "nodeDegree_down4" ~ "Resources"),
             metric = factor(metric, levels = 
                               c("Outcomes", "Tasks", "Processes", "Resources")),
             metric_viz = stringr::str_c(prefix, metric)) %>%
      arrange(location, scenario, metric) %>%
      select(Level, Node, metric_viz, value) %>%
      pivot_wider(names_from = metric_viz, values_from = value)
    
  } else if(levels == "Outcomes") {
    
    output <-
      results %>%
      filter(metric %in% 
               c("nodeDegree_up1", "nodeDegree_down1", 
                 "nodeDegree_down2", "nodeDegree_down3")) %>%
      filter(level == 2) %>%
      mutate(Level = stringr::str_c(level, " - ", levelName),
             metric = case_when(metric == "nodeDegree_up1" ~ "Purposes",
                                metric == "nodeDegree_down1" ~ "Tasks",
                                metric == "nodeDegree_down2" ~ "Processes",
                                metric == "nodeDegree_down3" ~ "Resources"),
             metric = factor(metric, levels = 
                               c("Purposes", "Tasks", "Processes", "Resources")),
             metric_viz = stringr::str_c(prefix, metric)) %>%
      arrange(location, scenario, metric) %>%
      select(Level, Node, metric_viz, value) %>%
      pivot_wider(names_from = metric_viz, values_from = value)
    
  } else if(levels == "Tasks") {
    
    output <-
      results %>%
      filter(metric %in% 
               c("nodeDegree_up2", "nodeDegree_up1", 
                 "nodeDegree_down1", "nodeDegree_down2")) %>%
      filter(level == 3) %>%
      mutate(Level = stringr::str_c(level, " - ", levelName),
             metric = case_when(metric == "nodeDegree_up2" ~ "Purposes",
                                metric == "nodeDegree_up1" ~ "Outcomes",
                                metric == "nodeDegree_down1" ~ "Processes",
                                metric == "nodeDegree_down2" ~ "Resources"),
             metric = factor(metric, levels = 
                               c("Purposes", "Outcomes", "Processes", "Resources")),
             metric_viz = stringr::str_c(prefix, metric)) %>%
      arrange(location, scenario, metric) %>%
      select(Level, Node, metric_viz, value) %>%
      pivot_wider(names_from = metric_viz, values_from = value)
    
  } else if(level == "Processes") {
    
    output <-
      results %>%
      filter(metric %in% 
               c("nodeDegree_up3", "nodeDegree_up2", 
                 "nodeDegree_up1", "nodeDegree_down1")) %>%
      filter(level == 4) %>%
      mutate(Level = stringr::str_c(level, " - ", levelName),
             metric = case_when(metric == "nodeDegree_up3" ~ "Purposes",
                                metric == "nodeDegree_up2" ~ "Outcomes",
                                metric == "nodeDegree_up1" ~ "Tasks",
                                metric == "nodeDegree_down1" ~ "Resources"),
             metric = factor(metric, levels = 
                               c("Purposes", "Outcomes", "Tasks", "Resources")),
             metric_viz = stringr::str_c(prefix, metric)) %>%
      arrange(location, scenario, metric) %>%
      select(Level, Node, metric_viz, value) %>%
      pivot_wider(names_from = metric_viz, values_from = value)
    
  } else if(levels == "Resources") {
    
    output <-
      results %>%
      filter(metric %in% 
               c("nodeDegree_up4", "nodeDegree_up3", 
                 "nodeDegree_up2", "nodeDegree_up1")) %>%
      filter(level == 5) %>%
      mutate(Level = stringr::str_c(level, " - ", levelName),
             metric = case_when(metric == "nodeDegree_up4" ~ "Purposes",
                                metric == "nodeDegree_up3" ~ "Outcomes",
                                metric == "nodeDegree_up2" ~ "Tasks",
                                metric == "nodeDegree_up1" ~ "Processes"),
             metric = factor(metric, levels = 
                               c("Purposes", "Outcomes", "Tasks", "Processes")),
             metric_viz = stringr::str_c(prefix, metric)) %>%
      arrange(location, scenario, metric) %>%
      select(Level, Node, metric_viz, value) %>%
      pivot_wider(names_from = metric_viz, values_from = value)
    
  }
  
  return(output)
  
}