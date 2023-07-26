table_vertices <- function(vSummary,
                           singleScenario = TRUE,
                           compareLocations = FALSE,
                           compareScenarios = FALSE) {
    
    if(singleScenario == TRUE) {
      
      output <-
        vSummary %>%
        mutate(`Number of Nodes` = 
                 paste0(n, 
                        " (", 
                        (((n / max(n)) %>% as.numeric) * 100) %>% round(2),
                        "%)")) %>%
        select(levelName, `Number of Nodes`)
      
    } else if (singleScenario == FALSE &
               compareLocations == TRUE &
               compareScenarios == FALSE) {
      
      output <-
        vSummary %>%
        select(levelName, location, n_vertices) %>% 
        group_by(location) %>% 
        mutate(group_max = max(n_vertices)) %>% 
        ungroup() %>%
        mutate(group_percent = 
                 (((n_vertices / group_max) %>% as.numeric) * 100) %>% round(2),
               value_table = paste0(n_vertices, " (", group_percent, "%)")) %>%
        select(-n_vertices, -group_max, -group_percent) %>%
        pivot_wider(names_from = location, values_from = value_table)
      
    } else if (singleScenario == FALSE &
               compareLocations == FALSE &
               compareScenarios == TRUE) {
      
      output <-
        vSummary %>%
        select(levelName, scenario, n_vertices) %>% 
        group_by(scenario) %>% 
        mutate(group_max = max(n_vertices)) %>% 
        ungroup() %>%
        mutate(group_percent = 
                 (((n_vertices / group_max) %>% as.numeric) * 100) %>% round(2),
               value_table = paste0(n_vertices, " (", group_percent, "%)")) %>%
        select(-n_vertices, -group_max, -group_percent) %>%
        pivot_wider(names_from = scenario, values_from = value_table)
      
    } else if (singleScenario == FALSE &
               compareLocations == TRUE &
               compareScenarios == TRUE) {
      
      output <-
        vSummary %>%
        select(levelName, location, scenario, n_vertices) %>% 
        group_by(location, scenario) %>% 
        mutate(group_max = max(n_vertices)) %>% 
        ungroup() %>%
        mutate(group_percent = 
                 (((n_vertices / group_max) %>% as.numeric) * 100) %>% round(2),
               value_table = paste0(n_vertices, " (", group_percent, "%)")) %>%
        select(-n_vertices, -group_max, -group_percent) %>%
        pivot_wider(names_from = scenario, values_from = value_table)
      
    }
    
    return(output)
    
}