table_edges <- function(eSummary,
                        singleScenario = TRUE,
                        compareLocations = FALSE,
                        compareScenarios = FALSE) {
    
    if(singleScenario == TRUE) {
      
      output <-
        eSummary %>%
        mutate(`Number of Links` = 
                 paste0(n, 
                        " (", 
                        (((n / max(n)) %>% as.numeric) * 100) %>% round(2),
                        "%)")) %>%
        select(layer, `Number of Links`)
      
    } else if (singleScenario == FALSE &
               compareLocations == TRUE &
               compareScenarios == FALSE) {
      
      output <-
        eSummary %>%
        select(layer, location, n_edges) %>% 
        group_by(location) %>% 
        mutate(group_max = max(n_edges)) %>% 
        ungroup() %>%
        mutate(group_percent = 
                 (((n_edges / group_max) %>% as.numeric) * 100) %>% round(2),
               value_table = paste0(n_edges, " (", group_percent, "%)")) %>%
        select(-n_edges, -group_max, -group_percent) %>%
        pivot_wider(names_from = location, values_from = value_table)
      
    } else if (singleScenario == FALSE &
               compareLocations == FALSE &
               compareScenarios == TRUE) {
      
      output <-
        eSummary %>%
        select(layer, scenario, n_edges) %>%
        group_by(scenario) %>%
        mutate(group_max = max(n_edges)) %>% 
        ungroup() %>%
        mutate(group_percent = 
                 (((n_edges / group_max) %>% as.numeric) * 100) %>% round(2),
               value_table = paste0(n_edges, " (", group_percent, "%)")) %>%
        select(-n_edges, -group_max, -group_percent) %>%
        pivot_wider(names_from = scenario, values_from = value_table)
      
    } else if (singleScenario == FALSE &
               compareLocations == TRUE &
               compareScenarios == TRUE) {
      
      output <-
        eSummary %>%
        select(layer, location, scenario, n_edges) %>%
        group_by(location, scenario) %>%
        mutate(group_max = max(n_edges)) %>% 
        ungroup() %>%
        mutate(group_percent = 
                 (((n_edges / group_max) %>% as.numeric) * 100) %>% round(2),
               value_table = paste0(n_edges, " (", group_percent, "%)")) %>%
        select(-n_edges, -group_max, -group_percent) %>%
        pivot_wider(names_from = scenario, values_from = value_table)
      
      return(output)
      
    }
    
}