# Function to get table of edges
function_getEdges <- 
  function(
    USAH_input,
    singleScenario = TRUE,
    compareLocations = FALSE,
    compareScenarios = FALSE) {
    
    if(singleScenario == TRUE) {
      
      output <-
        USAH_input$summary$edges %>%
        mutate(`Number of Links` = 
                 paste0(n, 
                        " (", 
                        (((n / max(n)) %>% as.numeric) * 100) %>% round(2),
                        "%)")) %>%
        select(layer, `Number of Links`) %>%
        rename(`Layer` = layer)
      
    } else if (singleScenario == FALSE &
               compareLocations == TRUE &
               compareScenarios == FALSE) {
      
      output <-
        USAH_input$edges %>%
        select(layerName_viz, location, n_edges) %>% 
        group_by(location) %>% 
        mutate(group_max = max(n_edges)) %>% 
        ungroup() %>%
        mutate(group_percent = 
                 (((n_edges / group_max) %>% as.numeric) * 100) %>% round(2),
               value_table = paste0(n_edges, " (", group_percent, "%)")) %>%
        select(-n_edges, -group_max, -group_percent) %>%
        pivot_wider(names_from = location, values_from = value_table) %>% 
        rename(Layer = layerName_viz,
               `Baseline: Generic UK City Template` = template)
      
    } else if (singleScenario == FALSE &
               compareLocations == FALSE &
               compareScenarios == TRUE) {
      
      output <-
        USAH_input$edges %>%
        select(layerName_viz, scenario, n_edges) %>%
        group_by(scenario) %>%
        mutate(group_max = max(n_edges)) %>% 
        ungroup() %>%
        mutate(group_percent = 
                 (((n_edges / group_max) %>% as.numeric) * 100) %>% round(2),
               value_table = paste0(n_edges, " (", group_percent, "%)")) %>%
        select(-n_edges, -group_max, -group_percent) %>%
        pivot_wider(names_from = scenario, values_from = value_table) %>%
        rename(Layer = layerName_viz) ## Specify your baseline column name?
      
    } else if (singleScenario == FALSE &
               compareLocations == TRUE &
               compareScenarios == TRUE) {
      
      output <-
        USAH_input$edges %>%
        select(layerName_viz, location, scenario, n_edges) %>%
        group_by(location, scenario) %>%
        mutate(group_max = max(n_edges)) %>% 
        ungroup() %>%
        mutate(group_percent = 
                 (((n_edges / group_max) %>% as.numeric) * 100) %>% round(2),
               value_table = paste0(n_edges, " (", group_percent, "%)")) %>%
        select(-n_edges, -group_max, -group_percent) %>%
        pivot_wider(names_from = scenario, values_from = value_table) %>%
        rename(Layer = layerName_viz) ## Specify your baseline column name?
      
      return(output)
      
    }
    
  }