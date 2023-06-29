# Function to get table of vertices
# previously named function_getVertices
table_vertices <- function(USAH_input,
                           singleScenario = TRUE,
                           compareLocations = FALSE,
                           compareScenarios = FALSE) {
    
    if(singleScenario == TRUE) {
      
      output <-
        USAH_input$summary$vertices %>%
        mutate(Level = 
                 case_when(
                   level == 1 ~ str_c(level, " - ", levelName),
                   level == 2 ~ str_c(level, " - ", levelName),
                   level == 3 ~ str_c(level, " - ", levelName),
                   level == 4 ~ str_c(level, " - ", levelName),
                   level == 5 ~ str_c(level, " - ", levelName),
                   level == "Total" ~ "Total"),
               `Number of Nodes` = 
                 paste0(n, 
                        " (", 
                        (((n / max(n)) %>% as.numeric) * 100) %>% round(2),
                        "%)")) %>%
        select(Level, `Number of Nodes`)
      
    } else if (singleScenario == FALSE &
               compareLocations == TRUE &
               compareScenarios == FALSE) {
      
      output <-
        USAH_input$vertices %>%
        select(levelName_viz, location, n_vertices) %>% 
        group_by(location) %>% 
        mutate(group_max = max(n_vertices)) %>% 
        ungroup() %>%
        mutate(group_percent = 
                 (((n_vertices / group_max) %>% as.numeric) * 100) %>% round(2),
               value_table = paste0(n_vertices, " (", group_percent, "%)")) %>%
        select(-n_vertices, -group_max, -group_percent) %>%
        pivot_wider(names_from = location, values_from = value_table) %>% ## 
        rename(Level = levelName_viz,
               `Baseline: Generic UK City Template` = template)
      
    } else if (singleScenario == FALSE &
               compareLocations == FALSE &
               compareScenarios == TRUE) {
      
      output <-
        USAH_input$vertices %>%
        select(levelName_viz, scenario, n_vertices) %>% 
        group_by(scenario) %>% 
        mutate(group_max = max(n_vertices)) %>% 
        ungroup() %>%
        mutate(group_percent = 
                 (((n_vertices / group_max) %>% as.numeric) * 100) %>% round(2),
               value_table = paste0(n_vertices, " (", group_percent, "%)")) %>%
        select(-n_vertices, -group_max, -group_percent) %>%
        pivot_wider(names_from = scenario, values_from = value_table) %>% ## 
        rename(Level = levelName_viz) ## Specify your baseline column name?
      
    } else if (singleScenario == FALSE &
               compareLocations == TRUE &
               compareScenarios == TRUE) {
      
      output <-
        USAH_input$vertices %>%
        select(levelName_viz, location, scenario, n_vertices) %>% 
        group_by(location, scenario) %>% 
        mutate(group_max = max(n_vertices)) %>% 
        ungroup() %>%
        mutate(group_percent = 
                 (((n_vertices / group_max) %>% as.numeric) * 100) %>% round(2),
               value_table = paste0(n_vertices, " (", group_percent, "%)")) %>%
        select(-n_vertices, -group_max, -group_percent) %>%
        pivot_wider(names_from = scenario, values_from = value_table) %>% ## 
        rename(Level = levelName_viz) ## Specify your baseline column name?
      
    }
    
    return(output)
    
  }