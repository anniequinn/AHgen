table_rankSBC <- 
  function(results,
           singleScenario = TRUE,
           compareLocations = FALSE,
           compareScenarios = FALSE) {
    
    # Ignore warning: 
    # "Values from `value_table` are not uniquely identified; 
    # output will contain list-cols."
    # This is due to multiple nodes having the same SBC/ranking 
    # and is reformatted with the mutate_all reformatting steps.
    
    if (singleScenario == TRUE &
        compareLocations == FALSE &
        compareScenarios == FALSE) {
      
      output <-
        results %>%
        filter(metric == "SBC_norm") %>% # note: values in final table relate to SBC_norm
        filter(level >= 3) %>%
        filter(rank_byLevel <= 37) %>%
        mutate(value_amp = (value * 100000) %>% round(0), # * 100000 is to amplify values for easier distinction by eye
               Node = paste0(Node, " (", value_amp, ")"),
               Level = str_c(level, " - ", levelName)) %>%
        group_by(Level, rank_byLevel) %>% 
        mutate(Node = paste0(Node, collapse = ", ")) %>%
        ungroup() %>%
        select(Level, rank_byLevel, Node) %>%
        distinct() %>%
        arrange(Level, rank_byLevel) %>%
        rename(`Ranked Within Level by Stable Betweenness Centrality` = rank_byLevel)
      
      
    } else if (singleScenario == FALSE &
               compareLocations == TRUE &
               compareScenarios == FALSE) {
      
      output <-
        results %>%
        filter(metric == "SBC_norm") %>% ## note: values included in table are for SBC_norm
        filter(level >= 3) %>%
        filter(rank_byLevel <= 37) %>%
        mutate(Node = paste0(Node, " (", value_amp %>% round(), ")"),
               Level = str_c(level, " - ", levelName)) %>%
        select(location, Level, Node, rank_byLevel) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
        pivot_wider(names_from = location, values_from = Node) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
        mutate_all(~str_remove_all(as.character(.x), 'c\\(')) %>%
        mutate_all(~str_remove_all(as.character(.x), '\"\\)')) %>%
        mutate_all(~str_remove_all(as.character(.x), '\"')) %>%
        mutate(rank_byLevel = rank_byLevel %>% as.numeric) %>%
        arrange(Level, rank_byLevel) %>%
        rename(`Ranked Within Level by Stable Betweenness Centrality` = rank_byLevel,
               `Baseline: Generic UK City Template` = template)
      
      
    } else if (singleScenario == FALSE &
               compareLocations == FALSE &
               compareScenarios == TRUE) {
      
      output <-
        results %>%
        filter(metric == "SBC_norm") %>% ## note: values included in table are for SBC_norm
        filter(level >= 3) %>%
        filter(rank_byLevel <= 37) %>%
        mutate(Node = paste0(Node, " (", value_amp %>% round(), ")")) %>%
        select(scenario, Level, Node, rank_byLevel) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
        pivot_wider(names_from = scenario, values_from = Node) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
        mutate_all(~str_remove_all(as.character(.x), 'c\\(')) %>%
        mutate_all(~str_remove_all(as.character(.x), '\"\\)')) %>%
        mutate_all(~str_remove_all(as.character(.x), '\"')) %>%
        mutate(rank_byLevel = rank_byLevel %>% as.numeric) %>%
        arrange(Level, rank_byLevel) %>%
        rename(`Ranked Within Level by Stable Betweenness Centrality` = rank_byLevel)
      
    } else if (singleScenario == FALSE &
               compareLocations == TRUE &
               compareScenarios == TRUE) {
      
      output <-
        results %>%
        filter(metric == "SBC_norm") %>%
        filter(level >= 3) %>%
        mutate(value_table = paste0(Node, " (", value_amp %>% round(), ")"),
               Level = str_c(level, " - ", levelName)) %>%
        select(location, scenario, Level, value_table, rank_byLevel) %>%
        pivot_wider(names_from = scenario, values_from = value_table) %>% 
        mutate_all(~str_remove_all(as.character(.x), 'c\\(')) %>%
        mutate_all(~str_remove_all(as.character(.x), '\"\\)')) %>%
        mutate_all(~str_remove_all(as.character(.x), '\"')) %>%
        mutate(rank_byLevel = rank_byLevel %>% as.numeric) %>%
        filter(rank_byLevel <= 37) %>%
        arrange(location, Level, rank_byLevel) %>%
        rename(`Ranked Within Level by Stable Betweenness Centrality` = rank_byLevel)
      
    }
    
    return(output)
    
  }