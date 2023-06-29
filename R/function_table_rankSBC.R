# Function to get table of nodes ranked by SBC
# previously named function_getRankSBC
table_rankSBC <- 
  function(USAH_input,
           singleScenario = TRUE,
           compareLocations = FALSE,
           compareScenarios = FALSE) {
    
    if (singleScenario == TRUE &
        compareLocations == FALSE &
        compareScenarios == FALSE) {
      
      output <-
        USAH_input$results %>%
        filter(metric == "SBC_norm") %>% # note: values in final table relate to SBC_norm
        filter(level >= 3) %>%
        filter(rank_byLevel <= 37) %>%
        mutate(value_amp = (value * 100000) %>% round(0), # * 100000 is to amplify values for easier distinction by eye
               Node = paste0(Node, " (", value_amp, ")"),
               levelName_viz = str_c(level, " - ", levelName)) %>%
        group_by(levelName_viz, rank_byLevel) %>% 
        mutate(Node = paste0(Node, collapse = ", ")) %>%
        ungroup() %>%
        select(levelName_viz, rank_byLevel, Node) %>%
        distinct() %>%
        arrange(levelName_viz, rank_byLevel) %>%
        rename(Level = levelName_viz,
               `Ranked Within Level by Stable Betweenness Centrality` = rank_byLevel)
      
      
    } else if (singleScenario == FALSE &
               compareLocations == TRUE &
               compareScenarios == FALSE) {
      
      output <-
        USAH_input$results %>%
        filter(metric == "SBC_norm") %>% ## note: values included in table are for SBC_norm
        filter(level >= 3) %>%
        filter(rank_byLevel <= 37) %>%
        mutate(Node = paste0(Node, " (", value_amp %>% round(), ")")) %>%
        select(location, levelName_viz, Node, rank_byLevel) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
        pivot_wider(names_from = location, values_from = Node) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
        mutate_all(~str_remove_all(as.character(.x), 'c\\(')) %>%
        mutate_all(~str_remove_all(as.character(.x), '\"\\)')) %>%
        mutate_all(~str_remove_all(as.character(.x), '\"')) %>%
        mutate(rank_byLevel = rank_byLevel %>% as.numeric) %>%
        arrange(levelName_viz, rank_byLevel) %>%
        rename(Level = levelName_viz,
               `Ranked Within Level by Stable Betweenness Centrality` = rank_byLevel,
               `Baseline: Generic UK City Template` = template)
      
      
    } else if (singleScenario == FALSE &
               compareLocations == FALSE &
               compareScenarios == TRUE) {
      
      output <-
        USAH_input$results %>%
        filter(metric == "SBC_norm") %>% ## note: values included in table are for SBC_norm
        filter(level >= 3) %>%
        filter(rank_byLevel <= 37) %>%
        mutate(Node = paste0(Node, " (", value_amp %>% round(), ")")) %>%
        select(scenario, levelName_viz, Node, rank_byLevel) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
        pivot_wider(names_from = scenario, values_from = Node) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
        mutate_all(~str_remove_all(as.character(.x), 'c\\(')) %>%
        mutate_all(~str_remove_all(as.character(.x), '\"\\)')) %>%
        mutate_all(~str_remove_all(as.character(.x), '\"')) %>%
        mutate(rank_byLevel = rank_byLevel %>% as.numeric) %>%
        arrange(levelName_viz, rank_byLevel) %>%
        rename(Level = levelName_viz,
               `Ranked Within Level by Stable Betweenness Centrality` = rank_byLevel)
      
    } else if (singleScenario == FALSE &
               compareLocations == TRUE &
               compareScenarios == TRUE) {
      
      output <-
        USAH_input$results %>%
        filter(metric == "SBC_norm") %>%
        filter(level >= 3) %>%
        mutate(value_table = paste0(Node, " (", value_amp %>% round(), ")")) %>%
        select(location, scenario, level, levelName_full, levelName, value_table, rank_byLevel) %>%
        pivot_wider(names_from = scenario, values_from = value_table) %>% 
        mutate_all(~str_remove_all(as.character(.x), 'c\\(')) %>%
        mutate_all(~str_remove_all(as.character(.x), '\"\\)')) %>%
        mutate_all(~str_remove_all(as.character(.x), '\"')) %>%
        mutate(rank_byLevel = rank_byLevel %>% as.numeric) %>%
        filter(rank_byLevel <= 37) %>%
        arrange(level, rank_byLevel) %>%
        rename(Level = level,
               `Level Name` = levelName,
               `Ranked Within Level by Stable Betweenness Centrality` = rank_byLevel)
      
    }
    
    return(output)
    
  }