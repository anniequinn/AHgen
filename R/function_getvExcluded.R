# Function to get table of vExcluded
function_getvExcluded <- 
  function(
    USAH_baseline,
    USAH_input, 
    singleScenario = TRUE,
    compareLocations = FALSE,
    compareScenarios = FALSE) {
    
    if(singleScenario == TRUE &
       compareLocations == FALSE &
       compareScenarios == FALSE) {
      
      if((class(USAH_baseline$vExcluded) == "list") == TRUE) {
        
        baseline_vExcluded <- NULL
        
      } else {
        
        baseline_vExcluded <- USAH_baseline$vExcluded %>% pull(Node)
        
      }
      
      step1 <- 
        USAH_input$vExcluded %>%
        filter(!Node %in% baseline_vExcluded) %>%
        mutate(levelName_viz = 
                 case_when(
                   level == 1 ~ str_c(level, " - ", levelName),
                   level == 2 ~ str_c(level, " - ", levelName),
                   level == 3 ~ str_c(level, " - ", levelName),
                   level == 4 ~ str_c(level, " - ", levelName),
                   level == 5 ~ str_c(level, " - ", levelName),
                   level == "Total" ~ "Total")) %>%
        select(-level, -levelName_full, -levelName) %>%
        group_by(levelName_viz) %>%
        count() %>%
        adorn_totals(where = "row") %>%
        rename(Node = n)
      
      output <-
        USAH_input$vExcluded %>%
        filter(!Node %in% baseline_vExcluded) %>%
        arrange(level) %>%
        mutate(levelName_viz = 
                 case_when(
                   level == 1 ~ str_c(level, " - ", levelName),
                   level == 2 ~ str_c(level, " - ", levelName),
                   level == 3 ~ str_c(level, " - ", levelName),
                   level == 4 ~ str_c(level, " - ", levelName),
                   level == 5 ~ str_c(level, " - ", levelName),
                   level == "Total" ~ "Total")) %>%
        select(levelName_viz, Node) %>%
        rbind(step1) %>%
        rename(Level = levelName_viz, Node = Node)
      
    }
    
    if(singleScenario == FALSE & 
       compareLocations == TRUE & 
       compareScenarios == FALSE) {
      
      if((class(USAH_baseline$vExcluded) == "list") == TRUE) {
        
        baseline_vExcluded <- NULL
        
      } else {
        
        baseline_vExcluded <- USAH_baseline$vExcluded %>% pull(Node)
        
      }
      
      step1 <- 
        USAH_input$vExcluded %>%
        select(location, levelName_viz, Node) %>% 
        group_by(location, levelName_viz) %>% 
        count() %>%
        ungroup() %>%
        pivot_wider(names_from = location, values_from = n) %>% 
        mutate(Node = NA, n_scenarios = NA)
      
      step2 <- 
        USAH_input$vExcluded %>%
        select(location, levelName_viz, Node) %>% 
        group_by(location) %>% 
        count() %>%
        ungroup() %>%
        mutate(levelName_viz = "Total") %>%
        pivot_wider(names_from = location, values_from = n) %>% 
        mutate(Node = NA, n_scenarios = NA)
      
      step3 <- 
        USAH_input$vExcluded %>%
        select(location) %>%
        unique() %>%
        pull(location) 
      
      output <- 
        USAH_input$vExcluded %>%
        select(location, levelName_viz, Node) %>% 
        mutate(tick = "X") %>%
        pivot_wider(names_from = location, values_from = tick) %>%
        rowwise() %>%
        mutate(n_scenarios = sum(!is.na(c_across(all_of(step3))))) %>%
        arrange(levelName_viz, desc(n_scenarios), Node) %>%
        rbind(step1, step2) %>%
        rename(Level = levelName_viz, 
               `Number of Scenarios where Excluded` = n_scenarios)
      
    }
    
    if(singleScenario == FALSE & 
       compareLocations == FALSE & 
       compareScenarios == TRUE) {
      
      if((class(USAH_baseline$vExcluded) == "list") == TRUE) {
        
        baseline_vExcluded <- NULL
        
      } else {
        
        baseline_vExcluded <- USAH_baseline$vExcluded %>% pull(Node)
        
      }
      
      step1 <-
        USAH_input$vExcluded %>%
        select(scenario, levelName_viz, Node) %>% 
        filter(!Node %in% baseline_vExcluded) %>%
        group_by(scenario, levelName_viz) %>% 
        count() %>%
        ungroup() %>%
        pivot_wider(names_from = scenario, values_from = n) %>% 
        mutate(Node = NA, n_scenarios = NA)
      
      if((nrow(step1) > 0) == FALSE) {
        
        stop("No vExcluded found.")
        
      } else {
        
        step2 <-
          USAH_input$vExcluded %>%
          select(scenario, levelName_viz, Node) %>% 
          filter(!Node %in% baseline_vExcluded) %>%
          group_by(scenario) %>% 
          count() %>%
          ungroup() %>%
          mutate(levelName_viz = "Total") %>%
          pivot_wider(names_from = scenario, values_from = n) %>% 
          mutate(Node = NA, n_scenarios = NA)
        
        step3 <-
          USAH_input$vExcluded %>%
          filter(scenario != "baseline") %>%
          select(scenario) %>% 
          unique() %>%
          pull(scenario) 
        
        step4 <- 
          USAH_input$vExcluded %>%
          select(scenario, levelName_viz, Node) %>% 
          filter(!Node %in% baseline_vExcluded) %>%
          filter(scenario != "baseline") %>%
          mutate(tick = "X") %>%
          pivot_wider(names_from = scenario, values_from = tick) %>% 
          
          output <-
          step4 %>%
          rowwise() %>%
          mutate(n_scenarios = sum(!is.na(c_across(all_of(step3))))) %>%
          arrange(levelName_viz, desc(n_scenarios), Node) %>%
          rbind(step1, step2) %>%
          rename(Level = levelName_viz, 
                 `Number of Scenarios where Excluded` = n_scenarios)
        
      }
      
    }
    
    if(USAH_baseline == "NA" &
       singleScenario == FALSE & 
       compareLocations == TRUE & 
       compareScenarios == TRUE) {
      
      step1 <-
        USAH_input$vExcluded %>%
        select(location, scenario, levelName_viz, Node) %>%
        group_by(location, scenario, levelName_viz) %>%
        count() %>%
        ungroup() %>%
        pivot_wider(names_from = scenario, values_from = n) %>% 
        mutate(Node = NA, n_scenarios = NA)
      
      if((nrow(step1) > 0) == FALSE) {
        
        stop("No vExcluded found.")
        
      } else {
        
        step2 <-
          USAH_input$vExcluded %>%
          select(location, scenario, levelName_viz, Node) %>% 
          group_by(location, scenario) %>% 
          count() %>%
          ungroup() %>%
          mutate(levelName_viz = "Total") %>%
          pivot_wider(names_from = scenario, values_from = n) %>% 
          mutate(Node = NA, n_scenarios = NA)
        
        step3 <-
          USAH_input$vExcluded %>%
          select(scenario) %>% 
          unique() %>%
          pull(scenario) 
        
        step4 <- 
          USAH_input$vExcluded %>%
          select(location, scenario, levelName_viz, Node) %>% 
          mutate(tick = "X") %>%
          pivot_wider(names_from = scenario, values_from = tick)
          
        output <-
          step4 %>%
          rowwise() %>%
          mutate(n_scenarios = sum(!is.na(c_across(all_of(step3))))) %>%
          arrange(levelName_viz, desc(n_scenarios), Node) %>%
          rbind(step1, step2) %>%
          rename(Level = levelName_viz, 
               `Number of Scenarios where Excluded` = n_scenarios)
        
      }
      
    }
    
    return(output)
    
  }