table_vExcluded <- function(vExcluded_benchmark, vExcluded_input,
                            singleScenario = TRUE,
                            compareLocations = FALSE,
                            compareScenarios = FALSE) {
    
    if(singleScenario == TRUE &
       compareLocations == FALSE &
       compareScenarios == FALSE) {
      
      if((class(vExcluded_benchmark) == "list") == TRUE) {
        
        vExcluded_input_vExcluded <- NULL
        
      } else {
        
        vExcluded_input_vExcluded <- vExcluded_benchmark %>% pull(Node)
        
      }
      
      step1 <- 
        vExcluded_input %>%
        filter(!Node %in% benchmark_vExcluded) %>%
        mutate(Level = str_c(level, " - ", levelName)) %>%
        select(-level, -levelName_full, -levelName) %>%
        group_by(Level) %>%
        count() %>%
        adorn_totals(where = "row") %>%
        rename(Node = n)
      
      output <-
        vExcluded_input %>%
        filter(!Node %in% benchmark_vExcluded) %>%
        arrange(level) %>%
        mutate(Level = str_c(level, " - ", levelName)) %>%
        select(Level, Node) %>%
        rbind(step1) %>%
        arrange(Level, Node)
      
    }
    
    if(singleScenario == FALSE & 
       compareLocations == TRUE & 
       compareScenarios == FALSE) {
      
      if((class(vExcluded_benchmark) == "list") == TRUE) {
        
        benchmark_vExcluded <- NULL
        
      } else {
        
        benchmark_vExcluded <- vExcluded_benchmark %>% pull(Node)
        
      }
      
      step1 <- 
        vExcluded_input %>%
        mutate(Level = str_c(level, " - ", levelName)) %>%
        select(location, Level, Node) %>% 
        group_by(location, Level) %>% 
        count() %>%
        ungroup() %>%
        pivot_wider(names_from = location, values_from = n) %>% 
        mutate(Node = NA, n_scenarios = NA)
      
      step2 <- 
        vExcluded_input %>%
        mutate(Level = str_c(level, " - ", levelName)) %>%
        select(location, Level, Node) %>% 
        group_by(location) %>% 
        count() %>%
        ungroup() %>%
        mutate(Level = "Total") %>%
        pivot_wider(names_from = location, values_from = n) %>% 
        mutate(Node = NA, n_scenarios = NA)
      
      step3 <- 
        vExcluded_input %>%
        select(location) %>%
        unique() %>%
        pull(location) 
      
      output <- 
        vExcluded_input %>%
        mutate(Level = str_c(level, " - ", levelName)) %>%
        select(location, Level, Node) %>% 
        mutate(tick = "X") %>%
        pivot_wider(names_from = location, values_from = tick) %>%
        rowwise() %>%
        mutate(n_scenarios = sum(!is.na(c_across(all_of(step3))))) %>%
        arrange(Level, desc(n_scenarios), Node) %>%
        rbind(step1, step2) %>%
        rename(`Number of Scenarios where Excluded` = n_scenarios) %>%
        arrange(Level, Node)
      
    }
    
    if(singleScenario == FALSE & 
       compareLocations == FALSE & 
       compareScenarios == TRUE) {
      
      if((class(vExcluded_benchmark) == "list") == TRUE) {
        
        benchmark_vExcluded <- NULL
        
      } else {
        
        benchmark_vExcluded <- vExcluded_benchmark %>% pull(Node)
        
      }
      
      step1 <-
        vExcluded_input %>%
        mutate(Level = str_c(level, " - ", levelName)) %>%
        select(scenario, Level, Node) %>% 
        filter(!Node %in% benchmark_vExcluded) %>%
        group_by(scenario, Level) %>% 
        count() %>%
        ungroup() %>%
        pivot_wider(names_from = scenario, values_from = n) %>% 
        mutate(Node = NA, n_scenarios = NA)
      
      if((nrow(step1) > 0) == FALSE) {
        
        stop("No vExcluded found.")
        
      } else {
        
        step2 <-
          vExcluded_input %>%
          mutate(Level = str_c(level, " - ", levelName)) %>%
          select(scenario, Level, Node) %>% 
          filter(!Node %in% benchmark_vExcluded) %>%
          group_by(scenario) %>% 
          count() %>%
          ungroup() %>%
          mutate(Level = "Total") %>%
          pivot_wider(names_from = scenario, values_from = n) %>% 
          mutate(Node = NA, n_scenarios = NA)
        
        step3 <-
          vExcluded_input %>%
          filter(scenario != "baseline") %>%
          select(scenario) %>% 
          unique() %>%
          pull(scenario) 
        
        step4 <- 
          vExcluded_input %>%
          mutate(Level = str_c(level, " - ", levelName)) %>%
          select(scenario, Level, Node) %>% 
          filter(!Node %in% vExcluded_input_vExcluded) %>%
          filter(scenario != "baseline") %>%
          mutate(tick = "X") %>%
          pivot_wider(names_from = scenario, values_from = tick) %>% 
          
          output <-
          step4 %>%
          rowwise() %>%
          mutate(n_scenarios = sum(!is.na(c_across(all_of(step3))))) %>%
          arrange(Level, desc(n_scenarios), Node) %>%
          rbind(step1, step2) %>%
          rename(`Number of Scenarios where Excluded` = n_scenarios) %>%
          arrange(Level, Node)
        
      }
      
    }
    
    if(vExcluded_benchmark == "NA" &
       singleScenario == FALSE & 
       compareLocations == TRUE & 
       compareScenarios == TRUE) {
      
      step1 <-
        vExcluded_input %>%
        mutate(Level = str_c(level, " - ", levelName)) %>%
        select(location, scenario, Level, Node) %>%
        group_by(location, scenario, Level) %>%
        count() %>%
        ungroup() %>%
        pivot_wider(names_from = scenario, values_from = n) %>% 
        mutate(Node = NA, n_scenarios = NA)
      
      if((nrow(step1) > 0) == FALSE) {
        
        stop("No vExcluded found.")
        
      } else {
        
        step2 <-
          vExcluded_input %>%
          mutate(Level = str_c(level, " - ", levelName)) %>%
          select(location, scenario, Level, Node) %>% 
          group_by(location, scenario) %>% 
          count() %>%
          ungroup() %>%
          mutate(Level = "Total") %>%
          pivot_wider(names_from = scenario, values_from = n) %>% 
          mutate(Node = NA, n_scenarios = NA)
        
        step3 <-
          vExcluded_input %>%
          select(scenario) %>% 
          unique() %>%
          pull(scenario) 
        
        step4 <- 
          vExcluded_input %>%
          mutate(Level = str_c(level, " - ", levelName)) %>%
          select(location, scenario, Level, Node) %>% 
          mutate(tick = "X") %>%
          pivot_wider(names_from = scenario, values_from = tick)
          
        output <-
          step4 %>%
          rowwise() %>%
          mutate(n_scenarios = sum(!is.na(c_across(all_of(step3))))) %>%
          arrange(Level, desc(n_scenarios), Node) %>%
          rbind(step1, step2) %>%
          rename(`Number of Scenarios where Excluded` = n_scenarios) %>%
          arrange(location, Level, Node)
        
      }
      
    }
    
    return(output)
    
}