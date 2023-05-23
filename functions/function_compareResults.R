# Function to compare baseline and each scenario, then reorganise into one
# dataframe from which plots can be made
compareResults <- function(baselineScenario, 
                           scenarioNames) {
  
  # Create empty list to add outputs to
  outputList <- list()
  
  # Function to label outliers
  is_outlier <- function(x) {
    return(
      x < quantile(as.numeric(x), 0.25, na.rm = TRUE) - 1.5 * IQR(as.numeric(x), na.rm = TRUE) | 
        x > quantile(as.numeric(x), 0.75, na.rm = TRUE) + 1.5 * IQR(as.numeric(x), na.rm = TRUE))
  }
  
  for (x in seq_along(scenarioNames)) { # for loop to iterate {lines} for each specific scenario
    
    step1a <-
      calcChangeNewEC( # calcChange will create a new dataframe that compares baseline (before) and scenario (after)
        before = 
          baselineScenario$results %>% 
          filter(type == "EC") %>%
          mutate(centrality = centrality * 1000),
        after = 
          allScenarios_results[[x]] %>% 
          filter(type == "EC") %>% 
          mutate(centrality = centrality * 1000),
        metric = "centrality") %>%
      mutate(type = "EC", .before = 1) 
    
    step1b <-
      calcChange( # calcChange will create a new dataframe that compares baseline (before) and scenario (after)
        before = 
          baselineScenario$results %>% 
          filter(type == "WVBC"), 
        after = 
          allScenarios_results[[x]] %>% 
          filter(type == "WVBC"),
        metric = "centrality") %>%
      mutate(type = "WVBC", .before = 1)
    
    step1 <- 
      rbind(step1a, step1b) %>% 
      rename(baseline_result_centrality = before,  # rename columns for clarity when dealing with multiple scenarios
             scenario_result_centrality = after, 
             scenario_result_centralityChange = absChange_afterMinusBefore,
             scenario_result_pctChange = pctChange)
    
    step2 <-
      allScenarios_results[[x]] %>%
      select(type, vName, rank_overall, rank_byLevel) %>%
      rename(scenario_result_rankOverall = rank_overall, 
             scenario_result_rankByLevel = rank_byLevel)
    
    output <-
      step1 %>%
      full_join(step2, by = c("type", "vName")) %>%
      gather(resultType, 
             result, 
             scenario_result_centrality:scenario_result_rankByLevel) %>% # put columns related to scenario results into "long format"
      mutate_(scenario = x) %>% # add column to specify which scenario each result corresponds to ## need to find way to directly use character string not number
      group_by(resultType, level) %>% # group by resultType and level so that the following outlier function calculates this for each group
      mutate(outlierLabel = ifelse(is_outlier(result), vName, NA)) %>% # calculate outliers and where these exist assign an outlierLabel to be used in plots
      ungroup
    
    outputList[[x]] <- output # add output for specific scenario to the overall list
    
  }
  
  # Change names of outputList list elements to reflect correct scenario
  names(outputList) <- scenarioNames
  
  # Return outputList
  outputList
  
  # Bind outputList list elements into one dataframe
  outputDataframe <- outputList %>% do.call("rbind", .)
  
  # Workaround to replace scenario numbers with scenario names
  scenarioKey <- 
    scenarioNames %>% 
    as.data.frame() %>% 
    data.table::setDT(keep.rownames = TRUE) %>% 
    rename(number = rn, name = ".")
  
  outputDataframe$scenario <- 
    scenarioKey$name[match(outputDataframe$scenario, scenarioKey$number)]
  
  ## Add locationLabel and scenarioLabel column for cleaner plots etc. ? May require location+baseline/hazard+week[COVID] if we are going to layer flood hazard + week x of COVID
  
  # Return outputDataframe
  outputDataframe
  
}
