summarise_confidence <- function(results) {
  
  output <- list()
  
  # Calculate the percentage of EC rankings with high, medium, and low confidence
  # per scenario per level
  # only including scenarios where a sensitivity analysis was performed
  # for the top 3 levels (Purposes, Outcomes, & Tasks) only
  output$confidence_scenarioLevel_EC = 
    results %>%
    filter(!is.na(confidence_rankByLevel_minusPlus)) %>%
    group_by(scenarioName, location, scenario, levelName, metric, confidence_rankByLevel_minusPlus) %>%
    count(confidence_rankByLevel_minusPlus, .drop = FALSE) %>%
    ungroup() %>%
    filter(levelName == "Purposes" | 
             levelName == "Outcomes" | 
             levelName == "Tasks") %>%
    filter(metric == "EC") %>%
    group_by(scenarioName, location, scenario, levelName, metric) %>%
    mutate(n_scenarioLevel = sum(n)) %>%
    ungroup() %>%
    mutate(ratio_scenarioLevel = paste0(n, " / ", n_scenarioLevel),
           pct_scenarioLevel = round(((n / n_scenarioLevel) * 100), 2))
  
  # Calculate the percentage of SBC_norm rankings with high, medium, and low confidence
  # per scenario per level
  # only including scenarios where a sensitivity analysis was performed
  # for the bottom 3 levels (Tasks, Processes, & Resources) only
  output$confidence_scenarioLevel_SBCnorm = 
    results %>%
    filter(!is.na(confidence_rankByLevel_minusPlus)) %>%
    group_by(scenarioName, location, scenario, levelName, metric, confidence_rankByLevel_minusPlus) %>%
    count(confidence_rankByLevel_minusPlus, .drop = FALSE) %>%
    ungroup() %>%
    filter(levelName == "Tasks" | 
             levelName == "Processes" | 
             levelName == "Resources") %>%
    filter(metric == "SBC_norm") %>%
    group_by(scenarioName, location, scenario, levelName, metric) %>%
    mutate(n_scenarioLevel = sum(n)) %>%
    ungroup() %>%
    mutate(ratio_scenarioLevel = paste0(n, " / ", n_scenarioLevel),
           pct_scenarioLevel = round(((n / n_scenarioLevel) * 100), 2))
  
  # Calculate the percentage of EC rankings with high, medium, and low confidence
  # per level 
  # only including scenarios where a sensitivity analysis was performed
  # for the top 3 levels (Purposes, Outcomes, & Tasks) only
  output$confidence_level_EC = 
    results %>%
    filter(!is.na(confidence_rankByLevel_minusPlus)) %>%
    group_by(levelName, metric, confidence_rankByLevel_minusPlus) %>%
    count(confidence_rankByLevel_minusPlus, .drop = FALSE) %>%
    ungroup() %>%
    filter(levelName == "Purposes" | 
             levelName == "Outcomes" | 
             levelName == "Tasks") %>%
    filter(metric == "EC") %>%
    group_by(levelName, metric) %>%
    mutate(n_level = sum(n)) %>%
    ungroup() %>%
    mutate(ratio_level = paste0(n, " / ", n_level),
           pct_level = round(((n / n_level) * 100), 2))
  
  # Calculate the percentage of SBC_norm rankings with high, medium, and low confidence
  # per level 
  # only including scenarios where a sensitivity analysis was performed
  # for the top 3 levels (Tasks, Processes, & Resources) only
  output$confidence_level_SBCnorm = 
    results %>%
    filter(!is.na(confidence_rankByLevel_minusPlus)) %>%
    group_by(levelName, metric, confidence_rankByLevel_minusPlus) %>%
    count(confidence_rankByLevel_minusPlus, .drop = FALSE) %>%
    ungroup() %>%
    filter(levelName == "Tasks" | 
             levelName == "Processes" | 
             levelName == "Resources") %>%
    filter(metric == "SBC_norm") %>%
    group_by(levelName, metric) %>%
    mutate(n_level = sum(n)) %>%
    ungroup() %>%
    mutate(ratio_level = paste0(n, " / ", n_level),
           pct_level = round(((n / n_level) * 100), 2))
  
  # Calculate the percentage of EC rankings with high, medium, and low confidence
  # per scenario 
  # only including scenarios where a sensitivity analysis was performed
  # for the top 3 levels (Purposes, Outcomes, & Tasks) only
  output$confidence_scenario_EC = 
    results %>%
    filter(level <= 3) %>%
    filter(!is.na(confidence_rankByLevel_minusPlus)) %>%
    group_by(scenarioName, location, scenario, metric, confidence_rankByLevel_minusPlus) %>%
    count(confidence_rankByLevel_minusPlus, .drop = FALSE) %>%
    ungroup() %>%
    filter(metric == "EC") %>%
    group_by(scenarioName, location, scenario, metric) %>%
    mutate(n_scenario = sum(n)) %>%
    ungroup() %>%
    mutate(ratio_scenario = paste0(n, " / ", n_scenario),
           pct_scenario = round(((n / n_scenario) * 100), 2))
  
  # Calculate the percentage of SBC_norm rankings with high, medium, and low confidence
  # per scenario 
  # only including scenarios where a sensitivity analysis was performed
  # for the top 3 levels (Tasks, Processes, & Resources) only
  output$confidence_scenario_SBCnorm = 
    results %>%
    filter(level >= 3) %>%
    filter(!is.na(confidence_rankByLevel_minusPlus)) %>%
    group_by(scenarioName, location, scenario, metric, confidence_rankByLevel_minusPlus) %>%
    count(confidence_rankByLevel_minusPlus, .drop = FALSE) %>%
    ungroup() %>%
    filter(metric == "SBC_norm") %>%
    group_by(scenarioName, location, scenario, metric) %>%
    mutate(n_scenario = sum(n)) %>%
    ungroup() %>%
    mutate(ratio_scenario = paste0(n, " / ", n_scenario),
           pct_scenario = round(((n / n_scenario) * 100), 2))
  
  # Calculate the percentage of EC rankings with high, medium, and low confidence
  # overall
  # only including scenarios where a sensitivity analysis was performed
  # for the top 3 levels (Purposes, Outcomes, & Tasks) only
  output$confidence_overall_EC = 
    results %>%
    filter(level <= 3) %>%
    filter(!is.na(confidence_rankByLevel_minusPlus)) %>%
    group_by(metric, confidence_rankByLevel_minusPlus) %>%
    count(confidence_rankByLevel_minusPlus, .drop = FALSE) %>%
    ungroup() %>%
    filter(metric == "EC") %>%
    group_by(metric) %>%
    mutate(n_overall = sum(n)) %>%
    ungroup() %>%
    mutate(ratio_overall = paste0(n, " / ", n_overall),
           pct_overall = round(((n / n_overall) * 100), 2))
  
  # Calculate the percentage of SBC_norm rankings with high, medium, and low confidence
  # overall
  # only including scenarios where a sensitivity analysis was performed
  # for the top 3 levels (Tasks, Processes, & Resources) only
  output$confidence_overall_SBCnorm = 
    results %>%
    filter(level >= 3) %>%
    filter(!is.na(confidence_rankByLevel_minusPlus)) %>%
    group_by(metric, confidence_rankByLevel_minusPlus) %>%
    count(confidence_rankByLevel_minusPlus, .drop = FALSE) %>%
    ungroup() %>%
    filter(metric == "EC") %>%
    group_by(metric) %>%
    mutate(n_overall = sum(n)) %>%
    ungroup() %>%
    mutate(ratio_overall = paste0(n, " / ", n_overall),
           pct_overall = round(((n / n_overall) * 100), 2))
  
  return(output)
  
}