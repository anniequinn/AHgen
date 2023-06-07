compareAH <- function(AH_benchmark, scenarios_toCompare, scenarioNames) {
  
  # Internal function
  mapSubLists <- function(input, scenarioNames) {
    
    Map(cbind, ., scenarioName = scenarioNames) %>%
      lapply(function (x) {
        x %>%
          as.data.frame() %>%
          mutate(scenario_dummy = scenarioName) %>%
          separate(scenario_dummy, 
                   c("remove", "version", "location", "scenario", "date"), 
                   sep = "_") %>%
          select(-remove)
      }) %>%
      discard(function(x) nrow(x) == 1) %>%
      do.call(rbind, .) %>%
      remove_rownames()
  }
  
  # Create list output to attach comparisons to
  scenarios_compared <- list()
  
  # Pull out only the $summary$vertices list elements from  and reformat
  scenarios_compared$vertices <- 
    scenarios_toCompare %>% 
    Biobase::subListExtract("summary") %>%
    Biobase::subListExtract("vertices") %>%
    mapSubLists(scenarioNames = scenarioNames)
  
  # Pull out only the $summary$edges list elements from  and reformat
  scenarios_compared$edges <- 
    scenarios_toCompare %>% 
    Biobase::subListExtract("summary") %>%
    Biobase::subListExtract("edges") %>%
    mapSubLists(scenarioNames = scenarioNames)
  
  # Pull out only the $vExcluded list elements from scenarios_toCompare and reformat
  scenarios_compared$vExcluded <- 
    scenarios_toCompare %>% 
    Biobase::subListExtract("vExcluded") %>% 
    mapSubLists(scenarioNames = scenarioNames)
  
  # Pull out only the $results list elements from scenarios_toCompare and reformat
  results_step1 <- 
    scenarios_toCompare %>% 
    Biobase::subListExtract("results") %>% 
    mapSubLists(scenarioNames = scenarioNames)
  
  # Key points here: 
  
  # baseline_value_amp and value_amp columns amplify the SBC_norm values by * 100,000
  # to make them easier to distinguish quickly by eye
  
  # baseline_value_amp and value_amp columns leave all other metric types out
  # so e.g. baseline_value_amp and baseline_value are the same for metric == EC
  # because EC is already normalized on a 0-1 scale
  
  # change_pct uses baseline_value_amp and value_amp
  # change_pct is itself amplified * 100 to make it easier to distinguish by eye
  # where a value of 7.5 = 7.5% (not 750%)
  
  scenarios_compared$results =
    AH_benchmark$results %>%
    rename(baseline_value = value,
           baseline_rankByLevel = rank_byLevel,
           baseline_rankOverall = rank_overall) %>%
    full_join(results_step1, 
              by = c("level", "levelName_full", "levelName", "Node", "metric")) %>%
    mutate(baseline_value_amp = 
             ifelse(metric == "SBC_norm", baseline_value * 100000, baseline_value), # SBC_norm values are amplified here by 100,000 to make them easier to distinguish by eye
           value_amp = 
             ifelse(metric == "SBC_norm", value * 100000, value), # SBC_norm values are amplified here by 100,000 to make them easier to distinguish by eye
           change_value_amp = value_amp - baseline_value_amp,
           change_pct = 
             ifelse(baseline_value == 0 & value == 0, 0,
                    ((value_amp - baseline_value_amp) / baseline_value_amp) * 100), # Not that changePct is after * 100 for % value
           change_rankByLevel = baseline_rankByLevel - rank_byLevel, # Note that a smaller rank number signifies a higher rank; a positive change_rank number signifies an increase in rank
           change_rankOverall = baseline_rankOverall - rank_overall) %>% # Note that a smaller rank number signifies a higher rank; a positive change_rank number signifies an increase in rank
    select(scenarioName, version, location, scenario, date, # scenario identifiers
           level, levelName_full, levelName, Node, # basic identifiers
           baseline_value, baseline_value_amp, baseline_rankByLevel, baseline_rankOverall, # baseline results
           metric, value, value_amp, rank_byLevel, rank_byLevel_conf, 
           rank_overall, rank_overall_conf, outlier, outlierLabel, # scenario results
           change_value_amp, change_pct, change_rankByLevel, change_rankOverall) # change
  
  return(scenarios_compared)

}