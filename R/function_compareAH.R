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
    rename(baseline_value = value, baseline_rankByLevel = rank_byLevel) %>%
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
           change_rankByLevel = baseline_rankByLevel - rank_byLevel) # Note that a smaller rank number signifies a higher rank; a positive change_rank number signifies an increase in rank

  if("confidence_rankByLevel_minusPlus" %in% colnames(scenarios$results)) {
    
    scenarios_compared$results <-
      scenarios_compared$results %>%
      select(scenarioName, version, location, scenario, date, # scenario identifiers
             level, levelName_full, levelName, Node, # basic identifiers
             metric, baseline_value, baseline_value_amp, baseline_rankByLevel, # baseline results
             value, value_amp, change_value_amp, change_pct, # scenario value change
             rank_byLevel, change_rankByLevel, confidence_rankByLevel_minusPlus, # scenario rank change
             confidence_rankByLevel_minus, rank_byLevel_minus, # sensitivity rank for detail on confidence (minus)
             confidence_rankByLevel_plus, rank_byLevel_plus, # sensitivity rank for detail on confidence (plus) 
             value_minus, value_plus) # sensitivity values for detail on confidence
    
    scenarios_compared$confidence <-
      scenarios_compared$results %>%
      group_by(scenarioName, level) %>%
      count(confidence_rankByLevel_minusPlus) %>%
      ungroup()
    # probably need to add a another step to add %
    # and do some renaming of columns etc. here 
    # refer to summarise_AH

  } else {
    
    scenarios_compared$results <-
      scenarios_compared$results %>%
      select(scenarioName, version, location, scenario, date, # scenario identifiers
             level, levelName_full, levelName, Node, # basic identifiers
             metric, baseline_value, baseline_value_amp, baseline_rankByLevel, # baseline results
             value, value_amp, change_value_amp, change_pct, # scenario value change
             rank_byLevel, change_rankByLevel) # scenario rank change
  }
    
  return(scenarios_compared)

}