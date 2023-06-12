compareAH <- function(AH_benchmark, scenarios_toCompare, scenarioNames) {
  
  # Internal function
  mapSubLists <- function(input, scenarioNames) {
    
    input %>%
      Map(cbind, ., scenarioName = scenarioNames) %>%
      lapply(function (x) {
        x %>%
          as.data.frame() %>%
          mutate(scenario_dummy = scenarioName) %>%
          separate(scenario_dummy, 
                   c("name", "version", "location", "scenario", "date"), 
                   sep = "_")
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
  
  # benchmark_value_amp and value_amp columns amplify the SBC_norm values by * 100,000
  # to make them easier to distinguish quickly by eye
  
  # benchmark_value_amp and value_amp columns leave all other metric types out
  # so e.g. benchmark_value_amp and benchmark_value are the same for metric == EC
  # because EC is already normalized on a 0-1 scale
  
  # change_pct uses benchmark_value_amp and value_amp
  # change_pct is itself amplified * 100 to make it easier to distinguish by eye
  # where a value of 7.5 = 7.5% (not 750%)
  
  results_step2 <-
    AH_benchmark$results %>%
    select(-version, -location, -scenario) %>%
    rename(benchmark_value = value, benchmark_rankByLevel = rank_byLevel) %>%
    full_join(results_step1, 
              by = c("level", "levelName_full", "levelName", "Node", "metric")) %>%
    mutate(benchmark_value_amp = 
             ifelse(metric == "SBC_norm", benchmark_value * 100000, benchmark_value), # SBC_norm values are amplified here by 100,000 to make them easier to distinguish by eye
           value_amp = 
             ifelse(metric == "SBC_norm", value * 100000, value), # SBC_norm values are amplified here by 100,000 to make them easier to distinguish by eye
           change_value_amp = value_amp - benchmark_value_amp,
           change_pct = 
             ifelse(benchmark_value == 0 & value == 0, 0,
                    ((value_amp - benchmark_value_amp) / benchmark_value_amp) * 100), # Not that changePct is after * 100 for % value
           change_rankByLevel = benchmark_rankByLevel - rank_byLevel) # Note that a smaller rank number signifies a higher rank; a positive change_rank number signifies an increase in rank

  if(any(sapply(scenarios_toCompare, function(x) any(names(x) == "confidence_rankByLevel_minusPlus")))) {
    
    scenarios_compared$results <-
      results_step2 %>%
      select(
        scenarioName, name, version, location, scenario, date, # scenario identifiers
        level, levelName_full, levelName, Node, # basic identifiers
        metric, benchmark_value, benchmark_value_amp, benchmark_rankByLevel, # baseline / benchmark results
        value, value_amp, change_value_amp, change_pct, # scenario value change
        rank_byLevel, change_rankByLevel, confidence_rankByLevel_minusPlus, # scenario rank change
        confidence_rankByLevel_minus, rank_byLevel_minus, # sensitivity rank for detail on confidence (minus)
        confidence_rankByLevel_plus, rank_byLevel_plus, # sensitivity rank for detail on confidence (plus) 
        value_minus, value_plus) # sensitivity values for detail on confidence
        
    scenarios_compared$confidence <-
      results_step2 %>%
      group_by(scenarioName, level) %>%
      count(confidence_rankByLevel_minusPlus) %>%
      ungroup()
    # probably need to add a another step to add %
    # and do some renaming of columns etc. here 
    # refer to summarise_AH

  } else {
    
    scenarios_compared$results <-
      results_step2 %>%
      select(
        scenarioName, name, version, location, scenario, date, # scenario identifiers
        level, levelName_full, levelName, Node, # basic identifiers
        metric, benchmark_value, benchmark_value_amp, benchmark_rankByLevel, # baseline / benchmark results
        value, value_amp, change_value_amp, change_pct, # scenario value change
        rank_byLevel, change_rankByLevel) # scenario rank change
  }
    
  return(scenarios_compared)

}