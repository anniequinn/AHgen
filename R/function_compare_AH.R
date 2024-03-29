compare_AH <- function(type = "AH", 
                       AH_benchmark, 
                       scenarios_toCompare, 
                       scenarioNames) {
  
  require(tibble)
  require(Biobase)
  
  if(type == "USAH") {
    
    idCols <- c("name", "version", "location", "scenario", "date")

  } else {
    
    idCols <- c("name", "date")
    
  }
  
  # Internal function to map sublists in list elements
  internal_mapSubLists <- function(input, scenarioNames) {
    
    input %>%
      Map(cbind, ., scenarioName = scenarioNames) %>%
      lapply(function (x) {
        x %>%
          as.data.frame() %>%
          mutate(scenario_dummy = scenarioName) %>%
          separate(scenario_dummy, idCols, sep = "_")
      }) %>%
      do.call(bind_rows, .) %>%
      remove_rownames()
    
  }
  
  # Create list output to attach comparisons to
  output <- list()
  
  # Pull out only the $summary$vertices list elements from  and reformat
  output$vertices <- 
    scenarios_toCompare %>% 
    Biobase::subListExtract("summary") %>%
    Biobase::subListExtract("vertices") %>%
    internal_mapSubLists(scenarioNames = scenarioNames)
  
  # Pull out only the $summary$edges list elements from  and reformat
  output$edges <- 
    scenarios_toCompare %>% 
    Biobase::subListExtract("summary") %>%
    Biobase::subListExtract("edges") %>%
    internal_mapSubLists(scenarioNames = scenarioNames)
  
  # Pull out only the $vExcluded list elements from scenarios_toCompare and reformat
  output$vExcluded <- 
    scenarios_toCompare %>% 
    Biobase::subListExtract("vExcluded") %>% 
    internal_mapSubLists(scenarioNames = scenarioNames)
  
  # Pull out only the $results list elements from scenarios_toCompare and reformat
  results_step1 <- 
    scenarios_toCompare %>% 
    Biobase::subListExtract("results") %>% 
    internal_mapSubLists(scenarioNames = scenarioNames)
  
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
    select(-any_of(idCols)) %>%
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

  if (any(sapply(scenarios_toCompare, function(x) any(names(x$results) == "confidence_rankByLevel_minusPlus"))))
    
  {
    output$results <-
      results_step2 %>%
      select(
        scenarioName, any_of(idCols), # scenario identifiers
        level, levelName_full, levelName, Node, # basic identifiers
        metric, benchmark_value, benchmark_value_amp, benchmark_rankByLevel, # baseline / benchmark results
        value, value_amp, change_value_amp, change_pct, # scenario value change
        rank_byLevel, change_rankByLevel, confidence_rankByLevel_minusPlus, # scenario rank change
        confidence_rankByLevel_minus, rank_byLevel_minus, # sensitivity rank for detail on confidence (minus)
        confidence_rankByLevel_plus, rank_byLevel_plus, # sensitivity rank for detail on confidence (plus) 
        value_minus, value_plus) # sensitivity values for detail on confidence
    
    output$confidence <-
      summarise_confidence(results = output$results)
    
  } else {
    
    output$results <-
      results_step2 %>%
      select(
        scenarioName, any_of(idCols), # scenario identifiers
        level, levelName_full, levelName, Node, # basic identifiers
        metric, benchmark_value, benchmark_value_amp, benchmark_rankByLevel, # baseline / benchmark results
        value, value_amp, change_value_amp, change_pct, # scenario value change
        rank_byLevel, change_rankByLevel) # scenario rank change
    
  }
    
  return(output)

}