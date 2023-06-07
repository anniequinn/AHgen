compareUSAH <- function(scenarios_toCompare, scenarioNames) {
  
  # Create list output to attach comparisons to
  scenarios_compared = list()
  
  # Pull out only the $summary$vertices list elements from  and reformat
  scenarios_compared$vertices <- 
     %>% 
    Biobase::subListExtract("summary") %>%
    Biobase::subListExtract("vertices") %>%
    
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
    remove_rownames() %>%
    rename(n_vertices = n)
  
  
  # Pull out only the $summary$edges list elements from  and reformat
  scenarios_compared$edges <- 
     %>% 
    Biobase::subListExtract("summary") %>%
    Biobase::subListExtract("edges") %>%
    
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
    remove_rownames() %>%
    rename(n_edges = n)
  
  # Pull out only the $vExcluded list elements from scenarios_toCompare and reformat
  scenarios_compared$vExcluded <- 
    scenarios_toCompare %>% 
    Biobase::subListExtract("vExcluded") %>% 
    
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
  
  # Pull out only the $results list elements from scenarios_toCompare and reformat
  resultsCompared_step1 <- 
    scenarios_toCompare %>% 
    Biobase::subListExtract("results") %>% 
    
    Map(cbind, ., scenarioName = scenarioNames) %>%
    lapply(function (x) {
      x %>% 
        mutate(scenario_dummy = scenarioName) %>%
        separate(scenario_dummy, 
                 c("remove", "version", "location", "scenario", "date"), 
                 sep = "_") %>%
        select(-remove)}) %>%
    do.call(rbind, .) %>%
    remove_rownames()
  
}