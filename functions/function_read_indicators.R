read_indicators <- function(filename, 
                            scenario, 
                            preferenceLevels = c("Preferred", "Alternate"),
                            rescale = FALSE) {
  
  db <-  filename %>% readxl::read_xlsx()
  
  colNames <- c("indicator", "indicatorPreference", "from", "to")
  colNames <- c(colNames, scenario)
  
  output <- 
    db %>% 
    select(all_of(colNames)) %>%
    setNames(c(colNames[-5], "scenario")) %>%
    mutate(indicatorPreference = factor(indicatorPreference, preferenceLevels)) %>%
    filter(!is.na(scenario)) %>%
    group_by(indicatorPreference, from, to) %>%
    summarise(value = mean(scenario), .groups = "drop_last") %>%
    ungroup %>%
    spread(indicatorPreference, value) %>%
    mutate(value = coalesce(!!! select(., matches(preferenceLevels)))) %>%
    select(-all_of(preferenceLevels)) %>%
    rename(weightNew = value) %>%
    select(everything())
  
  if(rescale == TRUE) { output <- output %>% mutate(weightNew = weightNew/2) }
  
  db <-
    db %>% 
    select(layer, from, to) %>%
    distinct(from, to, .keep_all = TRUE)
  
  output <-
    output %>%
    left_join(db, by = c("from", "to")) %>%
    relocate(layer, from, to, weightNew)
  
  return(output)
  
}