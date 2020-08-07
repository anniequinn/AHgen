read_indicatorsDatabase <- function(filename, 
                                    location, 
                                    preferenceLevels = c("Preferred", "Alternate")) {
  
  db <-  filename %>% readxl::read_xlsx()
  
  colNames <- c("indicator", "indicatorPreference", "from", "to")
  colNames <- c(colNames, location)
  
  output <- 
    db %>% 
    select(all_of(colNames)) %>%
    setNames(c(colNames[-5], "location")) %>%
    mutate(indicatorPreference = factor(indicatorPreference, preferenceLevels)) %>%
    filter(!is.na(location)) %>%
    group_by(indicatorPreference, from, to) %>%
    summarise(value = mean(location), .groups = "drop_last") %>%
    ungroup %>%
    spread(indicatorPreference, value) %>%
    mutate(value = coalesce(!!! select(., matches(preferenceLevels)))) %>%
    select(-all_of(preferenceLevels)) %>%
    rename(weightNew = value) %>%
    mutate(layers = "l2VPM_l3GF") %>% #### Confirm????
    select(layers, everything())
  
  return(output)
  
}