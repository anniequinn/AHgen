apply_sensitivity <- 
  function(AH_input, name, version, location, baseline, scenario, pct) {
    
    output <- list()
    
    output[[paste0(name, "_", version, "_", location, "_", scenario)]] <- 
      AH_input
    
    # Decrease any affected edge weights by x% (e.g. 10%)
    edgelist_minus = 
      edgeSensitivity(AH_input = AH_input, sign = "minus", pct = pct)
    
    # Generate minus scenario output
    output[[paste0(
      name, "_", version, "_", location, "_", scenario, "_minus", pct*100)]] <-   
      apply_scenario(AH_input = AH_input, edgelist_scenario = edgelist_minus,
                     name = name, version = version, location = location,
                     scenario = "{scenario}-minus{pct*100}pct")
    
    # Increase any affected edge weights by x% (e.g. 10%)
    edgelist_plus = 
      edgeSensitivity(AH_input = AH_input, sign = "plus", pct = pct)
    
    # Generate plus scenario output
    output[[paste0(
      name, "_", version, "_", location, "_", scenario, "_plus", pct*100)]] <-   
      apply_scenario(AH_input = USAH_scenario, edgelist_scenario = edgelist_minus,
                     name = name, version = version, location = location, 
                     scenario = "{scenario}-plus{pct*100}pct")
    
    output[[1]]$results <-
      output[[1]]$results # Find non-0 rank changes
    
    output$results <-
      output[[1]]$results %>% 
      rbind(output[[2]]$results) %>%
      rbind(output[[3]]$results)
    
    return(output)
    
  }