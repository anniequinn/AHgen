apply_sensitivity <- 
  function(USAH_input, version, location, baseline, scenario, pct) {
    
    output <- list()
    
    output[[paste0("USAH_", version, "_", location, "_", scenario)]] <- 
      USAH_input
    
    # Decrease any affected edge weights by x% (e.g. 10%)
    edgelist_minus = 
      edgeSensitivity(USAH_input = USAH_input, sign = "minus", pct = pct)
    
    # Generate minus scenario output
    output[[paste0(
      "USAH_", version, "_", location, "_", scenario, "_minus", pct*100)]] <-   
      apply_scenario(USAH_input = USAH_input, edgelist_scenario = edgelist_minus,
                     version = version, location = location,
                     scenario = "{scenario}-minus{pct*100}pct")
    
    # Increase any affected edge weights by x% (e.g. 10%)
    edgelist_plus = 
      edgeSensitivity(USAH_input = USAH_input, sign = "plus", pct = pct)
    
    # Generate plus scenario output
    output[[paste0(
      "USAH_", version, "_", location, "_", scenario, "_plus", pct*100)]] <-   
      apply_scenario(USAH_input = USAH_scenario, edgelist_scenario = edgelist_minus,
                     version = version, location = location, 
                     scenario = "{scenario}-plus{pct*100}pct")
    
    output$results <-
      output[[1]]$results %>% 
      rbind(output[[2]]$results) %>%
      rbind(output[[3]]$results)
    
    return(output)
    
  }