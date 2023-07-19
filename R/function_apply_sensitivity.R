apply_sensitivity <- function(AH_input, 
                              AH_name, 
                              AH_version = NULL, 
                              AH_location = NULL, 
                              AH_scenario = NULL, 
                              pct = 0.1, 
                              high = 0, 
                              medium = 1) {
    
    output <- list()
    
    if(AH_name == "USAH") {
      
      nameOriginal <- 
        paste0(AH_name, "_", AH_version, "_", AH_location, "_", AH_scenario)
      
      nameMinus <- 
        paste0(AH_name, "_", AH_version, "_", AH_location, "_", AH_scenario, 
               "_minus", pct*100, "pct")
      
      namePlus <-
        paste0(AH_name, "_", AH_version, "_", AH_location, "_", AH_scenario, 
               "_plus", pct*100, "pct")
        
    } else {
      
      nameOriginal <- AH_name
      
      nameMinus <- paste0(AH_name, "_minus", pct*100, "pct")
      
      namePlus <- paste0(AH_name, "_plus", pct*100, "pct")
      
    }
    
    output[[nameOriginal]] <- AH_input
    
    # Decrease any affected edge weights by x% (e.g. 10%)
    edgelist_minus <- 
      calc_sensitivity(edges = AH_input$edgelist, sign = "minus", pct = pct)
    
    # Generate minus scenario output

    if(AH_name == "USAH") {
      
      output[[nameMinus]] <-   
        apply_scenario(AH_input = AH_input, 
                       edgelist_scenario = edgelist_minus,
                       AH_name = AH_name, 
                       AH_version = AH_version, 
                       AH_location = AH_location, 
                       AH_scenario = AH_scenario)
      
    } else {
      
      output[[nameMinus]] <-   
        apply_scenario(AH_input = AH_input, 
                       edgelist_scenario = edgelist_minus,
                       AH_name = nameMinus)
      
    }
    
    # Increase any affected edge weights by x% (e.g. 10%)
    edgelist_plus <- 
      calc_sensitivity(edges = AH_input$edgelist, sign = "plus", pct = pct)
    
    # Generate plus scenario output
    
    if(AH_name == "USAH") {
      
      output[[namePlus]] <-   
        apply_scenario(AH_input = AH_input, 
                       edgelist_scenario = edgelist_plus,
                       AH_name = AH_name, 
                       AH_version = AH_version, 
                       AH_location = AH_location, 
                       AH_scenario = AH_scenario)
      
    } else {
      
      output[[namePlus]] <-   
        apply_scenario(AH_input = AH_input, 
                       edgelist_scenario = edgelist_plus,
                       AH_name = namePlus)
      
    }
    
    # Pull out key parts of AH_input for easy feeding into compareAH
    
    output$summary$vertices <- AH_input$summary$vertices
    
    output$summary$edges <- AH_input$summary$edges
    
    output$vExcluded <- AH_input$vExcluded
    
    # Then process the $results list element

    # Understand confidence in rank_byLevel 
    # according to minus & plus sensitivity runs, with default set to 
    
    # high = 0 for high confidence where sensitivity analysis
    # found no rank change; 
    # medium = 1 for medium confidence where sensitivity analysis 
    # found 1 rank change, 
    # low confidence where sensitivity analysis found > 1 rank change
    
    # However these can be changed e.g. 
    # high = 1 (rank changes of 0 or 1 position), 
    # medium = 5 (rank changes of 2-5 positions), 
    # therefore low > 5 (rank changes of > 5 positions)
    
    if(AH_name == "USAH") {
      
      group <- "scenario"
      
    } else {
      
      output[[1]]$results <- output[[1]]$results %>% mutate(group = "original")
      output[[2]]$results <- output[[2]]$results %>% mutate(group = "minus")
      output[[3]]$results <- output[[3]]$results %>% mutate(group = "plus")
      
    }
    
    confidence_step1 <-
      output[[1]]$results %>%
      select(Node, metric, rank_byLevel)
    
    confidence_step2 <-
      output[[2]]$results %>%
      select(group, Node, metric, value, rank_byLevel) %>%
      rename(value_minus = value, rank_byLevel_minus = rank_byLevel)
    
    confidence_step3 <-
      output[[3]]$results %>%
      select(group, Node, metric, value, rank_byLevel) %>%
      rename(value_plus = value, rank_byLevel_plus = rank_byLevel)
    
    confidence_minus <-
      confidence_step1 %>%
      full_join(confidence_step2) %>%
      mutate(change_rankByLevel_minus = 
               rank_byLevel - rank_byLevel_minus) %>%
      mutate(confidence_rankByLevel_minus = 
               case_when(
                 abs(change_rankByLevel_minus) <= high ~ "High",
                 abs(change_rankByLevel_minus) > high &
                   abs(change_rankByLevel_minus) <= medium ~ "Medium",
                 abs(change_rankByLevel_minus) > medium ~ "Low")) %>%
      select(-rank_byLevel, -group)
    
    confidence_plus <-
      confidence_step1 %>%
      full_join(confidence_step3) %>%
      mutate(change_rankByLevel_plus = 
               rank_byLevel - rank_byLevel_plus) %>%
      mutate(confidence_rankByLevel_plus = 
               case_when(
                 abs(change_rankByLevel_plus) <= high ~ "High",
                 abs(change_rankByLevel_plus) > high &
                   abs(change_rankByLevel_plus) <= medium ~ "Medium",
                 abs(change_rankByLevel_plus) > medium ~ "Low")) %>%
      select(-rank_byLevel, -group)
    
    output$results <-
      output[[1]]$results %>% 
      full_join(confidence_minus) %>%
      full_join(confidence_plus) %>%
      mutate(confidence_rankByLevel_minusPlus = 
               case_when(
                 confidence_rankByLevel_minus == "High" & 
                   confidence_rankByLevel_plus == "High" ~ "High",
                 confidence_rankByLevel_minus == "High" & 
                   confidence_rankByLevel_plus == "Medium" ~ "Medium",
                 confidence_rankByLevel_minus == "Medium" & 
                   confidence_rankByLevel_plus == "High" ~ "Medium",
                 confidence_rankByLevel_minus == "Medium" & 
                   confidence_rankByLevel_plus == "Medium" ~ "Medium",
                 confidence_rankByLevel_minus == "High" & 
                   confidence_rankByLevel_plus == "Low" ~ "Low",
                 confidence_rankByLevel_minus == "Low" & 
                   confidence_rankByLevel_plus == "High" ~ "Low",
                 confidence_rankByLevel_minus == "Medium" & 
                   confidence_rankByLevel_plus == "Low" ~ "Low",
                 confidence_rankByLevel_minus == "Low" & 
                   confidence_rankByLevel_plus == "Medium" ~ "Low",
                 confidence_rankByLevel_minus == "Low" & 
                   confidence_rankByLevel_plus == "Low" ~ "Low",
                 TRUE ~ NA)) %>%
      mutate(confidence_rankByLevel_minus = 
               factor(confidence_rankByLevel_minus, 
                      levels = c("High", "Medium", "Low")),
             confidence_rankByLevel_plus = 
               factor(confidence_rankByLevel_plus, 
                      levels = c("High", "Medium", "Low")),
             confidence_rankByLevel_minusPlus = 
               factor(confidence_rankByLevel_minusPlus,
                      levels = c("High", "Medium", "Low")))
    
    if(AH_name == "USAH") {

      output$results <-
        output$results %>%
        select(name, version, location, scenario, 
               level, levelName_full, levelName,
               Node, metric, value, value_minus, value_plus,
               rank_byLevel, confidence_rankByLevel_minusPlus, 
               confidence_rankByLevel_minus, confidence_rankByLevel_plus, 
               change_rankByLevel_minus, change_rankByLevel_plus, 
               rank_byLevel_minus, rank_byLevel_plus)
      
    } else {
      
      output$results <-
        output$results %>%
        select(name, 
               level, levelName_full, levelName,
               Node, metric, value, value_minus, value_plus,
               rank_byLevel, confidence_rankByLevel_minusPlus, 
               confidence_rankByLevel_minus, confidence_rankByLevel_plus, 
               change_rankByLevel_minus, change_rankByLevel_plus, 
               rank_byLevel_minus, rank_byLevel_plus)
      
    }

    return(output)
    
  }