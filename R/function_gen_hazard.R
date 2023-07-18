# Generate edgesNew based on proportion of Resources i.e. Physical objects still functional 
# as represented in a countCompared OSMtidy output
gen_hazard <- function(vInfo_template_full, 
                       edgelist, 
                       countCompared, 
                       hazard = NA, 
                       proxyWeight = 0) {
  
  # Create vector of physical objects affected
  resourcesAffected <-
    countCompared$compare_byPhysicalObject %>% 
    filter(!is.na(total_prop)) %>% # Filter out results for Resources that were not detected in the second scenario
    pull(physicalObject)
  
  if (hazard == "drought") {
    
    # Create check for affected Resources by hazard type (drought)
    resourcesNotAffected_drought <-
      vInfo_template_full %>% 
      filter(level == 5) %>% 
      filter(subnetwork_hazard_drought == FALSE) %>%
      pull(Node)
    
    # Update 
    resourcesAffected <- 
      resourcesAffected[!resourcesAffected %in% resourcesNotAffected_drought]
    
  }
  
  if (hazard == "flood") {
    
    # Create check for affected Resources by hazard type (flood)
    resourcesNotAffected_flood <-
      vInfo_template_full %>% 
      filter(level == 5) %>% 
      filter(subnetwork_hazard_flood == FALSE) %>%
      pull(Node)
    
    # Update resourcesAffected
    resourcesAffected <- 
      resourcesAffected[!resourcesAffected %in% resourcesNotAffected_flood]
    
  }
  
  # Create dataframe of weightNew for Resources affected
  resourcesAffected_weights <-
    countCompared$compare_byPhysicalObject %>% # Extract compare_byPhysicalObject
    filter(!is.na(total_prop)) %>% # Filter out results for Resources that were not detected in the second scenario
    filter(physicalObject %in% resourcesAffected) %>% # Use updated resourcesAffected for hazard scenarios to use only weights for these objects
    rename(to = physicalObject, weightNew = total_prop) %>% # Rename for joining to edgelist_input
    mutate(weightNew = replace(weightNew, weightNew == 0, proxyWeight)) %>% # Resources for complete removal will = 0, replace this with proxyWeight
    mutate(weightNew = replace(weightNew, weightNew < 0.0000000001, proxyWeight)) %>% # Weights assigned by OSMtidy outputs which are effectively removed should be set to the proxyWeight
    select(to, weightNew)
  
  # Join dataframes
  edgesNew <-
    edgelist %>%
    filter(to %in% resourcesAffected) %>% # Filter edgelist_input to only Resources affected
    left_join(resourcesAffected_weights, by = "to") %>% # Join
    select(-weight) # Remove weight column now superseded by weightNew
  
  return(edgesNew)
  
}