# Generate edgesNew based on proportion of physical objects still functional 
# as represented in a countCompared OSMtidy output
gen_hazard <- function(vInfo_template_full, edgelist, 
                       countCompared, hazard = NA, proxyWeight = 0) {
  
  # Create vector of physical objects affected
  objectsAffected <-
    countCompared$compare_byPhysicalObject %>% 
    filter(!is.na(total_prop)) %>% # Filter out results for objects that were not detected in the second scenario
    pull(physicalObject)
  
  if (hazard == "drought") {
    
    # Create check for affected physical objects by hazard type (drought)
    objectsNotAffected_drought <-
      vInfo_template_full %>% 
      filter(level == 5) %>% 
      filter(subnetwork_hazard_drought == FALSE) %>%
      pull(Node)
    
    # Update 
    objectsAffected <- 
      objectsAffected[!objectsAffected %in% objectsNotAffected_drought]
    
  }
  
  if (hazard == "flood") {
    
    # Create check for affected physical objects by hazard type (flood)
    objectsNotAffected_flood <-
      vInfo_template_full %>% 
      filter(level == 5) %>% 
      filter(subnetwork_hazard_flood == FALSE) %>%
      pull(Node)
    
    # Update objectsAffected
    objectsAffected <- 
      objectsAffected[!objectsAffected %in% objectsNotAffected_flood]
    
  }
  
  # Create dataframe of weightNew for physical objects affected
  objectsAffected_weights <-
    countCompared$compare_byPhysicalObject %>% # Extract compare_byPhysicalObject
    filter(!is.na(total_prop)) %>% # Filter out results for objects that were not detected in the second scenario
    filter(physicalObject %in% objectsAffected) %>% # Use updated objectsAffected for hazard scenarios to use only weights for these objects
    rename(to = physicalObject, weightNew = total_prop) %>% # Rename for joining to edgelist_input
    mutate(weightNew = replace(weightNew, weightNew == 0, proxyWeight)) %>% # Objects for complete removal will = 0, replace this with proxyWeight
    mutate(weightNew = replace(weightNew, weightNew < 0.0000000001, proxyWeight)) %>% # Weights assigned by OSMtidy outputs which are effectively removed should be set to the proxyWeight
    select(to, weightNew)
  
  # Join dataframes
  edgesNew <-
    edgelist %>%
    filter(to %in% objectsAffected) %>% # Filter edgelist_input to only physical objects affected
    left_join(objectsAffected_weights, by = "to") %>% # Join
    select(-weight) # Remove weight column now superseded by weightNew
  
  return(edgesNew)
  
}