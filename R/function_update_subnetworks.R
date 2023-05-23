# Function to generate vInfo_template_full with updated subnetwork assignment
update_subnetworks <- 
  function(edgelist_template, 
           vInfo_template_full) {
    
    # Extract vInfo_template_full column names
    namesInfo <- names(vInfo_template_full)
    
    # Find subnetwork column names
    namesSubnetworks <- namesInfo[str_detect(namesInfo, "subnetwork")]
    
    newSubnetworks <-
      lapply(namesSubnetworks, function(x) { 
        assign_subnetworks(
          edgelist_template = edgelist_template,
          vInfo_template_full = vInfo_template_full,
          subnetworkColName = x) }) %>% 
      reduce(full_join, by = "vName")
    
    output <-
      vInfo_template_full %>% 
      select(-all_of(namesSubnetworks)) %>% 
      full_join(newSubnetworks, by = "vName")
    
  }