update_subnetworks <- function(edgelist, vInfo_full) {
    
  require(stringr)
  require(stats)
  
  # Extract vInfo_full column names
  namesInfo <- names(vInfo_full)
  
  # Find subnetwork column names
  namesSubnetworks <- namesInfo[stringr::str_detect(namesInfo, "subnetwork")]
  
  # Internal function to assign subnetworks, based on assignment at objects level 
  # and propagating assignment upward to all connected vertices
  internal_assign_subnetworks <- function(edgelist, 
                                          vInfo_full, 
                                          subnetworkColName) {
    
    # Create dummy edge list for subnetwork assignment
    Node <- vInfo_full$Node
    edgelist$fromlayer <- 
      vInfo_full$level[match(edgelist$from, Node)] # Edges from
    edgelist$tolayer <- 
      vInfo_full$level[match(edgelist$to, Node)] # Edges to
    
    # Layer 5 key from input data
    key <- 
      list(key_L5 = 
             vInfo_full %>% 
             filter(level == 5) %>%
             select('Node', subnetworkColName))
    
    
    # Generate key by loop
    for(i in 5:2) {
      
      temp <- key[[paste0("key_L", i)]] %>% as.data.frame
      
      key[[paste0("key_L", i-1)]] <- 
        edgelist %>%
        mutate(x = temp[, subnetworkColName][match(edgelist$to, temp$'Node')]) %>%
        as_tibble() %>%
        stats::setNames(c("layer", "from", "to", "weight", "fromlevel", "tolevel", 
                          subnetworkColName)) %>%
        filter(tolevel == i) %>%
        select(from, subnetworkColName) %>%
        distinct() %>%
        arrange(from, desc(!! rlang::sym(subnetworkColName))) %>%
        filter(!duplicated(from)) %>%
        rename(`Node` = from)
      
    }
    
    key
    
    key = key %>% do.call("rbind", .)
    
    vInfo_full %>%
      select(-all_of(subnetworkColName)) %>%
      left_join(key, by = "Node") %>% 
      select(Node, subnetworkColName)
    
  }
  
  newSubnetworks <-
    lapply(namesSubnetworks, function(x) { 
      internal_assign_subnetworks(
        edgelist = edgelist,
        vInfo_full = vInfo_full,
        subnetworkColName = x) }) %>% 
    reduce(full_join, by = "Node")
  
  output <-
    vInfo_full %>% 
    select(-all_of(namesSubnetworks)) %>% 
    full_join(newSubnetworks, by = "Node")
  
  return(output)
    
}