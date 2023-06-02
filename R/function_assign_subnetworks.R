# Function to assign subnetworks, based on assignment at objects level and
# propagating assignment upward to all connected vertices
assign_subnetworks <- 
  function(edgelist_template, vInfo_template_full, subnetworkColName) {
    
    # Create dummy edge list for subnetwork assignment
    Node <- vInfo_template_full$Node
    edgelist_template$fromlayer <- 
      vInfo_template_full$level[match(edgelist_template$from, Node)] # Edges from
    edgelist_template$tolayer <- 
      vInfo_template_full$level[match(edgelist_template$to, Node)] # Edges to
    
    # Layer 5 key from input data
    key <- 
      list(key_L5 = vInfo_template_full %>% filter(level == 5) %>%
             select('Node', subnetworkColName))
    
    
    # Generate key by loop
    for(i in 5:2) {
      
      temp <- key[[paste0("key_L", i)]] %>% as.data.frame
      
      key[[paste0("key_L", i-1)]] <- 
        edgelist_template %>%
        mutate(x = temp[, subnetworkColName][match(edgelist_template$to, temp$'Node')]) %>%
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
    
    vInfo_template_full %>%
      select(-all_of(subnetworkColName)) %>%
      left_join(key, by = "Node") %>% select(Node, subnetworkColName)
    
  }
