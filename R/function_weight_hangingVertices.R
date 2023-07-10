weight_hangingVertices <- function(edgelist, proxyWeight = 0) {
  
  # Note: apply_geoData is best applied before apply_stressors (or changing weight 
  # at upper levels rather than l4ORP_l5PO); not tested backward order propagation
  
  require(tidyverse)
  
  # Check for ORPs which have all links weighted the proxyWeight (to imitate removal) 
  # and set all their links to GFs to the proxyWeight
  l4ORP_l5PO_excluded <- 
    edgelist %>% 
    filter(layer == "l4ORP_l5PO") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight == proxyWeight) %>%
    select(-weight)
  
  l4ORP_l5PO_included <- 
    edgelist %>% 
    filter(layer == "l4ORP_l5PO") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight > proxyWeight) %>%
    select(-weight)
  
  ORP_excluded <-
    dplyr::setdiff(l4ORP_l5PO_excluded, l4ORP_l5PO_included) %>% pull(from)
  
  if(is_empty(ORP_excluded) == TRUE) {
    
    return(edgelist)
    
  } else {
    
    edgesNew <- 
      edgelist %>%
      filter(from %in% all_of(ORP_excluded) | to %in% all_of(ORP_excluded)) %>% # Set aside excluded ORP vertices
      mutate(weight = proxyWeight) %>%
      rename(weightNew = weight)
    
    edgelist <- edgelist %>% weight_edges(edgesNew)
    
    return(edgelist)
    
  }
  
  l3GF_l4ORP_excluded <- 
    edgelist %>% 
    filter(layer == "l3GF_4ORP") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight == proxyWeight) %>%
    select(-weight)
  
  l3GF_l4ORP_included <- 
    edgelist %>% 
    filter(layer == "l3GF_l4ORP") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight > proxyWeight) %>%
    select(-weight)
  
  GF_excluded <-
    dplyr::setdiff(l3GF_l4ORP_excluded, l3GF_l4ORP_included) %>% pull(from)
  
  if(is_empty(GF_excluded) == TRUE) {
    
    return(edgelist)
    
  } else {
    
    edgesNew <- 
      edgelist %>%
      filter(from %in% all_of(GF_excluded) | to %in% all_of(GF_excluded)) %>% # Set aside excluded GF vertices
      mutate(weight = proxyWeight) %>%
      rename(weightNew = weight)
    
    edgelist <- edgelist %>% weight_edges(edgesNew)
    
    return(edgelist)
    
  }
  
  l2VPM_l3GF_excluded <- 
    edgelist %>% 
    filter(layer == "l2VPM_l3GF") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight == proxyWeight) %>%
    select(-weight)
  
  l2VPM_l3GF_included <- 
    edgelist %>% 
    filter(layer == "l2VPM_l3GF") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight > proxyWeight) %>%
    select(-weight)
  
  VPM_excluded <-
    dplyr::setdiff(l2VPM_l3GF_excluded, l2VPM_l3GF_included) %>% pull(from)
  
  if(is_empty(VPM_excluded) == TRUE) {
    
    return(edgelist)
    
  } else {
    
    edgesNew <- 
      edgelist %>%
      filter(from %in% all_of(VPM_excluded) | to %in% all_of(VPM_excluded)) %>% # Set aside excluded VPM vertices
      mutate(weight = proxyWeight) %>%
      rename(weightNew = weight)
    
    edgelist <- edgelist %>% weight_edges(edgesNew)
    
    return(edgelist)
    
  }
  
  l1FP_l2VPM_excluded <- 
    edgelist %>% 
    filter(layer == "l1FP_l2VPM") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight == proxyWeight) %>%
    select(-weight)
  
  l1FP_l2VPM_included <- 
    edgelist %>% 
    filter(layer == "l1FP_l2VPM") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight > proxyWeight) %>%
    select(-weight)
  
  FP_excluded <-
    dplyr::setdiff(l1FP_l2VPM_excluded, l1FP_l2VPM_included) %>% pull(from)
  
  if(is_empty(FP_excluded) == TRUE) {
    
    return(edgelist)
    
  } else {
    
    edgesNew <- 
      edgelist %>%
      filter(from %in% all_of(FP_excluded) | to %in% all_of(FP_excluded)) %>% # Set aside excluded FP vertices
      mutate(weight = proxyWeight) %>%
      rename(weightNew = weight)
    
    edgelist <- edgelist %>% weight_edges(edgesNew)
    
    return(edgelist)
    
  }
  
}