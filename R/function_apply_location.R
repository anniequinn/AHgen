apply_location <- function(desc_check,
                           vInfo_template, 
                           edgelist_template, 
                           proxyWeight = 0,
                           AH_name, 
                           AH_version, 
                           AH_location, 
                           AH_scenario) {
  
  require(tidyverse)

  PO_excluded <- 
    desc_check$notDetected_exclude %>%  # Take dataframe of desc terms not detected in this location which should be excluded
    select(resource) %>% unique() %>% pull(resource) # Create vector of physical objects to be excluded
  
  edgesNew <-
    edgelist_template %>%
    filter(from %in% all_of(PO_excluded) | to %in% all_of(PO_excluded)) %>% # Set aside excluded object vertices from edgelist scenario
    mutate(weight = proxyWeight) %>%
    rename(weightNew = weight)
  
  edgelist_location_step1 <- 
    edgelist_template %>% 
    weight_edges(edgesNew, remove = FALSE) # Set to remove = FALSE so that we can track nodes to be excluded in the following steps
  
  # Check for hanging vertices which have all links downward with proxyWeight (to imitate removal)
  # and iteratively set upward links with proxyWeight (to imitate removal) as relevant
  edgelist_location_step2 <- 
    weight_hangingVertices(edgelist = edgelist_location_step1,
                           proxyWeight = proxyWeight,
                           remove = FALSE) # Set to remove = FALSE so that we can track nodes to be excluded in next steps
  
  # Create dataframe of only the included vertices
  
  to <- 
    edgelist_location_step2 %>% 
    select(to, weight) %>% 
    rename(Node = to)
  
  from <- 
    edgelist_location_step2 %>% 
    select(from, weight) %>% 
    rename(Node = from)
  
  all_excluded <- 
    to %>% rbind(from) %>% unique() %>% table() %>% as.data.frame() %>% 
    filter(weight == 1, Freq == 0) %>% mutate_if(is.factor, as.character) %>% # not precise but works
    pull(Node)
  
  vInfo_location <- 
    vInfo_template %>% 
    filter(!Node %in% all_of(all_excluded)) # Keep track of included/excluded nodes even if they are not fully removed (just edges set to proxyWeight)
  
  vInfo_tmp <-
    if(proxyWeight == 0) {vInfo_location} else {vInfo_template} # Not sure this is still needed
  
  # Create edgelist
  edgelist_location <-
    edgelist_location_step2 %>% 
    filter(weight != 0) # Now we can remove the edges we want to remove so that the number of vertices in the igraph object is what we want
  
  # Create igraph
  igraph_location <- 
    edgelist_location %>%
    edgelist_to_igraph(vInfo_tmp) # Create location-specific igraph, because no vertices have been removed use vInfo_template
  
  # Create adjMat
  adjMat_location <-
    edgelist_location %>%
    edgelist_to_adjMat(vInfo_tmp)
  
  # Create output
  output <- 
    list("desc_check" = desc_check, # Attach desc_check list detailing which desc were detected in this location
         "vIncluded" = vInfo_location, # Attach dataframe of included vertices
         "vExcluded" = dplyr::setdiff(vInfo_template, vInfo_location), # Create and attach dataframe of excluded vertices
         "adjMat" = adjMat_location, # Create location-specific adjacency matrix
         "edgelist" = edgelist_location, # Attach location-specific edgelist
         "igraph" = igraph_location, # Attach location-specific igraph
         "results" = 
           gen_results(
             igraph = igraph_location, 
             vInfo = vInfo_tmp, 
             AH_name = AH_name, 
             AH_version = AH_version, 
             AH_location = AH_location, 
             AH_scenario = AH_scenario), # Create location-specific results
         "summary" = 
           summarise_AH(
             vIncluded = vInfo_location, # Create summary of vertices by level
             edgelist = edgelist_location, # Create summary of edges by layer
             proxyWeight = proxyWeight)) # Specify proxyWeight
  
  return(output)
  
}