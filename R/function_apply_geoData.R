apply_geoData <- function(geoData, 
                          desc_detection,
                          vInfo_template, 
                          edgelist_template, 
                          proxyWeight = 0) {
  
  PO_excluded <- 
    desc_detection$notDetected_exclude %>%  # Take dataframe of desc terms not detected in this location which should be excluded
    select(physicalObject) %>% unique() %>% pull(physicalObject) # Create vector of physical objects to be excluded
  
  edgesNew <-
    edgelist_template %>%
    filter(from %in% all_of(PO_excluded) | to %in% all_of(PO_excluded)) %>% # Set aside excluded object vertices from edgelist scenario
    mutate(weight = proxyWeight) %>%
    rename(weightNew = weight)
  
  edgelist_location_step1 <- 
    edgelist_template %>% 
    weightEdges(edgesNew)
  
  # Check for hanging vertices which have all links downward with proxyWeight (to imitate removal)
  # and iteratively set upward links with proxyWeight (to imitate removal) as relevant
  edgelist_location_step2 <- 
    weight_hangingVertices(edgelist = edgelist_location_step1,
                           proxyWeight = proxyWeight)
  
  # Create dataframe of only the included vertices
  
  to <- 
    edgelist_location_step2 %>% 
    select(to, weight) %>% 
    rename(vName = to)
  
  from <- 
    edgelist_location_step2 %>% 
    select(from, weight) %>% 
    rename(vName = from)
  
  all_excluded <- 
    to %>% rbind(from) %>% unique() %>% table() %>% as.data.frame() %>% 
    filter(weight == 1, Freq == 0) %>% mutate_if(is.factor, as.character) %>% # not precise but works
    pull(vName)
  
  vInfo_location <- 
    vInfo_template %>% 
    filter(!vName %in% all_of(all_excluded)) # Keep track of included/excluded nodes even if they are not fully removed (just edges set to proxyWeight)
  
  vInfo_tmp <-
    if(proxyWeight == 0) {vInfo_location} else {vInfo_template} # Not sure this is still needed
  
  # Create edgelist
  edgelist_location <-
    edgelist_location_step2 %>% 
    filter(weight != 0)
    
  # Create igraph
  igraph_location <- 
    edgelist_location %>%
    edgelist_to_igraph(vInfo_tmp) # Create location-specific igraph, because no vertices have been removed use vInfo_template
  
  # Create adjMat
  adjMat_location <-
    edgelist_location %>%
    edgelist_to_adjMat(vInfo_tmp)
  
  # Create outputList
  outputList <- 
    list("desc_detection" = desc_detection, # Attach desc_detection list detailing which desc were detected in this location
         "vIncluded" = vInfo_location, # Attach dataframe of included vertices
         "vExcluded" = dplyr::setdiff(vInfo_template, vInfo_location), # Create and attach dataframe of excluded vertices
         "adjMat" = adjMat_location, # Create location-specific adjacency matrix
         "edgelist" = edgelist_location, # Attach location-specific edgelist
         "igraph" = igraph_location, # Attach location-specific igraph
         "results" = getResults(igraph = igraph_location, vInfo = vInfo_tmp), # Create location-specific results
         "summary" = summarise_ah(vIncluded = vInfo_location, # Create summary of vertices by level
                                  edgelist = edgelist_location, # Create summary of edges by layer
                                  proxyWeight = proxyWeight)) # Specify proxyWeight
  
  return(outputList)
  
}