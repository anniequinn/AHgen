gen_AH <- function(vInfo_full = NULL, 
                   vIncluded, 
                   vExcluded = NULL,
                   adjMat,
                   name,
                   version = NULL, 
                   location = NULL, 
                   scenario = NULL){
  
  # Create edgelist
  edgelist <- adjMat %>% adjMat_to_edgelist(vInfo = vIncluded)
  
  # Create igraph
  igraph <- adjMat %>% adjMat_to_igraph(vInfo = vIncluded)
  
  if(is.null(vInfo_full)) {
    
    vInfo_full <- list()
    
  }
  
  else {
    
    # If changes have been made to how objects have been assigned to subnetworks
    # Then update assignment of subnetworks to connected vertices and 
    # generate a new vInfo_full
    vInfo_full <- 
      update_subnetworks(
        edgelist = edgelist, 
        vInfo_full = vInfo_full) %>%
      dplyr::relocate(level, levelName, levelName_full, Node, definition, reference)
    
  }
  
  if(is.null(vExcluded)) {
    
    vExcluded <- list()
  
  AH <-
    list("vInfo" = vInfo_full, # Attach any additional info for vertices e.g. colours, subnetworks
         "vIncluded" = vIncluded, # Attach dataframe of included vertices (in most cases this will be all vertices)
         "vExcluded" = vExcluded, # Attach dataframe of excluded vertices (in most cases this will be no verticies i.e. an empty list for comparison to other scenarios)
         "adjMat" = adjMat, # Attach adjacency matrix
         "edgelist" = edgelist, # Attach edgelist
         "igraph" = igraph, # Attach igraph
         "results" = # Create and attach results
           gen_results(
             igraph = igraph, 
             vInfo = vIncluded, 
             name = name, version = version, location = location, scenario = scenario),
         "summary" = # Create and attach a summary
           summarise_AH(
             vIncluded = vIncluded, # Create summary of vertices by level
             edgelist = edgelist)) # Create summary of edges by layer
  
  return(AH)
  
}