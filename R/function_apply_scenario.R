# Function to generate AH_scenario output based on a new scenario edgelist
apply_scenario <- function(AH_input, 
                           edgelist_scenario,
                           proxyWeight = 0,
                           name, 
                           version = NULL, location = NULL, scenario = NULL) {
  
  require(tidyverse)
  
  # The input edgelist should include edges with a weight of 0 
  # (using remove = FALSE in weight_edges & weight_hangingVertices)
  # rather than completely removing theme (using remove = TRUE) 
  
  # This is so that we can track nodes to be excluded in the following steps
  
  # Create dataframe of only the included vertices
  edges_to <- 
    edgelist_scenario %>% 
    select(to, weight) %>% 
    rename(Node = to)
  
  edges_from <- 
    edgelist_scenario %>% 
    select(from, weight) %>% 
    rename(Node = from)
  
  edges_all <- 
    edges_to %>% 
    rbind(edges_from) %>% 
    unique()
  
  edges_included <- 
    edges_all %>% 
    filter(weight > proxyWeight) %>% 
    select(-weight)
  
  edges_excluded <- 
    edges_all %>% 
    filter(weight <= proxyWeight) %>% 
    select(-weight)
  
  all_excluded <- 
    dplyr::setdiff(edges_excluded, edges_included) %>% 
    pull(Node)
  
  # Create list object for output
  AH_scenario <- list()
  
  # Keep track of included/excluded nodes even if they are not fully removed (just edges set to proxyWeight)
  
  AH_scenario$vIncluded <- 
    AH_input$vIncluded %>% 
    filter(!Node %in% all_of(all_excluded))
  
  AH_scenario$vExcluded <- 
    AH_input$vIncluded %>% 
    filter(Node %in% all_of(all_excluded)) %>%
    rbind(AH_input$vExcluded)
  
  # Attach scenario-specific edgelist
  AH_scenario$edgelist <- 
    edgelist_scenario %>%
    filter(weight != 0) # Now we can remove the edges we want to remove so that the number of vertices in the igraph object is what we want
  
  # Create scenario-specific adjMat
  AH_scenario$adjMat <- 
    AH_scenario$edgelist %>%
    edgelist_to_adjMat(vInfo = AH_scenario$vIncluded)
  
  # Create scenario-specific igraph
  AH_scenario$igraph <- 
    AH_scenario$edgelist %>%
    edgelist_to_igraph(vInfo = AH_scenario$vIncluded)
  
  # Create scenario-specific results based on weighted vertex betweenness centrality
  AH_scenario$results <- 
    gen_results(igraph = AH_scenario$igraph, vInfo = AH_scenario$vIncluded, 
                name = name, version = version, location = location, scenario = scenario)
  
  # Create scenario-specific summary of network
  AH_scenario$summary <- 
    summarise_AH(vIncluded = AH_scenario$vIncluded, # Create summary of vertices by level
                 edgelist = AH_scenario$edgelist, # Create summary of edges by layer
                 proxyWeight = proxyWeight) # Specify proxyWeight
  
  return(AH_scenario)
  
}