# Function to generate USAH_scenario output based on a new scenario edgelist
apply_scenario <- function(USAH_template,
                           USAH_input, 
                           edgelist_scenario,
                           proxyWeight = 0) {
  
  # Create dataframe of only the included vertices
  
  edges_to <- 
    edgelist_scenario %>% 
    select(to, weight) %>% 
    rename(vName = to)
  
  edges_from <- 
    edgelist_scenario %>% 
    select(from, weight) %>% 
    rename(vName = from)
  
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
    filter(weight == proxyWeight) %>% 
    select(-weight)
  
  all_excluded <- 
    dplyr::setdiff(edges_excluded, edges_included) %>% 
    pull(vName)
  
  # Create list object for output
  USAH_scenario <- list()
  
  # Keep track of included/excluded nodes even if they are not fully removed (just edges set to proxyWeight)
  
  USAH_scenario$vIncluded <- 
    USAH_template$vIncluded %>% 
    filter(!vName %in% all_of(all_excluded))
  
  USAH_scenario$vExcluded <- 
    USAH_template$vIncluded %>% 
    filter(vName %in% all_of(all_excluded))
  
  # Attach scenario-specific edgelist
  USAH_scenario$edgelist <- 
    edgelist_scenario %>%
    filter(weight != 0)

  # Create scenario-specific adjMat
  USAH_scenario$adjMat <- 
    USAH_scenario$edgelist %>%
    edgelist_to_adjMat(vInfo = USAH_scenario$vIncluded)
  
  # Create scenario-specific igraph
  USAH_scenario$igraph <- 
    USAH_scenario$edgelist %>%
    edgelist_to_igraph(vInfo = USAH_scenario$vIncluded)
  
  # Create scenario-specific results based on weighted vertex betweenness centrality
  USAH_scenario$results <- 
    getResults(igraph = USAH_scenario$igraph, 
               vInfo = USAH_scenario$vIncluded)
  
  # Create scenario-specific summary of network
  USAH_scenario$summary <- 
    summarise_ah(vIncluded = USAH_scenario$vIncluded, # Create summary of vertices by level
                 edgelist = USAH_scenario$edgelist, # Create summary of edges by layer
                 proxyWeight = proxyWeight) # Specify proxyWeight
  
  return(USAH_scenario)
  
}