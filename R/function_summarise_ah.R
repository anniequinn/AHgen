# Function to summarise abstraction hierarchy
# This is a function to summarise the number of vertices at each layer of an
# abstraction hierarchy; and the number of edges at each level of an 
# abstraction hierarchy
summarise_ah <- function(vIncluded, 
                         edgelist, 
                         proxyWeight = 0) {
  
  require(janitor)
  
  vertices <- 
    vIncluded %>% 
    group_by(level, levelName_full, levelName) %>% 
    count() %>% 
    janitor::adorn_totals("row") %>%
    rename(n_vertices = n)
  
  edgelist_true <- 
    edgelist %>% 
    filter(weight != proxyWeight)
  
  edges <- 
    edgelist_true %>% 
    group_by(layer) %>% 
    count() %>% 
    ungroup(layer) %>%
    janitor::adorn_totals("row") %>%
    rename(n_edges = n)
  
  list("vertices" = vertices, "edges" = edges)
  
}