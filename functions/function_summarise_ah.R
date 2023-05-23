# Function to summarise abstraction hierarchy
# This is a function to summarise the number of vertices at each layer of an
# abstraction hierarchy; and the number of edges at each level of an 
# abstraction hierarchy
summarise_ah <- function(vIncluded, 
                         edgelist, 
                         proxyWeight = 0.0000000001) {
  
  require(janitor)
  
  vertices <- 
    vIncluded %>% 
    group_by(level, levelName) %>% 
    count() %>% 
    janitor::adorn_totals("row")
  
  edgelist_true <- 
    edgelist %>% 
    filter(weight != proxyWeight)
  
  edges <- 
    edgelist_true %>% 
    group_by(layer) %>% 
    count() %>% 
    ungroup(layer) %>%
    janitor::adorn_totals("row")
  
  list("vertices" = vertices, "edges" = edges)
  
}