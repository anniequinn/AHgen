# Function to summarise abstraction hierarchy
# This is a function to summarise the number of vertices at each layer of an
# abstraction hierarchy; and the number of edges at each level of an 
# abstraction hierarchy
summarise_ah <- function(vInfo, edgelist) {
  
  require(janitor)
  
  vertices <- 
    vInfo %>% group_by(level, levelName) %>% count %>% adorn_totals("row")
  edges <- 
    edgelist %>% group_by(layer) %>% count %>% janitor::adorn_totals("row")
  
  list("vertices" = vertices, "edges" = edges)
  
}