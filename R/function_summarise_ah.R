summarise_AH <- function(vIncluded, edgelist, proxyWeight = 0) {
  
  require(janitor)
  
  vertices <- 
    vIncluded %>% 
    group_by(level, levelName_full, levelName) %>% 
    count() %>% 
    janitor::adorn_totals("row") %>%
    rename(n_vertices = n)
  
  edgelist_true <- edgelist %>% filter(weight != proxyWeight)
  
  edges <- 
    edgelist_true %>% 
    group_by(layer) %>% 
    count() %>% 
    ungroup(layer) %>%
    janitor::adorn_totals("row") %>%
    rename(n_edges = n)
  
  output <- list("vertices" = vertices, "edges" = edges)
  
  return(output)
  
}