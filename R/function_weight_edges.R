weight_edges <- function(edgelist, edgesNew) {
  
  output <- 
    edgelist %>% 
    left_join(edgesNew, by = c("layer", "from", "to")) %>% 
    mutate(weight = coalesce(weightNew, weight)) %>% 
    select(-weightNew) %>%
    filter(weight != 0) %>%
    arrange(layer, from, to)
  
  return(output)
  
}