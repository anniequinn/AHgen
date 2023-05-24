weightEdges <- function(edgelist_input, 
                        edgesNew) {
  
  output <- 
    edgelist_input %>% 
    left_join(edgesNew, by = c("layer", "from", "to")) %>% 
    mutate(weight = coalesce(weightNew, weight)) %>% 
    select(-weightNew) %>%
    arrange(layer, from, to)
  
  return(output)
  
}