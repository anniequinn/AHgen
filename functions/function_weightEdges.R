weightEdges <- function(x, y) {
  
  # x is the edge list to modify
  # y is the edge list with the new weightings
  
  output <- 
    x %>% 
    left_join(y, by = c("layer", "from", "to")) %>% 
    mutate(weight = coalesce(weightNew, weight)) %>% 
    select(-weightNew) %>%
    arrange(layer, from, to)
  
  return(output)
  
}