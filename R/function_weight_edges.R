weight_edges <- function(edgelist, edgesNew, remove = TRUE) {
  
  output <- 
    edgelist %>% 
    left_join(edgesNew, by = c("layer", "from", "to")) %>% 
    mutate(weight = coalesce(weightNew, weight)) %>% 
    select(-weightNew) %>%
    arrange(layer, from, to)
  
  if(remove == TRUE) {
    
    output <- output %>% filter(weight != 0)
    
    return(output)
    
  } else if(remove == FALSE) {
    
    return(output)
    
  }
  
}