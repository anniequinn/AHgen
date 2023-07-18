# Find & adjust any affected edges by x%
# Note that this sets any negative values to minimum 0 and any values > 1 to maximum 1
calc_sensitivity <- function(edges, sign, pct) {
  
  if(sign == "plus") {
    
    edgesNew = 
      edges %>% 
      mutate(weight = ifelse(weight != 1, (weight * (1 + pct)), weight))
    
  } else if(sign == "minus") {
    
    edgesNew = 
      edges %>% 
      mutate(weight = ifelse(weight != 1, (weight * (1 - pct)), weight))
  }
  
  edgesNew =
    edgesNew %>%
    mutate(weight = ifelse(weight > 1, 1, weight)) %>%
    mutate(weight = ifelse(weight < 0, 0, weight)) %>%
    weight_hangingVertices(remove = FALSE) # For vertices with downward weights all = 0, make upward weights = 0
  
  return(edgesNew)
  
}