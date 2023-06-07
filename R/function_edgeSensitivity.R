# Find & adjust any affected edges by x%
# Note that this sets any negative values to minimum 0
# and any values > 1 to maximum 1
edgeSensitivity <- function(AH_input, sign, pct) {
  
  if(sign == "plus") {
    
    edgelistNew = 
      AH_input$edgelist %>% 
      mutate(weight = ifelse(weight != 1, (weight * (1 + pct)), weight)) %>%
      mutate(weight = ifelse(weight > 1, 1, weight)) %>%
      mutate(weight = ifelse(weight < 0, 0, weight)) %>%
      weight_hangingVertices() # For vertices with downward weights all = 0, make upward weights = 0
    
  } else if(sign == "minus") {
    
    edgelistNew = 
      AH_input$edgelist %>% 
      mutate(weight = ifelse(weight != 1, (weight * (1 - pct)), weight)) %>%
      mutate(weight = ifelse(weight > 1, 1, weight)) %>%
      mutate(weight = ifelse(weight < 0, 0, weight)) %>%
      weight_hangingVertices() # For vertices with downward weights all = 0, make upward weights = 0
    
  }
  
  return(edgelistNew)
  
}