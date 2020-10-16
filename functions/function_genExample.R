genExample <- function(edgelist, layerName = NULL) { 
  
  set.seed(1)
  
  if(!is.null(layerName)) { edgelist <- edgelist %>% filter(layer == layerName) }
  
  output <- 
    edgelist %>%
    rowwise() %>% 
    mutate(weightNew = sample(seq(0.01,1,0.01),1:n())) %>%
    select(-weight)
  
  return(output)
  
}