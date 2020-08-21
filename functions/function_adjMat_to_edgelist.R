adjMat_to_edgelist <- function(adjMat, outputList = FALSE) { 
  
  mat <- adjMat %>% select(4:ncol(.))
  mat[upper.tri(mat, diag = TRUE)] <- NA
  
  source("functions/function_melt2.R")
  
  step1 <- 
    adjMat %>% 
    select(nodeName) %>% 
    cbind(mat) %>% 
    as_tibble() %>%
    melt2(measure.vars = c(2:ncol(.))) %>% 
    filter(value == 1)
  
  step2 <- 
    adjMat %>% 
    select(layer, layerName, nodeName) %>% 
    left_join(layerKey, by = c("layer", "layerName")) %>% 
    select(abbr, nodeName)
  
  output <- 
    step1 %>%
    select(from = variable, to = nodeName, weight = value) %>%
    left_join(step2 %>% select(layerFrom = abbr, from = nodeName), by = "from") %>%
    left_join(step2 %>% select(layerTo = abbr, to = nodeName), by = "to") %>%
    mutate(layers = paste(layerFrom, layerTo, sep = "_")) %>%
    select(layers, from, to, weight) %>%
    arrange(layers, from, to)
  
  if(outputList == TRUE) { output <- output %>% split(., .$layers) } # Not sure if required
  
  return(output)
  
}