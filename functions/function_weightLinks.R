weightLinks <- function(x, y, type = c("edgelist", "igraph")) {
  
  # x is the abstraction hierarchy to modify, y consists of the new weightings
  # Both must be of the same input type - edgelist or igraph
  
  internal_weightLinks_edgelist <- function(x,y) {
    x %>% 
      left_join(y, by = c("layers", "from", "to")) %>% 
      mutate(weight = coalesce(weightNew, weight)) %>% 
      select(-weightNew) %>%
      arrange(layers, from, to)
  }
  
  if(type == "igraph") {
    x <- x %>% get.data.frame %>% as_tibble()
    y <- y %>% get.data.frame %>% as_tibble() %>% rename(weightNew = weight)
  }
  
  output <- internal_weightLinks_edgelist(x,y)
  
  if(type == "igraph") { output <- output %>% edgelist_to_igraph() }
  
  return(output)
  
}