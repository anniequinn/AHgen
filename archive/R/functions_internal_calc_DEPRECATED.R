function_merge_vInfo <- function(dt, vInfo) {
  
  vInfo %>% 
    select(level, levelName, Node) %>%
    right_join(dt, by = "Node")
  
}


function_igraphResultFormatting <- function(igraphresult, name) { 
  
  igraphresult %>% 
    as.data.frame %>% 
    rownames_to_column %>% 
    setNames(c("Node", name)) %>%
    as_tibble()
  
}