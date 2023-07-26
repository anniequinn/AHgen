visLayout <- function(edgelist, vInfo, minSpacing = 0, maxSpacing = 100, key) { 
    
    internal_horizontalLayout(edgelist = edgelist, vInfo = vInfo) %>%
      internal_horizontalLayout_spacing(., minSpacing = minSpacing, maxSpacing = maxSpacing) %>%
      internal_radialLayout(horizontalLayout = ., edgelist = edgelist, key = key) %>% 
      internal_genericLayout(radialLayout = .)
    
  }