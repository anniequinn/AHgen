layerKey <- 
  tibble(layer = 1:5, 
         layerName = c("Functional purposes", 
                       "Values and priority measures", 
                       "Generalised functions", 
                       "Object-related processes", 
                       "Physical objects")) %>%
  mutate(abbr = paste0("l", layer, c("FP", "VPM", "GF", "ORP", "PO")))