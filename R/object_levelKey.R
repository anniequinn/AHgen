levelKey <- 
  tibble(level = 1:5, 
         levelName = c("Functional purposes", 
                       "Values and priority measures", 
                       "Generalised functions", 
                       "Object-related processes", 
                       "Physical objects")) %>%
  mutate(abbr = paste0("l", level, c("FP", "VPM", "GF", "ORP", "PO")))