OSMtidy_to_AHgen <- function(geodata, key, drop = TRUE) {
  
  dt <- 
    read_csv(key, col_types = cols()) %>%
    filter(!is.na(desc)) # %>%
    #filter(!str_detect(physicalObject, "remove|Remove"))
  
  output <- 
    dt %>% 
    inner_join(geodata %>% as_tibble, by = "desc") %>% 
    st_as_sf() %>%
    select(physicalObject, desc, type, geometry, everything())
  
  if(drop == TRUE) { output <- output %>% select(-desc) }
  
  return(output)
  
}