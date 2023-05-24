detect_desc <- function(geoData, key) {
  
  require(sf)
  
  # Determine if input data format is simple feature class
  class <- class(geoData) %>% as.vector()
  
  if("sf" %in% class) { # If geoData is simple feature class
    
    objects_location <- 
      geoData %>% 
      as_tibble() %>% 
      mutate(geometry = sf::st_as_text(geometry)) # Reformat geometry as character class for easier filtering
    
  } else {
    
    objects_location <- geoData %>% as_tibble()
    
  }
  
  objects_location <- 
    objects_location %>%
    select(desc) %>% # Select desc column from OSMtidy output
    unique() %>% # Find unique entries
    mutate(OSMtidyDetected = "TRUE") %>% # Create new column to flag that these unique desc were detected by OSMtidy
    full_join(key, by = "desc") %>% # Join this to the OSMtidy-AHgen key
    replace_na(list(OSMtidyDetected = "FALSE")) # Flag the desc from the key that were not detected by OSMtidy
  
  desc_notDetected <- objects_location %>% filter(OSMtidyDetected == "FALSE") # Create dataframe of desc terms not detected in this location
  
  PO_detected_vec <- # Create a vector of physical object names that were detected in this location
    objects_location %>%
    filter(OSMtidyDetected == "TRUE") %>% # Filter only detected desc terms
    select(physicalObject) %>% unique() %>% pull(physicalObject) # Create a vector of the associated physical object names
  
  PO_notDetected <- # Create a tibble of physical object types not detected in this location
    desc_notDetected %>% # Take the dataframe of desc terms not detected in this location
    filter(!physicalObject %in% PO_detected_vec) %>% # Remove the rows pertaining to physical objects that have some other desc term that has detected them
    arrange(physicalObject) # Order rows alphabetically
  
  desc_notDetected_alwaysInclude_vec <-
    PO_notDetected %>% # Take the dataframe of physical objects not detected in this location
    filter(OSMtidyStage == "alwaysIncluded") %>% # Find the rows for physical objects which according to the key should always be included
    select(physicalObject) %>% unique() %>% pull(physicalObject) # Create a vector of physical object names that were not detected in this location but according to the key should always be included
  
  PO_notDetected_excluded <-
    PO_notDetected %>%  # Take the dataframe of physical objects not detected in this location
    filter(!physicalObject %in% desc_notDetected_alwaysInclude_vec) # Create a dataframe of all descs for physical object names which were not detected in this location and should be excluded
  
  PO_notDetected_alwaysIncluded <-
    PO_notDetected %>% # Take the dataframe of physical objects not detected in this location
    filter(physicalObject %in% desc_notDetected_alwaysInclude_vec) # Create a dataframe of all descs for physical object names which were not detected in this location but according to the key should always be included
  
  output <- 
    list(PO_notDetected_excluded, PO_notDetected_alwaysIncluded, objects_location) # Create an output listing the not detected physical objects which should be excluded; not detected physical objects which should not be excluded; and the full breakdown of whether desc terms were detected in this location
  
  names(output) <- 
    c("notDetected_exclude", "notDetected_alwaysInclude", "keyApplication") # Attach names to list output
  
  return(output)
  
}