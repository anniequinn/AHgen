function_clusterGeo <- function(inputData, repetitions = 1, maxIterations = 500) {
  
  function_internal_clusterGeo <- function(input, maxIterations) {
    
    ll <- input %>% st_intersects
    le <- lengths(ll)
    ll <- ll[order(le, decreasing = TRUE)]
    simplified <- list()
    input2 <- input %>% st_as_sf %>% st_make_valid()
    
    for(i in 1:maxIterations) { 
      
      # Save together as one object
      simplified[[i]] <- input %>% slice(ll[[1]]) %>% summarise() %>% mutate(desc = "Sports and games; Clubhouse and outdoor facility (golf)")
      
      # Save indices of removals
      remove <- which(sapply(1:length(ll), function(x) { ll[[x]] %in% ll[[1]] %>% sum }) > 0)
      
      # Update input 
      input2 <- input2 %>% slice(-ll[[1]])
      
      # Update ll
      ll <- ll[-remove]
      if(length(ll) == 0) break
      
    }
    
    simplified <- simplified %>% mapedit:::combine_list_of_sf() # Final golf course output
    
    return(simplified)
    
  }
  
  for(i in 1:repetitions) { 
    
    inputData <- inputData %>% function_internal_clusterGeo(maxIterations)
    
  }
  
  return(inputData)
  
}




function_simpleMap <- function(input) { 
  
  basemap <- 
    leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, options = providerTileOptions(opacity = 0.5))
  
  POINT <- input %>% filter(str_detect(st_geometry_type(geometry), "POINT")) 
  LINE <- input %>% filter(str_detect(st_geometry_type(geometry), "LINE")) 
  POLYGON <- input %>% filter(str_detect(st_geometry_type(geometry), "POLYGON")) 
  
  map <- basemap
  
  if(!is.null(POINT) & nrow(POINT) > 0 ) { map <- map %>% addCircles(data = POINT) }
  if(!is.null(LINE) & nrow(LINE) > 0 ) { map <- map %>% addPolylines(data = LINE) }
  if(!is.null(POLYGON) & nrow(POLYGON) > 0 ) { map <- map %>% addPolygons(data = POLYGON) }
  
  return(map)
  
}





function_simplifyPoints <- function(inputData, distance = 500, repetitions = 1, maxIterations = 500) {
  
  function_internal_simplifyPoints <- function(input, distance, maxIterations) {
    
    ll <- st_is_within_distance(input, input, units::set_units(distance, "m")); ll
    le <- lengths(ll); le
    ll <- ll[order(le, decreasing = TRUE)]; ll
    simplified <- list()
    input2 <- input %>% st_as_sf() %>% st_make_valid()
    
    for(i in 1:maxIterations) {
      
      check <- input %>% slice(ll[[1]]); check
      
      # Simplify or join
      if(nrow(check) > 1 & sum(str_detect(check$type, "POLYGON")) >= 1 & sum(str_detect(check$type, "POINT")) >= 1) { 
        
        simplified[[i]] <- input %>% slice(ll[[1]]) %>% filter(!str_detect(type, "POINT")) %>% summarise() %>% mutate(desc = "Public transport; Rail station")
        
      } else {
        
        if(sum(str_detect(check$type, "POINT")) == nrow(check)) { 
          simplified[[i]] <- input %>% slice(ll[[1]]) %>% mutate(desc = "Public transport; Rail station")
        } else {
          simplified[[i]] <- input %>% slice(ll[[1]]) %>% summarise() %>% mutate(desc = "Public transport; Rail station")
        }
        
      }
      
      # Save indices of removals
      remove <- which(sapply(1:length(ll), function(x) { ll[[x]] %in% ll[[1]] %>% sum }) > 0); remove
      
      # Update input 
      input2 <- input2 %>% slice(-ll[[1]]); input2
      
      # Update ll
      ll <- ll[-remove]; ll
      if(length(ll) == 0) break
      
    }
    
    # simplified #
    simplified <- simplified %>% mapedit:::combine_list_of_sf()
    
    return(simplified)
    
  }
  
  for(i in 1:repetitions) { 
    
    inputData <- 
      inputData %>% function_internal_simplifyPoints(
        distance = distance, 
        maxIterations = maxIterations)
    
  }
  
  return(inputData)
  
}
