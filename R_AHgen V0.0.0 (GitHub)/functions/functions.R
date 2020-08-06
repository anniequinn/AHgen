genericAbstractionHierarchy <- 
  function(filename = "USAH.csv", 
           nodeInfo = c("nodeName", "layer", "layerName", "filterCoverage",
                        "definition", "UKspecific", 
                        "subnetwork_infrastructure_communications",
                        "subnetwork_infrastructure_electric",	
                        "subnetwork_food", "subnetwork_infrastructure_gas",
                        "subnetwork_medical", "subnetwork_lawOrder", 
                        "subnetwork_nature", "subnetwork_religion",	
                        "subnetwork_infrastructure_transport",      
                        "subnetwork_infrastructure_transport_air", 
                        "subnetwork_infrastructure_transport_active",
                        "subnetwork_infrastructure_transport_rail",
                        "subnetwork_infrastructure_transport_roadPrivate",
                        "subnetwork_infrastructure_transport_roadPublic",
                        "subnetwork_infrastructure_waste", 
                        "subnetwork_infrastructure_water",
                        "subnetwork_infrastructure_water_black",
                        "subnetwork_infrastructure_water_blue",
                        "subnetwork_infrastructure_water_green",
                        "subnetwork_infrastructure_water_grey",       
                        "hazard_biological", "hazard_cyberAttack", 
                        "hazard_drought", "hazard_earthquake", "hazard_fire", 
                        "hazard_flood", "hazard_landslide", "hazard_snow", 
                        "hazard_wind")) {
    
    output <- list()
    
    output$abstractionHierarchy <- function_read_abstractionHierarchy(filename, nodeInfo)
    output$nodeInfo <- output$abstractionHierarchy %>% function_abstractionHierarchy_to_nodeInfo
    output$nodeList <- output$nodeInfo %>% function_nodeList 
    output$igraph <- output$abstractionHierarchy %>% function_abstractionHierarchy_to_igraph
    output$igraphList <- function_igraphlist(output$nodeInfo, output$igraph)
    
    return(output) 
    
  }


genEdgelist <- function(igraph) { igraph %>% get.data.frame %>% as_tibble }


cutHazard <- function(geotaggedDatabase, fileString = "shapefilesHazard") {
  
  # INTERNAL FUNCTIONS ------------------------------------------------------
  cutHazard_individual <- function(geotaggedDatabase, filename_hazardShapefile) { 
    
    rmCols <- function(input) { input %>% Filter(function(x) !all(is.na(x)), .) }
    rmNullList <- function(input) { input %>% Filter(Negate(is.null), .) }
    rmEmptyList <- function(input) { input %>% Filter(function(x) dim(x)[1] > 0, .) }
    
    
    timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
    message(paste0(timeStamp, "Step one of four"))
    
    
    hazardShapefile <- st_read(filename_hazardShapefile, quiet = TRUE) %>% st_transform(crs = 4326)
    
    dl <- 
      geotaggedDatabase %>% 
      mutate(element = st_geometry_type(geometry)) %>%
      split(., .$element) %>%
      modify(. %>% as_tibble)
    namesdl <- names(dl)
    
    
    timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
    message(paste0(timeStamp, "Step two of four"))
    
    
    dl <- pblapply(dl, function(x) { 
      
      suppressMessages(suppressWarnings( 
        x %>% 
          as_tibble %>% 
          st_as_sf %>% 
          st_join(hazardShapefile, left = FALSE)
      )) 
      
    })
    dl <- dl %>% setNames(namesdl) %>% rmEmptyList()
    
    
    timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
    message(paste0(timeStamp, "Step three of four"))
    
    
    dl2 = pblapply(dl, function(x) { 
      
      suppressMessages(suppressWarnings(
        x %>% 
          mutate(contains = paste0("dl_", st_contains(hazardShapefile, x, sparse = FALSE) %>% unlist %>% as.vector)) %>% 
          split(., .$contains)
      ))
      
    }) %>% purrr::flatten()
    
    
    timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
    message(paste0(timeStamp, "Step four of four"))
    
    
    st3False <- dl2[ which(str_detect(names(dl2), "FALSE")) ]
    st3True <- dl2[ which(str_detect(names(dl2), "TRUE")) ]
    
    st3False <- pblapply(st3False, function(x) { 
      
      suppressMessages(suppressWarnings( 
        
        x %>% st_make_valid %>% st_intersection(hazardShapefile) 
        
      )) 
      
    })
    
    
    timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
    message(paste0(timeStamp, "Complete, preparing output"))
    
    
    output <- c(st3False, st3True)
    output <- output %>% modify(. %>% select(-contains)) %>% unname 
    
    output <-
      output %>% 
      modify(. %>% as_tibble %>% mutate(geometry = st_as_text(geometry))) %>%
      bind_rows() %>%
      select(geometry, desc, desc1, desc2) %>%
      mutate(geometry = st_as_sfc(geometry)) %>%
      st_as_sf(crs = 4326)
    
    return(output)
    
  }
  
  cutHazard_byDirectory <- function(geotaggedDatabase, directory) { 
    
    files <- list.files(directory, pattern = ".shp", full.names = TRUE)
    if(length(files) == 0) { stop(paste0("No files found in directory /", directory)) }
    
    output <- 
      
      pblapply(1:length(files), function(x) { 
        
        pbo <- pboptions(type = "none")
        on.exit(pboptions(pbo), add = TRUE)
        
        output <- suppressMessages( cutHazard_individual(geotaggedDatabase, filename_hazardShapefile = files[[x]]) )
        output %>% mutate(scenario = files[[x]])
        
      })
    
    output <- output %>% modify(. %>% select(scenario, everything()))
    output <- output %>% setNames(files)
    
    return(output)
    
  }
  # -------------------------------------------------------------------------
  
  if(str_detect(fileString, ".shp")) { 
    
    output <- list()
    output[[fileString]] <- cutHazard_individual(geotaggedDatabase = dlGeo$geotaggedDatabase, 
                                                 filename_hazardShapefile = fileString) %>% 
      mutate(scenario = fileString)
    
  } else { 
    
    output <- cutHazard_byDirectory(dlGeo$geotaggedDatabase, 
                                    directory = fileString)
    
  }
  
  return(output)
  
}

nameScenarios <- function(hazardList, vecNames) {
  
  names1 = names(hazardList)
  names2 = vecNames 

  names3 <- factor(names1, levels = names1, labels = names2, ordered = FALSE)
  
  output <- 
    hazardList %>% 
    modify(. %>% rename(scenarioOld = scenario) %>% 
             inner_join(tibble(scenarioOld = names1, scenario = names3), 
                        by = "scenarioOld") %>%
             select(-scenarioOld) %>%
             select(scenario, geometry:desc2)) %>%
    setNames(names2)
  
  return(output)
  
}

objectCount <- function(baseline = NULL, hazardList = NULL) { 
  
function_objectCount <- function(dt) { 
    
    dl = 
      dt %>%
      as_tibble %>%
      mutate(type = st_geometry_type(geometry)) %>%
      split(., .$type, drop = TRUE) %>%
      modify(. %>% select(type, desc, desc1, desc2, geometry))
    
    counts <- 
      lapply(1:length(dl), function(x) { 
        namesDl <- names(dl)
        
        if(str_detect(namesDl[[x]], "POINT")) { 
          output = dl[[x]] %>% select(-geometry) %>% group_by_all() %>% summarise(n = n()) %>% ungroup %>% mutate(unit = "count") 
        }
        
        if(str_detect(namesDl[[x]], "STRING")) {
          output = 
            dl[[x]] %>% 
            mutate(length = st_length(st_sfc(geometry))) %>% 
            select(-geometry) %>% 
            group_by(type, desc, desc1, desc2) %>% 
            summarise(n = sum(length) %>% as.numeric) %>%
            ungroup %>%
            mutate(unit = "m")
        }
        
        if(str_detect(namesDl[[x]], "POLYGON")) {
          output = 
            dl[[x]] %>% 
            mutate(area = st_area(st_sfc(geometry))) %>%
            select(-geometry) %>% 
            group_by(type, desc, desc1, desc2) %>% 
            summarise(n = sum(area) %>% as.numeric) %>%
            ungroup %>%
            mutate(unit = "sqm")
        }
        
        output %>% select(-type)
        
      }) %>% 
      bind_rows() %>%
      arrange(desc)
    return(counts)
  }
  
  output <- list()
  
  if(!is.null(baseline)) { output[["baseline"]] <- baseline %>% function_objectCount %>% rename(baseline = n) %>% 
    select(desc, desc1, desc2, unit, everything()) %>%
    arrange(desc, desc1, desc2, unit)
  }
  
  if(!is.null(hazardList)) {
    
    tmp <- 
      lapply(1:length(hazardList), function(x) { 
        
        scenarioName = names(hazardList)[[x]]
        function_objectCount(hazardList[[x]]) %>% setNames(c("desc", "desc1", "desc2", scenarioName, "unit")) %>%
          arrange(desc, desc1, desc2, unit)
        
      })
    
    output[["hazard"]] <- tmp %>% reduce(full_join, by = c("desc", "desc1", "desc2", "unit")) %>% select(desc, desc1, desc2, unit, everything()) %>%
      arrange(desc, desc1, desc2, unit)
    
  }
  
  if(!is.null(baseline) & !is.null(hazardList)) {
    
    output[[1]] <- output %>% reduce(inner_join, by = c("desc", "desc1", "desc2", "unit")) %>% select(desc, desc1, desc2, unit, everything())
    
  }
  
  output[[1]]
  
}













function_igraphtoedgelist <- function(igraph, weightName = "weight") {
  
  output <- igraph %>% get.data.frame %>% as_tibble()
  output[[weightName]] <- as.numeric(output[[weightName]])
  return(output)
  
}










rmNullList <- function(input) { input %>% Filter(Negate(is.null), .) }






function_locationWeightings <- function(filenameObjectCount, keyFilename = "OSM_AH_key.RDS") {
  
  dtLocation_PO <-  # formerly dtValues
    readRDS(keyFilename) %>% 
    inner_join(read_csv(filenameObjectCount, col_types = cols()) %>% 
                 select(descTerm, weightHazard), 
               by = "descTerm") %>% 
    na.omit() %>% 
    group_by(nodeName) %>% 
    summarise(weightHazard = mean(weightHazard))
  
  list(locationNodes = dtLocation_PO$nodeName,
       hazardRemoves = (dtLocation_PO %>% filter(weightHazard == 0))$nodeName,
       hazardRetains = dtLocation_PO %>% filter(weightHazard >0))
}

function_filenameExport <- function(location, details, extension) {
  
  file = paste0("AHgen_", location, "_", details, "_", 
                format(Sys.time(), "%Y%m%d-%H%M%S"), 
                ".", extension)
  
  file = paste0("outputs", "/", file) # Add on folder directory
  
  return(file)
  
}

export <- function(exportObject, exportType, location = "generic") { 
  
  filename1 = function_filenameExport(location = location, details = exportType, "csv")
  filename1 = paste0(filename1)
  filename2 = function_filenameExport(location = location, details = exportType, "RDS")
  filename2 = paste0(filename2)
  
  check1 = tryCatch({exportObject %>% write_csv(filename1); "Complete"}, error = function(e) NULL)
  check2 = tryCatch({exportObject %>% saveRDS(filename2); "Complete"}, error = function(e) NULL)
  
  if(is.null(check1) & !is.null(check2)) { 
    printout <- paste0("File saved as: ", filename2); message(printout)
  }
  
  if(!is.null(check1) & is.null(check2)) { 
    printout <- paste0("File saved as: ", filename1); message(printout)
  }
  
  if(is.null(check1) & is.null(check2)) { 
    printout <- paste0("Export error; please check inputs"); message(printout)
  }
  
  if(!is.null(check1) & !is.null(check2)) { 
    printout <- paste0("File(s) saved as: ", paste0("\n\t", filename1, "\n\t", filename2)); message(printout)
  }
  
}





weightsIndicators <- function(locationName,
                              indicatorsFilename = "indicators_CS.csv",
                              colNames = c("indName", "indDesc", "indStatus", "sourceDesc", "sourceLink", 
                                           "lastAccessed", "otherComments", "nodeVPM", "nodeGF"),
                              type = "igraph") {
  
  colNames <- c(colNames, locationName)
  nColNames <- colNames %>% length
  dt <- read_csv(indicatorsFilename, col_types = cols()) %>% select(colNames) %>% arrange(nodeVPM, nodeGF) 
  dv <- dt %>% select(locationName) %>% unlist %>% as.vector
  dt <- dt %>% slice(-which(is.na(dv)))
  
  dt <-
    dt %>%
    setNames(c(colNames[-nColNames], "location")) %>%
    group_by(nodeVPM, nodeGF, indStatus) %>%
    summarise(value = mean(location)) %>% 
    ungroup %>%
    mutate(indStatus = factor(indStatus, levels = c("Preferred", "Alternate")))
  
  if(dt$indStatus %>% unique %>% length > 1) {
    
    dt <- 
      dt %>% 
      spread(indStatus, value) %>% 
      mutate(value = coalesce(Preferred, Alternate)) %>%
      select(-Alternate, -Preferred)
    
  }
  
  dt <- dt %>% select(-contains("indStatus")) %>% rename(weightLocation = value, from = nodeVPM, to = nodeGF)
  dt <- dt %>% graph.data.frame(directed = FALSE)
  
  if(type == "igraph") { output <- dt }
  if(type == "tibble") { output <- dt %>% get.data.frame %>% as_tibble() }
  
  return(output)
  
}


weightLinks <- function(linkName = "links_2_VPM_to_GF", igraphlist, weights) {
  
  output <- union(igraphlist[[linkName]], weights)
  edge.attributes(output)$weight <- coalesce(edge.attributes(output)$weightLocation, rep(1, E(output) %>% length))
  output <- remove.edge.attribute(output, "weightLocation")
  
  igraphlist[[linkName]] <- output
  return(igraphlist)
  
}


checkWeightLinks <- function(igraphGeneric, igraphWeighted, linkName = NULL) { 
  
  if(!is.null(linkName)) { igraphGeneric <- igraphGeneric[[linkName]]; igraphWeighted <- igraphWeighted[[linkName]] }
  
  inner_join(igraphGeneric %>% get.data.frame %>% as_tibble() %>% rename(generic = weight), 
             igraphWeighted %>% get.data.frame %>% as_tibble() %>% rename(weighted = weight),
             
             by = c("from", "to"))
  
}


baselinePhysicalObjects_fromHazard <- function(igraph) { 
  
  if(is_tibble(igraph)) { igraph <- list(igraph) }
  
  lapply(igraph, function(x) { x$nodeName }) %>% 
    purrr::flatten() %>% 
    unlist %>% 
    as.vector %>% 
    unique
  
}





baselinePhysicalObjects_fromFile <- function(objectCountFilename, keyFilename = "OSMAHkey_CS.csv") { 
  
  if(str_detect(objectCountFilename, "RDS")) { input <- readRDS(objectCountFilename) %>% na.omit }
  if(str_detect(objectCountFilename, "csv")) { input <- read_csv(objectCountFilename, col_types = cols()) %>% na.omit }
  if(!str_detect(objectCountFilename, "csv") & !str_detect(objectCountFilename, "RDS")) { 
    stop("objectCountFilename must have an RDS or csv extension") 
    }
  
  dt_OSMtidy <- input[,1] %>% setNames(c("desc"))
  
  dk <- 
    read_csv(keyFilename, col_types = cols()) %>% 
    mutate(nodeName = coalesce(ahObject1, ahObject2)) %>%
    select(desc = descTerm, nodeName)
  
  dt_OSMtidy %>% left_join(dk, by = "desc") %>% select(nodeName) %>% unlist %>% as.vector
  
}


baselinePhysicalObjects_fromFile_revised <- function(objectCountFilename, keyFilename = "OSM_AH_key.csv") { 
  
  if(str_detect(objectCountFilename, "RDS")) { 
    input <- readRDS(objectCountFilename) 
    input <- input$desc %>% as_tibble %>% na.omit # renamed 'input$geotaggedDatabase' to 'input$desc', deleted '-geometry'
  }
  if(str_detect(objectCountFilename, "csv")) { input <- read_csv(objectCountFilename, col_types = cols()) %>% na.omit }
  if(!str_detect(objectCountFilename, "csv") & !str_detect(objectCountFilename, "RDS")) { 
    stop("objectCountFilename must have an RDS or csv extension") 
  }
  
  dt_OSMtidy <- input[,1] %>% setNames(c("desc"))
  
  dk <- 
    read_csv(keyFilename, col_types = cols()) %>% 
    select(desc = descTerm, nodeName = ahObject1)
  
  dt_OSMtidy %>% left_join(dk, by = "desc") %>% select(nodeName) %>% unique() %>% unlist %>% as.vector
  
}


removeNodes <- function(igraph, removeNodes) {
  
  l4input <- list(linkName = "links_4_ORP_to_PO", 
                  removeNodes = removeNodes)
  
  l4 <- tryCatch( function_removeNodes_byLayer(igraph, l4input$linkName, l4input$removeNodes), 
                  error = function(e) list(igraph = igraph[[l4input$linkName]]) )
  
  
  l3input <- list(linkName = "links_3_GF_to_ORP",
                  removeNodes = tryCatch( (l4$removedNodes %>% filter(level == "upper"))$nodeName %>% unique, 
                                          error = function(e) NULL) )
  
  l3 <- tryCatch( function_removeNodes_byLayer(igraph, l3input$linkName, l3input$removeNodes), 
                  error = function(e) list(igraph = igraph[[l3input$linkName]]) )
  
  l2input <- list(linkName = "links_2_VPM_to_GF",
                  removeNodes = tryCatch( (l3$removedNodes %>% filter(level == "upper"))$nodeName %>% unique, 
                                          error = function(e) NULL) )
  
  l2 <- tryCatch( function_removeNodes_byLayer(igraph, l2input$linkName, l2input$removeNodes), 
                  error = function(e) list(igraph = igraph[[l2input$linkName]]) )
  
  l1input <- list(linkName = "links_1_FP_to_VPM",
                  removeNodes = tryCatch( (l2$removedNodes %>% filter(level == "upper"))$nodeName %>% unique, 
                                          error = function(e) NULL) )
  l1 <- tryCatch( function_removeNodes_byLayer(igraph, l1input$linkName, l1input$removeNodes), 
                  error = function(e) list(igraph = igraph[[l1input$linkName]]) )
  
  output <- list()
  dl <- list(l1, l2, l3, l4)
  dlInput <- list(l1input, l2input, l3input, l4input)
  
  output[["igraph"]] <- 
    list(l1$igraph, l2$igraph, l3$igraph, l4$igraph) %>%
    setNames(c(l1input$linkName, l2input$linkName, l3input$linkName, l4input$linkName))
  
  output[["removedNodes"]] <- 
    lapply(1:length(dl), function(x) { 
      tryCatch(
        dl[[x]][["removedNodes"]] %>% 
          mutate(level = dlInput[[x]]$linkName), 
        error = function(e) NULL) }) %>% 
    bind_rows() %>%
    select(level, everything()) %>%
    arrange(desc(level), round, nodeName)
  
  output[["brokenLinks"]] <- 
    lapply(1:length(dl), function(x) { 
      tryCatch(
        dl[[x]][["brokenLinks"]] %>% 
          mutate(level = dlInput[[x]]$linkName), 
        error = function(e) NULL) }) %>% 
    bind_rows() %>%
    select(level, everything()) %>%
    arrange(desc(level), round, from, to)
  
  return(output)
  
}


weightsHazard <- function(objectCount, baselineScenario = "baseline", keyFilename = "OSMAHkey_CS.csv") {
  
  objectCount %>% function_weightsHazard(baselineScenario, keyFilename) %>% function_removeNodes_hazard
  
}


removeNodes_hazard <- function(igraph, removeNodesList) {
  
  lapply(removeNodesList, function(x) { 
    
    tryCatch(removeNodes(igraph = igraph, removeNodes = x), error = function(e) list(igraph = igraph))

  })
  
}

weightHazard <- function(igraph_after_removeNodes_hazard, weightNodes) {
  
  lapply(1:length(igraph_after_removeNodes_hazard), function(x) { 
    
    input <- igraph_after_removeNodes_hazard[[x]]$igraph
    
    l4 <- input$links_4_ORP_to_PO %>% 
      get.data.frame %>% 
      as_tibble() %>%
      select(-weight) %>%
      full_join(weightNodes[[x]] %>% rename(to = nodeName), by = "to") %>%
      mutate(weight = coalesce(weight, rep(1, n()))) %>%
      na.omit() %>%
      igraph::graph.data.frame()
    
    input[["links_4_ORP_to_PO"]] <- l4
    
    return(input)
    
  }) %>%
    setNames(names(igraph_after_removeNodes_hazard))
  
}

transform_igraphList_igraph <- function(igraphList) { 
  
  lapply(1:length(igraphList), function(x) {
    
    igraphList[[x]] %>% 
      function_igraphtoedgelist() %>%
      mutate(level = names(igraphList)[[x]])
    
  }) %>%
    bind_rows() %>%
    graph_from_data_frame(directed = FALSE)
  
}

transform_igraph_abstractionHierarchy <- function(igraph) {
  igraph %>%
  get.adjacency() %>%
  as.matrix() %>%
  as.data.frame %>%
  rownames_to_column("nodeName") %>%
  as_tibble()
  
}

results_networkSummary <- function(igraph) {
  
  funs <- list(nNodes_total = vcount, nEdges_total = ecount, 
               assortativityDegree = assortativity_degree, diameter = diameter)
  sapply(funs, function(x) igraph %>% x) %>% t() %>% as_tibble
  
}

results_nodeWise <- function(igraph, resultsNames = NULL) { 
  
  if(is.null(resultsNames)) { 
    
    resultsNames <- c("Unweighted vertex betweenness centrality", "Weighted vertex betweenness centrality", 
                      "Authority score", "Closeness", "Eigen centrality", "Subgraph centrality", "Burt's constraint", 
                      "Eccentricity", "Diversity")
    
  }
  
  pblapply(resultsNames, function(x) igraph %>% function_results_byNode(x)) %>% reduce(., inner_join, by = "nodeName")
  
}

results_nodeWise_byLevel <- function(igraph, resultsNames = NULL, format = "tibble") {
  
  output <- 
    
    pblapply(1:length(igraph), function(x) {
      
      pbo <- pboptions(type = "none")
      on.exit(pboptions(pbo), add = TRUE)
      
      output <- igraph[[x]] %>% results_nodeWise(., resultsNames = resultsNames)
      if(format == "tibble") { 
        output <- output %>% 
          mutate(level = names(igraph)[[x]]) %>% 
          select(level, everything())
      }
      
      return(output)
      
    })
  
  if(format == "tibble") { output <- output %>% bind_rows %>% arrange(level, nodeName) }
  if(format == "list") { output <- output %>% setNames(names(igraph)) }
  
  return(output)
  
}

results_networkSummary_byLevel <- function(igraph) { 
  
  lapply(1:length(igraph), function(x) {
    
    pbo <- pboptions(type = "none")
    on.exit(pboptions(pbo), add = TRUE)
    
    igraph[[x]] %>% 
      results_networkSummary %>%
      mutate(level = names(igraph)[[x]])
    
  }) %>%
    bind_rows() %>%
    select(level, everything()) %>%
    arrange(level)
  
}
