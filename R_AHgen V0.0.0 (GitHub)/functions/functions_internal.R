function_read_abstractionHierarchy <- 
  
  function(filename = "USAH.csv", 
           nodeInfo = c("nodeName", "layer", "layerName", "filterCoverage")) { 
  
  colNames <- read_csv(filename, col_types = cols()) %>% names
  nCols <- length(colNames)
  colLoc <- which(colNames %in% nodeInfo) 
  
  colClass <- rep("n", nCols) # Vector of classes, "n" numeric
  colClass[colLoc] <- "c" # Replace nodeInfo, "c" character
  colClass <- paste0(colClass, collapse = "") # Collapse vector to string
  
  output <- dtAH <- read_csv(filename, col_types = colClass)
  attr(output, "nodeInfo") <- nodeInfo
  return(output)
  
}


function_abstractionHierarchy_to_nodeInfo <- function(abstractionHierarchy) { 
  
  abstractionHierarchy %>% select(attr(abstractionHierarchy, "nodeInfo")) 
  
}


function_nodeList <- function(nodeInfo) { 
  
  output <- 
    nodeInfo %>% 
    split(., .$layer) %>%
    modify(. %>% select(nodeName, filterCoverage)) %>%
    setNames(paste0("layer_", c("1_FP", "2_VPM", "3_GF", "4_ORP", "5_PO")))
  output[1:4] <- output[1:4] %>% modify(. %>% select(-filterCoverage) %>% unlist %>% as.vector)
  output[[5]] <- output[[5]] %>% split(., .$filterCoverage) %>% modify(. %>% select(-filterCoverage) %>% unlist %>% as.vector)
  
  return(output)
  
}


function_abstractionHierarchy_to_igraph <- function(abstractionHierarchy) { 
  
  dtAH <- abstractionHierarchy %>% select(-attr(abstractionHierarchy, "nodeInfo"))
  
  dtAH %>% as.matrix %>% graph.adjacency(mode = "undirected", weighted = TRUE)
  
}


function_igraphlist <- function(nodeInfo, igraph) {
  
  dtInfo <- nodeInfo
  dtEdgelist <- igraph %>% genEdgelist
  
  data <- dtInfo %>% 
    select(from = nodeName, layer, layerName) %>% 
    inner_join(dtEdgelist) %>%
    split(., .$layer) %>%
    setNames(c("links_1_FP_to_VPM", "links_2_VPM_to_GF", "links_3_GF_to_ORP", "links_4_ORP_to_PO")) 
  
  data %>%
    modify(. %>%
             select(-layer, -layerName) %>%
             as.matrix %>%
             graph.data.frame(directed = FALSE))
  
}


# Is function_abstractionHierarchy_to_edgeListTibble unused?
function_abstractionHierarchy_to_edgeListTibble <- function(abstractionHierarchy) {
  
  abstractionHierarchy %>% function_adjMat %>% get.data.frame %>% as_tibble()
  
  }
    

function_internal_removeNodes <- function(igraph, removeNodes) { igraph %>% delete_vertices(removeNodes) }


function_internal_isolated <- function(igraph_nodesRemoved) { 
  
  dv <- which(degree(igraph_nodesRemoved) == 0) %>% names %>% as.vector
  dv <- dv[order(dv)]
  
  if(length(dv) == 0) { dv <- NULL }
  
  return(dv)
  
}

function_internal_brokenLinks <- function(igraph, removeNodes) {
  
  dl <- igraph %>% adjacent_vertices(removeNodes)
  
  dl <-
    tryCatch(
      lapply(1:length(dl), function(x) {
        
        from = names(dl)[[x]]
        to = dl[[x]] %>% names
        tibble(from = from, to = to)
        
      }) %>% 
        bind_rows() %>% 
        arrange(from, to), error = function(e) NULL)
  
  if(!is.null(dl)) { if(nrow(dl) == 0) { dl <- NULL } }
  
  return(dl)
  
}

function_removeNodes_withSummary <- function(igraph, removeNodes) { 
  
  di_removedNodes <- igraph %>% function_internal_removeNodes(removeNodes)
  dv_isolated <- di_removedNodes %>% function_internal_isolated 
  dt_brokenLinks <- igraph %>% function_internal_brokenLinks(removeNodes)
  
  list(igraph_removedNodes = di_removedNodes, isolated = dv_isolated, brokenLinks = dt_brokenLinks)
  
}

function_removeNodes_byLayer <- function(igraph, linkName, removeNodes, max = 10) {
  
  igraph <- igraph[[linkName]]
  removeNodes_input <- removeNodes
  
  nodeNames_byLevel <- 
    igraph %>% 
    get.data.frame %>% 
    as_tibble() %>%
    select(upper = from, lower = to) %>%
    gather("level", "nodeName") %>% 
    unique
  
  dl <- list()
  dvNames <- paste0("round_", 1:max)
  
  dl[[dvNames[[1]]]] <- function_removeNodes_withSummary(igraph, removeNodes)
  
  if(!is.null(dl[[dvNames[[1]]]]$isolated)) {
    
    for(i in 2:max) { 
      
      tmp <- dl[[dvNames[[i-1]]]]; tmpigraph = tmp$igraph_removedNodes; tmpremoveNodes = tmp$isolated
      
      dl[[dvNames[[i]]]] <- function_removeNodes_withSummary(tmpigraph, tmpremoveNodes) 
      
      if(is.null(dl[[dvNames[[i]]]]$isolated)) { break }
      
    }
    
  }
  
  dvNames_round <- dl %>% names; dvNames_round
  dl <- 
    lapply(1:length(dl), function(x) { 
      
      dr <- dl[[x]] %>% rmNullList 
      
      lapply(dr, function(y) { 
        
        attr(y, "round") <- dvNames_round[[x]]
        return(y)
        
      })
      
    })
  
  dlFlat <- dl %>% purrr::flatten(); dlFlat
  
  dv_igraph <- which(names(dlFlat) == "igraph_removedNodes") %>% max
  dv_isolated <- which(names(dlFlat) == "isolated")
  dv_brokenLinks <- which(names(dlFlat) == "brokenLinks")
  
  di_igraphOutput <- dlFlat[[dv_igraph]]
  
  dt_nodesRemoved <- 
    
    lapply(dlFlat[dv_isolated], function(x) { 
      
      tibble(round = attr(x, "round"), nodeName = x)
      
    }) %>% 
    bind_rows() %>%
    
    rbind(tibble(round = "round_0", nodeName = removeNodes_input)) %>%
    
    arrange(round, nodeName) %>%
    
    mutate(round = str_replace(round, "round_", ""),
           round = round %>% as.numeric)
  
  dt_nodesRemoved <- 
    dt_nodesRemoved %>% 
    left_join(nodeNames_byLevel, by = "nodeName") %>%
    select(level, everything())
  
  dt_brokenLinks <- 
    
    lapply(dlFlat[dv_brokenLinks], function(x) { 
      
      x %>% mutate(round = attr(x, "round")) %>% select(round, everything())
      
    }) %>%
    bind_rows %>%
    
    arrange(round, from, to) %>%
    
    mutate(round = str_replace(round, "round_", ""),
           round = round %>% as.numeric,
           round = round-1)
  
  list(igraph = di_igraphOutput, removedNodes = dt_nodesRemoved, brokenLinks = dt_brokenLinks)
  
}

function_weightsHazard <- function(objectCount, baselineScenario = "baseline", keyFilename = "OSMAHkey_CS.csv") { 
  
  if(str_detect(objectCount, "RDS")) { output <- readRDS(objectCount) }
  if(str_detect(objectCount, "csv")) { output <- read_csv(objectCount, col_types = cols()) }
  if(!str_detect(objectCount, "csv") & !str_detect(objectCount, "RDS")) { output <- objectCount }
  
  output <- output %>% select(-contains("desc1"), -contains("desc2"))
  
  dk <- 
    read_csv(keyFilename, col_types = cols()) %>% 
    mutate(nodeName = coalesce(ahObject1, ahObject2)) %>%
    select(contains("desc"), nodeName) %>%
    setNames(c("desc", "nodeName"))
  
  output <- output %>% left_join(dk, by = "desc") %>% select(-desc) %>% select(nodeName, everything())
  
  posVec <- which(!names(output) %in% c("nodeName", baselineScenario))
  
  output %>% 
    gather("scenario", "value", posVec) %>%
    setNames(c("nodeName", baselineScenario, "scenario", "value")) %>%
    mutate(functional = baseline - value,
           weightHazard =  functional / baseline) %>%
    split(., .$scenario) %>%
    modify(. %>% select(nodeName, weightHazard) %>% arrange(nodeName) %>% na.omit)
  
}

function_removeNodes_hazard <- function(igraph_weightsHazard) {
  
  rmEmptyList <- function(input) { input %>% Filter(function(x) dim(x)[1] > 0, .) }
  
  dl_removeNodes <- 
    igraph_weightsHazard %>% 
    modify(. %>% filter(weightHazard == 0))
  dl_removeNodes <- tryCatch(dl_removeNodes %>% 
                               modify(. %>% select(nodeName) %>% unlist %>% as.vector), 
                                      error = function(e) NULL)
  dl_removeNodes <- lapply(dl_removeNodes, function(x) { if(length(x) == 0) { x = NULL }; return(x) })
  
  dl_weightNodes <- 
    igraph_weightsHazard %>% 
    modify(. %>% filter(weightHazard > 0))
  dl_weightNodes <- lapply(dl_weightNodes, function(x) { 
    if(nrow(x) == 0) { x = NULL } else { x = x %>% rename(weight = weightHazard) }
    return(x) 
    })
  
  list(removeNodes = dl_removeNodes, weightNodes = dl_weightNodes)
  
}

function_format_results <- function(result, resultName) { 
  
  result %>%
    as.data.frame() %>% 
    rownames_to_column() %>%
    as_tibble() %>%
    setNames(c("nodeName", resultName))
  
}


function_results_byNode <- function(igraph, resultName = c("Unweighted vertex betweenness centrality", "UWVBC",
                                                  "Weighted vertex betweenness centrality", "WVBC",
                                                  "Authority", "Authority score", "Hub", "Hub score",
                                                  "Closeness",
                                                  "Eigen", "Eigen centrality",
                                                  "Subgraph centrality", "Subgraph",
                                                  "Burt's constraint", "Burts constraint", "Burt",
                                                  "Eccentricity", "Diversity")) {
  
  if(resultName %in% c("Unweighted vertex betweenness centrality", "UWVBC")) {
    
    # It can also calculated a weighted in igraph; different result to tnet; why do we use the tnet one for weighted?
    output <- igraph %>% betweenness(weights = NA) %>% function_format_results("UWVBC")
    
  }
  
  if(resultName %in% c("Weighted vertex betweenness centrality", "WVBC")) {
    
    tnetInput <- igraph %>% get.adjacency() %>% as.matrix()
    
    output <- 
      tnetInput %>%
      symmetrise_w() %>%
      betweenness_w(directed = NULL, alpha = 0.5) %>%
      as_tibble() %>%
      mutate(nodeName = rownames(tnetInput)) %>%
      select(nodeName, WVBC = betweenness)
    
  }
  
  if(resultName %in% c("Authority", "Authority score", "Hub score", "Hub")) {
    
    # Same as hub score for undirected networks
    output <- (igraph %>% authority_score)$vector %>% function_format_results("authorityScore")
    
  }
  
  # Weighted
  if(resultName %in% c("Closeness")) { output <- igraph %>% closeness %>% function_format_results("closeness") }
  
  # Weighted
  if(resultName %in% c("Eigen", "Eigen centrality")) { output <- (igraph %>% eigen_centrality)$vector %>% function_format_results("eigenCentrality") }
  
  if(resultName %in% c("Subgraph centrality", "Subgraph")) { output <- igraph %>% subgraph_centrality %>% function_format_results("subgraphCentrality") }
  
  if(resultName %in% c("Burt's constraint", "Burts constraint", "Burt")) { output <- igraph %>% constraint %>% function_format_results("burtsConstraint") }
  
  if(resultName %in% c("Eccentricity")) { output <- igraph %>% eccentricity %>% function_format_results("eccentricity") }
  
  if(resultName %in% c("Diversity")) { output <- igraph %>% diversity %>% function_format_results("diversity") }
  
  return(output)
  
}

function_flattenList_addNames <- function(listObject) {
  
  vecNames <- names(listObject)
  
  lapply(1:length(listObject), function(x) { 
    listObject[[x]] %>% 
      mutate(listname = vecNames[[x]]) %>%
      select(version, listname, everything())
  }) %>%
    bind_rows()
  
}

function_allResults_temporary <- function(generic, indicators, hazards) {
  
  pbo <- pboptions(type = "none")
  on.exit(pboptions(pbo), add = TRUE)
  
  output1 <- 
    
    list(generic = generic %>% 
           transform_igraphList_igraph %>% 
           results_nodeWise %>%
           mutate(version = "AH_generic") %>%
           select(version, everything()),
         
         indicators = indicators %>% 
           transform_igraphList_igraph %>% 
           results_nodeWise %>%
           mutate(version = "AH_indicators") %>%
           select(version, everything()))
  
  tmp = lapply(hazards, function(x) { 
    
    pbo <- pboptions(type = "none")
    on.exit(pboptions(pbo), add = TRUE)
    
    x %>%
      transform_igraphList_igraph %>%
      results_nodeWise %>%
      mutate(version = "AH_hazards") %>%
      select(version, everything())
    
  })
  
  output1[["hazards"]] <- tmp %>% function_flattenList_addNames %>% rename(scenario = listname)
  
  output2 <- list(generic = generic %>% results_nodeWise_byLevel() %>% mutate(version = "AH_generic") %>% select(version, everything()),
                  indicators = indicators %>% results_nodeWise_byLevel() %>% mutate(version = "AH_indicators") %>% select(version, everything()))
  
  
  tmp <- lapply(hazards, function(x) { 
    
    pbo <- pboptions(type = "none")
    on.exit(pboptions(pbo), add = TRUE)
    
    x %>% results_nodeWise_byLevel %>%
      mutate(version = "AH_hazards") %>%
      select(version, everything())
    
  })
  
  output2[["hazards"]] <- tmp %>% function_flattenList_addNames %>% rename(scenario = listname)
  
  list(networkSummary = output1, nodeWise = output2)
  
}

function_exportExcelResults_temporary <- function(inputList, filename) {
  
  require(openxlsx)
  
  wb <- openxlsx::createWorkbook()
  
  input = inputList
  
  sheetnames <- names(inputList)
  
  Map(function(data, nameofsheet){     
    
    addWorksheet(wb, nameofsheet)
    writeDataTable(wb, nameofsheet, data, rowNames = FALSE)
    
  }, input, sheetnames)
  
  openxlsx::saveWorkbook(wb, file = filename)
  
}
