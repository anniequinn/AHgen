# -------------------------------------------------------------------------
# AHgen.R
# Annie Visser-Quinn & Melissa Bedinger
#
# Created: 03-05-2020
# Last revised: 22-07-2020
# -------------------------------------------------------------------------

rm(list = ls()); cat("\014")

pacman::p_load(tidyverse, igraph, janitor, data.table, tnet, sf, pbapply, dplyr,
               viridis)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("functions/functions.R")
source("functions/functions_internal.R") 
# Useful to have access to the internal functions for troubleshooting
# But these can be embedded in the main functions internally at a later date


# -------------------------------------------------------------------------
# GENERIC ABSTRACTION HIERARCHY -------------------------------------------
# -------------------------------------------------------------------------
# GENERIC ABSTRACTION HIERARCHY
# Load in abstraction hierarchy
dl = genericAbstractionHierarchy(filename = "melissaInputs/USAH_final_20200712.csv")

# Manually add edgelist to the igraph object
dl$edgeList = dl$igraph %>% genEdgelist
dl$edgeList

# Export igraph object as RDS
dl %>% export(exportType = "abstractionHierarchy")

# Export edgelist
dl$edgeList %>% 
  as_tibble() %>% 
  mutate(location = "generic", scenario = "generic") %>%
  export(exportType = "edgelist", location = "generic")

# Extract generic igraph
di_generic <- dl$igraphList

# Pull nodeInfo into a dataframe to make for easier attaching to results further down
nodeInfo = dl$nodeInfo
# see if dl$nodeInfo could be used throughout

# Generic results need to be attached to RDS export so this can be reloaded easily in future
# Nodewise results for entire network (generic)
resultsBaseline_nodewise = 
  di_generic %>% 
  transform_igraphList_igraph %>% 
  results_nodeWise %>% 
  select(-WVBC) %>% # remove inaccurate WVBC results and fix in function later
  mutate(location = "generic", scenario = "generic", 
         rank_UWVBCoverall = rank(desc(UWVBC)),
         change_UWVBC = "NA", change_WVBC = "NA", 
         osmFrequency = "NA") %>% 
  inner_join(dl$nodeInfo, by = "nodeName") %>%
  group_by(layer) %>%
  mutate(rank_UWVBCbyLayer = rank(desc(UWVBC)))

# Separate function to add weighted vertex betweenness centrality (WVBC)
results_tnet <- function(igraph) {
  # Format AH into one igraph object
  tnetInput <- 
    igraph %>% 
    transform_igraphList_igraph()
  
  # Extract edgelist and symmetrise for undirected graphs
  net <- cbind(get.edgelist(tnetInput, names=FALSE), E(tnetInput)$weight)
  net <- net %>% symmetrise_w() %>% as_tibble()
  
  # Ensure that it conforms to the tnet standard
  net <- as.tnet(net, type = "weighted one-mode tnet")
  
  # Generate accurate nodeNames, as tnet only assigns identifier node numbers
  tnetNames <- 
    igraph %>% 
    transform_igraphList_igraph() %>%
    get.adjacency() %>% 
    as.matrix() %>% 
    rownames() %>% 
    as_tibble() %>%
    rename(nodeName = value)
  
  output <- 
    net %>% 
    betweenness_w(directed = NULL, alpha = 0.5) %>%
    as_tibble() %>%
    mutate(tnetNames) %>%
    rename(WVBC = betweenness) %>%
    select(-node)
}

# Add WVBC & WVBC rankings to nodewise results
resultsBaseline_nodewise = 
  di_generic %>% 
  results_tnet %>% 
  inner_join(resultsBaseline_nodewise) %>%
  mutate(rank_WVBCoverall = rank(desc(WVBC))) %>%
  group_by(layer) %>%
  mutate(rank_WVBCbyLayer = rank(desc(WVBC)))

# Organise rows and columns as desired
resultsBaseline_nodewise =
  resultsBaseline_nodewise %>%
  select(nodeName, sort(colnames(.))) %>%
  arrange(layer, desc(UWVBC))
  
# Results summary (generic)
resultsBaseline_summary = 
  di_generic %>% 
  transform_igraphList_igraph %>% 
  results_networkSummary %>% 
  mutate(nNodes_FP = table(nodeInfo$layer)["1"],
         nNodes_VPM = table(nodeInfo$layer)["2"],
         nNodes_GF = table(nodeInfo$layer)["3"],
         nNodes_ORP = table(nodeInfo$layer)["4"],
         nNodes_PO = table(nodeInfo$layer)["5"],
         nNodes_total_rem = "0",
         nNodes_FP_rem = "0",
         nNodes_VPM_rem = "0",
         nNodes_GF_rem = "0",
         nNodes_ORP_rem = "0",
         nNodes_PO_rem = "0",
         nEdges_level1 = ecount(di_generic$links_1_FP_to_VPM), 
         nEdges_level2 = ecount(di_generic$links_2_VPM_to_GF),
         nEdges_level3 = ecount(di_generic$links_3_GF_to_ORP),
         nEdges_level4 = ecount(di_generic$links_4_ORP_to_PO),
         nEdges_total_rem = "0",
         nEdges_level1_rem = "0",
         nEdges_level2_rem = "0",
         nEdges_level3_rem = "0",
         nEdges_level4_rem = "0",
         location = "generic", scenario = "generic")

# Export results (generic)
resultsBaseline_nodewise %>% export("nodewise", location = "generic")
resultsBaseline_summary %>% export("summary", location = "generic")

# Format generic UWVBC for comparisons
resultsBaseline_nodewise$UWVBC_generic = resultsBaseline_nodewise$UWVBC
resultsBaseline_nodewise$WVBC_generic = resultsBaseline_nodewise$WVBC


# -------------------------------------------------------------------------
# WEIGHT LINKS (INDICATORS) -----------------------------------------------
# -------------------------------------------------------------------------
# Specify location name to pull relevant indicators out of database
locationName <- "Edinburgh" ###################################################

# DATA
# type = "igraph" or "tibble"; default is "igraph"
di_weightsIndicators <- 
  weightsIndicators(locationName = locationName, 
                    indicatorsFilename = "melissaInputs/indicators_20200712.csv")

# WEIGHTING
di_weightedIndicators <- 
  weightLinks(linkName = "links_2_VPM_to_GF", 
              igraphlist = di_generic, 
              weights = di_weightsIndicators)

# Export weighted edgelist
di_weightedIndicators %>% transform_igraphList_igraph %>% as_data_frame() %>%
  mutate(location = locationName, scenario = "indicators") %>% 
  export(exportType = "edgelist_indicators", location = locationName)

# CHECK WEIGHTING (OPTIONAL)
# Specify the igraphs directly without checkWeightLinks linkName argument
checkWeightLinks(igraphGeneric = di_generic$links_2_VPM_to_GF, 
                 igraphWeighted = di_weightedIndicators$links_2_VPM_to_GF)


# -------------------------------------------------------------------------
# REMOVE NODES (PHYSICAL OBJECTS) -----------------------------------------
# -------------------------------------------------------------------------
# BASELINE PHYSICAL OBJECTS
# Object count can be RDS or csv
# keyFilename is OSM_AH_key.csv by default
dvPO =
  baselinePhysicalObjects_fromFile_revised(
    objectCountFilename = "OSMtidyOutputs/Bristol_City_6_dataTidy-filtered_20200712-170018.RDS", ################
    keyFilename = "melissaInputs/key_MBtest4.csv")

# VECTOR OF NODES TO REMOVE
# No function as this seems to case specific
dv_removeNodes = dl$nodeList$layer_5_PO$`TRUE`[!dl$nodeList$layer_5_PO$`TRUE` %in% dvPO]
nodeInfo$filterCoverage
# REMOVE NODES
dl_nodesRemoved = removeNodes(igraph = di_weightedIndicators, removeNodes = dv_removeNodes)

# Create object for removed nodes
nodeInfo_removed = 
  dl_nodesRemoved$removedNodes %>% 
  mutate(location = locationName, scenario = "indicators_objects") %>% 
  inner_join(nodeInfo, by = "nodeName") %>%
  arrange(layer)

# Export removed nodes
nodeInfo_removed %>% export(exportType = "removedNodes", location = locationName)

# Create object for removed links
linkInfo_removed = 
  dl_nodesRemoved$brokenLinks %>% 
  mutate(location = locationName, scenario = "indicators_objects")
# Export removed links
linkInfo_removed %>% export(exportType = "brokenLinks", location = locationName)

# Create new edgelist
di_weightedIndicators_and_nodesRemoved = dl_nodesRemoved$igraph

# Export new edgelist
di_weightedIndicators_and_nodesRemoved %>% 
  transform_igraphList_igraph %>% 
  as_data_frame() %>% 
  mutate(location = locationName, scenario = "indicators_objects") %>%
  export(exportType = "edgelist_indicators_objects", location = locationName)


# -------------------------------------------------------------------------
# WEIGHTING HAZARD --------------------------------------------------------
# -------------------------------------------------------------------------
# HAZARD WEIGHTING DATA
weightsHazard %>% args
# objectcount can be a csv or RDS filename, or the object count directly
# baselineScenario is a string representing the name of the baseline scenario
# key is the filename of the OSMtidy-AHgen key; OSMAHkey_CS.csv by default

di_weightsHazard <- weightsHazard("outputs/AHgen_Jaipur_objectCount_manual.RDS", #####
                                  keyFilename = "melissaInputs/key_MBtest4.csv") #####
di_weightsHazard %>% summary
di_weightsHazard$removeNodes
di_weightsHazard$weightNodes #### Can export using export()

dl_removeNodes <- di_weightsHazard$removeNodes; dl_removeNodes


# REMOVE NODES KNOCKED OUT BY HAZARD
dl_withHazard <- 
  removeNodes_hazard(igraph = di_weightedIndicators_and_nodesRemoved, 
                     removeNodesList = dl_removeNodes)
dl_withHazard %>% summary

dl_withHazard$scenario_buffer_1000m %>% summary # A runthrough with removals
dl_withHazard$scenario_water %>% summary # A runthrough without removals (no nodes to remove)


# WEIGHT HAZARD
di_weightedHazards <- weightHazard(dl_withHazard, di_weightsHazard$weightNodes)
di_weightedHazards %>% summary


# -------------------------------------------------------------------------
# RESULTS -----------------------------------------------------------------
# -------------------------------------------------------------------------

# WEIGHTED BY INDICATORS ONLY

# Format nodewise results
# Need to ensure columns compatible with generic & objects results i.e. add "osmFrequency" etc.
dt_weightedIndicators = 
  di_weightedIndicators %>% 
  transform_igraphList_igraph() %>% 
  results_nodeWise %>% 
  select(-WVBC) %>% # remove inaccurate WVBC results and fix in function later
  inner_join(nodeInfo, by = "nodeName") %>%
  mutate(location = locationName, scenario = "indicators", osmFrequency = "0",
         rank_UWVBCoverall = rank(desc(UWVBC))) %>%
  group_by(layer) %>%
  mutate(rank_UWVBCbyLayer = rank(desc(UWVBC)))

# Attach comparison to generic results (UWVBC)
dt_weightedIndicators = 
  merge(x = dt_weightedIndicators, 
        y = resultsBaseline_nodewise[ , c("nodeName", "UWVBC_generic")], 
        by = "nodeName", all.x = TRUE)

# Calculate percentage change from generic (UWVBC)
dt_weightedIndicators = 
  dt_weightedIndicators %>%
  mutate(change_UWVBC = (UWVBC - UWVBC_generic) / UWVBC_generic * 100,
         change_UWVBC = round(change_UWVBC, digits = 2),
         change_UWVBC = ifelse(is.nan(change_UWVBC), 0, change_UWVBC)) %>%
  select(-UWVBC_generic)

# Add WVBC & WVBC rankings to nodewise results
dt_weightedIndicators = 
  di_weightedIndicators %>% 
  results_tnet %>% 
  inner_join(dt_weightedIndicators) %>%
  mutate(rank_WVBCoverall = rank(desc(WVBC))) %>%
  group_by(layer) %>%
  mutate(rank_WVBCbyLayer = rank(desc(WVBC)))

# Attach comparison to generic results (WVBC)
dt_weightedIndicators = 
  merge(x = dt_weightedIndicators, 
        y = resultsBaseline_nodewise[ , c("nodeName", "WVBC_generic")], 
        by = "nodeName", all.x = TRUE)

# Calculate percentage change from generic (WVBC)
dt_weightedIndicators = 
  dt_weightedIndicators %>%
  mutate(change_WVBC = (WVBC - WVBC_generic) / WVBC_generic * 100,
         change_WVBC = round(change_WVBC, digits = 2),
         change_WVBC = ifelse(is.nan(change_WVBC), 0, change_WVBC)) %>%
  select(-WVBC_generic)

# Organise rows and columns as desired
dt_weightedIndicators =
  dt_weightedIndicators %>%
  select(nodeName, sort(colnames(.))) %>%
  arrange(layer, desc(UWVBC))

# Export nodewise results
dt_weightedIndicators %>% export("nodewise_indicators", location = locationName)


# Results for network summary
di_weightedIndicators %>% 
  transform_igraphList_igraph %>% 
  results_networkSummary %>% 
  mutate(nNodes_FP = table(dt_weightedIndicators$layer)["1"],
         nNodes_VPM = table(dt_weightedIndicators$layer)["2"],
         nNodes_GF = table(dt_weightedIndicators$layer)["3"],
         nNodes_ORP = table(dt_weightedIndicators$layer)["4"],
         nNodes_PO = table(dt_weightedIndicators$layer)["5"],
         nNodes_total_rem = "0",
         nNodes_FP_rem = "0",
         nNodes_VPM_rem = "0",
         nNodes_GF_rem = "0",
         nNodes_ORP_rem = "0",
         nNodes_PO_rem = "0",
         nEdges_level1 = ecount(di_weightedIndicators$links_1_FP_to_VPM), 
         nEdges_level2 = ecount(di_weightedIndicators$links_2_VPM_to_GF),
         nEdges_level3 = ecount(di_weightedIndicators$links_3_GF_to_ORP),
         nEdges_level4 = ecount(di_weightedIndicators$links_4_ORP_to_PO),
         nEdges_total_rem = "0",
         nEdges_level1_rem = "0",
         nEdges_level2_rem = "0",
         nEdges_level3_rem = "0",
         nEdges_level4_rem = "0",
         location = locationName, scenario = "indicators") %>%
  export("summary_indicators", location = locationName)


# WEIGHTED BY INDICATORS + OBJECTS

# Format nodewise results
dt_weightedIndicators_and_nodesRemoved = 
  di_weightedIndicators_and_nodesRemoved %>% 
  transform_igraphList_igraph() %>% 
  results_nodeWise %>% 
  select(-WVBC) %>%
  inner_join(nodeInfo, by = "nodeName") %>%
  mutate(location = locationName, scenario = "indicators_objects", 
         rank_UWVBCoverall = rank(desc(UWVBC))) %>%
  group_by(layer) %>%
  mutate(rank_UWVBCbyLayer = rank(desc(UWVBC)))

# Attach comparison to generic results (UWVBC)
dt_weightedIndicators_and_nodesRemoved = 
  merge(x = dt_weightedIndicators_and_nodesRemoved, 
        y = resultsBaseline_nodewise[ , c("nodeName", "UWVBC_generic")], 
        by = "nodeName", all.x = TRUE)

# Calculate percentage change from generic (UWVBC)
dt_weightedIndicators_and_nodesRemoved = 
  dt_weightedIndicators_and_nodesRemoved %>%
  mutate(change_UWVBC = (UWVBC - UWVBC_generic) / UWVBC_generic * 100,
         change_UWVBC = round(change_UWVBC, digits = 2),
         change_UWVBC = ifelse(is.nan(change_UWVBC), 0, change_UWVBC)) %>%
  select(-UWVBC_generic)

# Add WVBC & WVBC rankings to nodewise results
dt_weightedIndicators_and_nodesRemoved = 
  di_weightedIndicators_and_nodesRemoved %>% 
  results_tnet %>% 
  inner_join(dt_weightedIndicators_and_nodesRemoved) %>%
  mutate(rank_WVBCoverall = rank(desc(WVBC))) %>%
  group_by(layer) %>%
  mutate(rank_WVBCbyLayer = rank(desc(WVBC)))

# Attach comparison to generic results (WVBC)
dt_weightedIndicators_and_nodesRemoved = 
  merge(x = dt_weightedIndicators_and_nodesRemoved, 
        y = resultsBaseline_nodewise[ , c("nodeName", "WVBC_generic")], 
        by = "nodeName", all.x = TRUE)

# Calculate percentage change from generic (WVBC)
dt_weightedIndicators_and_nodesRemoved = 
  dt_weightedIndicators_and_nodesRemoved %>%
  mutate(change_WVBC = (WVBC - WVBC_generic) / WVBC_generic * 100,
         change_WVBC = round(change_WVBC, digits = 2),
         change_WVBC = ifelse(is.nan(change_WVBC), 0, change_WVBC)) %>%
  select(-WVBC_generic)

# Add object count
objectCount_rough <- function(objectCountFilename, keyFilename = "OSM_AH_key.csv") { 
  
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
  
  dk <- dt_OSMtidy %>% left_join(dk, by = "desc") %>% select(nodeName)
  
  dk <- dk %>% table() %>% as.data.frame()
  
}
# Create object count
oCount = objectCount_rough(
  objectCountFilename = "OSMtidyOutputs/Bristol_City_6_dataTidy-filtered_20200712-170018.RDS", ################
  keyFilename = "melissaInputs/key_MBtest4.csv")
# Rename columns in oCount
colnames(oCount) = c("nodeName", "osmFrequency")
# Join to results file
dt_weightedIndicators_and_nodesRemoved = 
  dt_weightedIndicators_and_nodesRemoved %>% full_join(oCount, by = "nodeName")

# Organise rows and columns as desired
dt_weightedIndicators_and_nodesRemoved =
  dt_weightedIndicators_and_nodesRemoved %>%
  select(nodeName, sort(colnames(.))) %>%
  arrange(layer, desc(UWVBC))

# Export nodewise results
dt_weightedIndicators_and_nodesRemoved %>% export("nodewise_indicators_objects", location = locationName)


# Results for network summary
di_weightedIndicators_and_nodesRemoved %>% 
  transform_igraphList_igraph %>% 
  results_networkSummary %>% 
  mutate(nNodes_FP = table(dt_weightedIndicators_and_nodesRemoved$layer)["1"],
         nNodes_VPM = table(dt_weightedIndicators_and_nodesRemoved$layer)["2"],
         nNodes_GF = table(dt_weightedIndicators_and_nodesRemoved$layer)["3"],
         nNodes_ORP = table(dt_weightedIndicators_and_nodesRemoved$layer)["4"],
         nNodes_PO = table(dt_weightedIndicators_and_nodesRemoved$layer)["5"],
         nNodes_total_rem = count(nodeInfo_removed),
         nNodes_FP_rem = table(nodeInfo_removed$layer)["1"],
         nNodes_VPM_rem = table(nodeInfo_removed$layer)["2"],
         nNodes_GF_rem = table(nodeInfo_removed$layer)["3"],
         nNodes_ORP_rem = table(nodeInfo_removed$layer)["4"],
         nNodes_PO_rem = table(nodeInfo_removed$layer)["5"],
         nEdges_level1 = ecount(di_weightedIndicators_and_nodesRemoved$links_1_FP_to_VPM), 
         nEdges_level2 = ecount(di_weightedIndicators_and_nodesRemoved$links_2_VPM_to_GF),
         nEdges_level3 = ecount(di_weightedIndicators_and_nodesRemoved$links_3_GF_to_ORP),
         nEdges_level4 = ecount(di_weightedIndicators_and_nodesRemoved$links_4_ORP_to_PO),
         nEdges_total_rem = count(linkInfo_removed),
         nEdges_level1_rem = table(linkInfo_removed$level)["links_1_FP_to_VPM"],
         nEdges_level2_rem = table(linkInfo_removed$level)["links_2_VPM_to_GF"],
         nEdges_level3_rem = table(linkInfo_removed$level)["links_3_GF_to_ORP"],
         nEdges_level4_rem = table(linkInfo_removed$level)["links_4_ORP_to_PO"],
         location = locationName, scenario = "indicators_objects") %>%
  export("summary_indicators_objects", location = locationName)


# PREPARE COMPARATIVE RESULTS ---------------------------------------------
# Load filenames for network nodewise results
filesNodewise = 
  list.files(path = "./outputs/", 
             pattern = c("(generic_nodewise|nodewise_indicators_objects).*\\.csv"), 
             full.names = T)

# Load filenames for network summary results
filesSummary = 
  list.files(path = "./outputs/", 
             pattern = c("(generic_summary|summary_indicators_objects).*\\.csv"), 
             full.names = T)

# Create dataframe combining all network nodewise results
dtNodewise = 
  lapply(1:length(filesNodewise), function(x) { 
    read_csv(filesNodewise[[x]], col_types = cols())
  }) %>%
  do.call("rbind",.)

# Create dataframe combining all network summary results
dtSummary = 
  lapply(1:length(filesSummary), function(x) { 
    read_csv(filesSummary[[x]], col_types = cols())
  }) %>%
  do.call("rbind",.)

# Format summary output
dtSummary = 
  dtSummary %>%
  t() %>%
  as.data.frame %>%
  setDT(keep.rownames = TRUE)


# RESULTS EXPORTS ---------------------------------------------------------

# BASIC RESULTS EXPORTS

# Export nodewise output
dtNodewise %>%  arrange(location, layer, desc(WVBC)) %>% export("nodewise", location = "all")

# Export summary output
dtSummary %>% export("summary", location = "all")


# ADVANCED RESULTS EXPORTS

# Add some range information
rankschange = 
  dtNodewise %>%
  select(layer, layerName, nodeName, location, rank_WVBCoverall, rank_WVBCbyLayer) %>%
  group_by(nodeName) %>%
  mutate(range_rank_WVBCoverall_inclG = max(rank_WVBCoverall) - min(rank_WVBCoverall),
         range_rank_WVBCbyLayer_inclG = max(rank_WVBCbyLayer) - min(rank_WVBCbyLayer)) %>%
  filter(location != "generic") %>%
  mutate(range_rank_WVBCoverall_exclG = max(rank_WVBCoverall) - min(rank_WVBCoverall),
         range_rank_WVBCbyLayer_exclG = max(rank_WVBCbyLayer) - min(rank_WVBCbyLayer))

# Export all nodeNames for each location and layer based on a single metric (rank_WVBCbyLayer)
ranks = 
  dtNodewise %>%
  select(layer, layerName, nodeName, location, rank_WVBCbyLayer) %>%
  pivot_wider(names_from = location, values_from = rank_WVBCbyLayer,
              names_prefix = "rank_WVBCbyLayer_") %>%
  arrange(layer, rank_WVBCbyLayer_generic)

ranks %>% export("rank_WVBCbyLayer", location = "all")

# Export top 5 nodeNames for each location and layer based on a single metric (WVBC)
top5 = 
  dtNodewise %>%
  select(layer, layerName, nodeName, location, WVBC) %>%
  group_by(layerName, location) %>%
  top_n(5, WVBC) %>%
  arrange(location, layer, desc(WVBC)) %>% 
  pivot_wider(names_from = location, values_from = WVBC,
              names_prefix = "WVBC_")

top5 = export("top5_WVBC", location = "all")

# Export top quantile nodenames for each location and layer based on UWVBC
topQuantile = 
  dtNodewise %>%
  select(layer, layerName, nodeName, location, WVBC) %>%
  group_by(layerName, location) %>%
  filter(WVBC > quantile(WVBC, 0.8)) %>%
  arrange(location, layer, desc(WVBC)) # try gather/spread for easier comparative output

topQuantile %>% export("topQuantile_WVBC", location = "all")

# Find top nodes by layer by WVBC, including generic values
function_topNames_byWVBC <- function(nQuantile, nTop) {
  output =
    dtNodewise %>%
    select(layer, location, nodeName, WVBC) %>%
    group_by(layer, location) %>%
    filter(WVBC > quantile(WVBC, nQuantile))
  
  output =
    output %>%
    group_by(layer, location) %>%
    top_n(nTop, WVBC) %>%
    ungroup(layer, location) %>%
    select(nodeName) %>%
    unique()
}
topNames_byWVBC = function_topNames_byWVBC(nQuantile = 0.8, nTop = 10)

# Find top nodes by layer by range in UWVBC across locations, excluding generic values
function_topNames_byRangeWVBC <- function(nQuantile, nTop) {
  dtNodewise %>%
    select(layer, location, nodeName, WVBC) %>%
    filter(location != "generic") %>%
    group_by(nodeName) %>%
    mutate(range_WVBC = max(WVBC) - min(WVBC)) %>%
    select(layer, nodeName, range_WVBC) %>%
    unique() %>%
    ungroup(nodeName) %>%
    group_by(layer) %>%
    filter(range_WVBC > quantile(range_WVBC, nQuantile)) %>%
    top_n(nTop, range_WVBC) %>%
    ungroup(layer) %>%
    select(nodeName)
}
topNames_byRangeWVBC = function_topNames_byRangeWVBC(nQuantile = 0.8, nTop = 10) 

# Find top nodes by layer by change_UWVBC, necessarily excluding generic values
function_topNames_byChangeWVBC <- function(nQuantile, nTop) {
  output =
    dtNodewise %>%
    select(layer, location, nodeName, change_WVBC) %>%
    filter(location != "generic") %>%
    group_by(layer, location) %>%
    filter(change_WVBC > quantile(change_WVBC, nQuantile))
  
  output =
    output %>%
    group_by(layer, location) %>%
    top_n(nTop, change_WVBC) %>%
    ungroup(layer, location) %>%
    select(nodeName) %>%
    unique()
}
topNames_byChangeWVBC = function_topNames_byChangeWVBC(nQuantile = 0.8, nTop = 10)

# Find top nodes by layer by change_UWVBC, necessarily excluding generic values
function_topNames_byObjectCount <- function(nQuantile) {
  dtNodewise %>%
    select(layer, location, nodeName, osmFrequency) %>%
    filter(location != "generic", layer == 5) %>%
    mutate(osmFrequency = replace_na(osmFrequency, 0)) %>%
    group_by(location) %>%
    filter(osmFrequency > quantile(osmFrequency, nQuantile)) %>%
    ungroup(location) %>%
    select(nodeName) %>%
    unique()
}
topNames_byObjectCount = function_topNames_byObjectCount(nQuantile = 0.95)

# Find top nodes by change in WVBC rank overall
function_topNames_byRankChangeWVBC_overall <- function(nQuantile, nTop) {
  output =
    dtNodewise %>%
    select(nodeName, layer, location, rank_WVBCoverall, rank_WVBCbyLayer) %>%
    group_by(nodeName) %>%
    mutate(range_rank_WVBCoverall_inclG = max(rank_WVBCoverall) - min(rank_WVBCoverall),
           range_rank_WVBCbyLayer_inclG = max(rank_WVBCbyLayer) - min(rank_WVBCbyLayer)) %>%
    filter(location != "generic") %>%
    mutate(range_rank_WVBCoverall_exclG = max(rank_WVBCoverall) - min(rank_WVBCoverall),
           range_rank_WVBCbyLayer_exclG = max(rank_WVBCbyLayer) - min(rank_WVBCbyLayer)) %>%
    ungroup(nodeName) %>%
    group_by(layer, location) %>%
    filter(range_rank_WVBCoverall_exclG > quantile(range_rank_WVBCoverall_exclG, nQuantile))
  
  output =
    output %>%
    group_by(layer, location) %>%
    top_n(nTop, range_rank_WVBCoverall_exclG) %>%
    ungroup(layer, location) %>%
    select(nodeName) %>%
    unique()
}
topNames_byRankChangeWVBC_overall = function_topNames_byRankChangeWVBC_overall(nQuantile = 0.8, nTop = 5)

# Find top nodes by change in WVBC rank overall
function_topNames_byRankChangeWVBC_byLayer <- function(nQuantile, nTop) {
  output =
    dtNodewise %>%
    select(nodeName, layer, location, rank_WVBCoverall, rank_WVBCbyLayer) %>%
    group_by(nodeName) %>%
    mutate(range_rank_WVBCoverall_inclG = max(rank_WVBCoverall) - min(rank_WVBCoverall),
           range_rank_WVBCbyLayer_inclG = max(rank_WVBCbyLayer) - min(rank_WVBCbyLayer)) %>%
    filter(location != "generic") %>%
    mutate(range_rank_WVBCoverall_exclG = max(rank_WVBCoverall) - min(rank_WVBCoverall),
           range_rank_WVBCbyLayer_exclG = max(rank_WVBCbyLayer) - min(rank_WVBCbyLayer)) %>%
    ungroup(nodeName) %>%
    group_by(layer, location) %>%
    filter(range_rank_WVBCoverall_exclG > quantile(range_rank_WVBCbyLayer_exclG, nQuantile))
  
  output =
    output %>%
    group_by(layer, location) %>%
    top_n(nTop, range_rank_WVBCbyLayer_exclG) %>%
    ungroup(layer, location) %>%
    select(nodeName) %>%
    unique()
}
topNames_byRankChangeWVBC_byLayer = function_topNames_byRankChangeWVBC_byLayer(nQuantile = 0.8, nTop = 5)


# Find top nodes by change in WVBC rank by layer
# spread & change column headers to location+scenario name?
# + averages columns?


# VISUALISATIONS ----------------------------------------------------------

# Visualise differences by scenario
ggplot(dtNodewise, aes(x = location, y = UWVBC)) +
  geom_boxplot()


# Plot WVBC by location and level
function_plotByLevels1 <- function(xName, lower, upper) {
  
  # For more complex visualisations by layer of network, specify layerName as an ordered factor
  dtNodewise$layerName = 
    factor(dtNodewise$layerName, c("Physical objects", "Object-related processes",
                                   "Generalised functions", "Values and priority measures",
                                   "Functional purposes"))
  
  dtNodewise %>%
    ggplot(aes(x = layerName, y = WVBC, colour = location)) +
    
    stat_boxplot(geom = "errorbar", width = 0.5, position = position_dodge(width = 0.75)) +
    geom_boxplot(outlier.shape = 1) + 
    
    theme_bw(base_size = 11, base_family = "Arial") +
    
    theme(legend.position = "top", panel.grid.minor= element_blank()) +
    coord_flip() +
    
    labs(x = "Level of Abstraction Hierarchy",
         y = xName,
         colour = "Location:") +
    
    scale_colour_viridis(discrete = TRUE, begin = 0.2, end = 0.8) +
    scale_y_continuous(limits = c(lower, upper))
}

function_plotByLevels1(xName = "Weighted vertex betweenness centrality", lower = 0, upper = 1600)


# Plot percentage change in UWVBC (compared to generic template) by location and level
function_plotByLevels2 <- function(xName, lower, upper) {
  
  # For more complex visualisations by layer of network, specify layerName as an ordered factor
  dtNodewise$layerName = 
    factor(dtNodewise$layerName, c("Physical objects", "Object-related processes",
                                   "Generalised functions", "Values and priority measures",
                                   "Functional purposes"))
  
  dtNodewise %>%
    ggplot(aes(x = layerName, y = change_WVBC, colour = location)) +
    
    stat_boxplot(geom = "errorbar", width = 0.5, position = position_dodge(width = 0.75)) +
    geom_boxplot(outlier.shape = 1) + 
    
    theme_bw(base_size = 11, base_family = "Arial") +
    
    theme(legend.position = "top", panel.grid.minor= element_blank()) +
    coord_flip() +
    
    labs(x = "Level of Abstraction Hierarchy",
         y = xName,
         colour = "Location:") +
    
    scale_colour_viridis(discrete = TRUE, begin = 0.2, end = 0.8) +
    scale_y_continuous(limits = c(lower, upper))
}

function_plotByLevels2(
  xName = "Percentage change in WVBC from generic UK city template (%)", lower = -100, upper = 100)


# Plot all results in a level (with x-axis labels)
function_plotLevel1 <- function(nLayer, ptSize, xName, lower, upper) {
  dtNodewise %>%
    filter(layer == nLayer) %>%
    
    ggplot(aes(x = nodeName, y = WVBC, colour = location)) +
    
    geom_point(size = ptSize) +
    
    theme_bw(base_size = 11, base_family = "Arial") +
    
    theme(legend.position = "top", panel.grid.minor= element_blank(),
          axis.text.x = element_text(angle = 90)) +
    
    labs(x = xName,
         y = "Weighted vertex betweenness centrality",
         colour = "Location:") +
    
    scale_colour_viridis(discrete = TRUE, begin = 0.1, end = 0.9) +
    scale_y_continuous(limits = c(lower,upper))
}

function_plotLevel1(nLayer = 1, ptSize = 3, xName = "Functional purpose", lower = 10 , upper = 50)
function_plotLevel1(nLayer = 2, ptSize = 2, xName = "Value and priority measure", lower = 0 , upper = 800)
function_plotLevel1(nLayer = 3, ptSize = 2, xName = "Generalised function", lower = 0, upper = 10000)

# Plot all results in a level (without x-axis labels)
function_plotLevel2 <- function(nLayer, ptSize, xName, lower, upper) {
  dtNodewise %>%
    filter(layer == nLayer) %>%
    
    ggplot(aes(x = nodeName, y = WVBC, colour = location)) +
    
    geom_point(size = ptSize) +
    
    theme_bw(base_size = 11, base_family = "Arial") +
    
    theme(legend.position = "top", panel.grid.minor= element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    
    labs(x = xName,
         y = "Weighted vertex betweenness centrality",
         colour = "Location:") +
    
    scale_colour_viridis(discrete = TRUE, begin = 0.1, end = 0.9) +
    scale_y_continuous(limits = c(lower,upper))
}

function_plotLevel2(nLayer = 4, ptSize = 1, xName = "Object-related process", lower = 0, upper = 8500)
function_plotLevel2(nLayer = 5, ptSize = 1, xName = "Physical object", lower = 0, upper = 3000)


# Plot top nodes determined by metric and subset by layer
function_plotHighlights <- function(nodeGroup, nLayer, xName, lower, upper) {
  dtNodewise %>%
    filter(nodeName %in% nodeGroup, layer == nLayer) %>%
  
  ggplot(aes(x = nodeName, y = WVBC, colour = location)) +
  
  geom_point(size = 3) +
  
  theme_bw(base_size = 11, base_family = "Arial") +
  
  theme(legend.position = "top", panel.grid.minor= element_blank(),
        axis.text.x = element_text(angle = 90)) +
  
  labs(x = xName,
       y = "Weighted vertex betweenness centrality",
       colour = "Location:") +
    
  scale_colour_viridis(discrete = TRUE, begin = 0.1, end = 0.9) +
  scale_y_continuous(limits = c(lower,upper))
}

function_plotHighlights(
  nodeGroup = topNames_byWVBC$nodeName, 
  nLayer = 5, xName = "Physical object", lower = 600, upper = 2800)
function_plotHighlights(
  nodeGroup = topNames_byWVBC$nodeName, 
  nLayer = 4, xName = "Object-related process", lower = 3000, upper = 8500)
function_plotHighlights(
  nodeGroup = topNames_byWVBC$nodeName, 
  nLayer = 3, xName = "Generalised function", lower = 0, upper = 10000) # can we do a log scale?
function_plotHighlights(
  nodeGroup = topNames_byWVBC$nodeName, 
  nLayer = 2, xName = "Value and priority measure", lower = 500, upper = 1700)
function_plotHighlights(
  nodeGroup = topNames_byWVBC$nodeName, 
  nLayer = 1, xName = "Functional purpose", lower = 35, upper = 50)

function_plotHighlights(
  nodeGroup = topNames_byRangeWVBC$nodeName, 
  nLayer = 5, xName = "Physical object", lower = 350, upper = 1500)
function_plotHighlights(
  nodeGroup = topNames_byRangeWVBC$nodeName, 
  nLayer = 4, xName = "Object-related process", lower = 3000, upper = 8500)
function_plotHighlights(
  nodeGroup = topNames_byRangeWVBC$nodeName, 
  nLayer = 3, xName = "Generalised function", lower = 200, upper = 10000) # can we do a log scale?
function_plotHighlights(
  nodeGroup = topNames_byRangeWVBC$nodeName, 
  nLayer = 2, xName = "Value and priority measure", lower = 0, upper = 1500)
function_plotHighlights(
  nodeGroup = topNames_byRangeWVBC$nodeName, 
  nLayer = 1, xName = "Functional purpose", lower = 35, upper = 50)

function_plotHighlights(
  nodeGroup = topNames_byChangeWVBC$nodeName, 
  nLayer = 5, xName = "Physical object", lower = 0, upper = 2500)
function_plotHighlights(
  nodeGroup = topNames_byChangeWVBC$nodeName, 
  nLayer = 4, xName = "Object-related process", lower = 0, upper = 500)
function_plotHighlights(
  nodeGroup = topNames_byChangeWVBC$nodeName, 
  nLayer = 3, xName = "Generalised function", lower = 0, upper = 3000)

function_plotHighlights(
  nodeGroup = topNames_byRankChangeWVBC_byLayer$nodeName, # would be great to order these by biggest rank range instead of alphabetically
  nLayer = 5, xName = "Physical object", lower = 0, upper = 200)
function_plotHighlights(
  nodeGroup = topNames_byRankChangeWVBC_byLayer$nodeName,
  nLayer = 4, xName = "Object-related process", lower = 0, upper = 800)
function_plotHighlights(
  nodeGroup = topNames_byRankChangeWVBC_byLayer$nodeName,
  nLayer = 3, xName = "Generalised function", lower = 0, upper = 1500)
function_plotHighlights(
  nodeGroup = topNames_byRankChangeWVBC_byLayer$nodeName,
  nLayer = 2, xName = "Value and priority measure", lower = 0, upper = 600)
function_plotHighlights(
  nodeGroup = topNames_byRankChangeWVBC_byLayer$nodeName,
  nLayer = 1, xName = "Functional purpose", lower = 0, upper = 50) # this is incorrect somehow

function_plotHighlights(
  nodeGroup = topNames_byObjectCount$nodeName,
  nLayer = 5, xName = "Physical object", lower = 0, upper = 400)


# Plot by node group
function_plotTest <- function(nodeGroup, nLayer, xName, lower, upper) {
  dtNodewise %>%
    filter(nodeName %in% nodeGroup, layer == nLayer) %>%
    
    ggplot(aes(x = nodeName, y = WVBC)) +
    
    stat_boxplot(geom = "errorbar", width = 0.5, position = position_dodge(width = 0.75)) +
    geom_boxplot(outlier.shape = 1) + 
    
    theme_bw(base_size = 11, base_family = "Arial") +
    
    theme(legend.position = "top", panel.grid.minor= element_blank()) +
    coord_flip() +
    
    labs(x = "Node",
         y = xName) +
    
    scale_colour_viridis(discrete = TRUE, begin = 0.2, end = 0.8) +
    scale_y_continuous(limits = c(lower, upper))
}

function_plotTest(
  nodeGroup = topNames_byWVBC$nodeName,
  nLayer = 5,
  xName = "Weighted vertex betweenness centrality", lower = 600, upper = 3000)
function_plotTest(
  nodeGroup = topNames_byWVBC$nodeName,
  nLayer = 4,
  xName = "Weighted vertex betweenness centrality", lower = 2000, upper = 9000)
function_plotTest(
  nodeGroup = topNames_byRangeWVBC$nodeName,
  nLayer = 5,
  xName = "Weighted vertex betweenness centrality", lower = 0, upper = 1500)
function_plotTest(
  nodeGroup = topNames_byRangeWVBC$nodeName,
  nLayer = 4,
  xName = "Weighted vertex betweenness centrality", lower = 0, upper = 10000)

# Are any topNames_byObjectCount connected to any topNames_byUWVBC for nLayer = 4? In other words,
# the most frequent physical objects do not have very high centrality at the PO level. But do
# their connected processes have a very high centrality at the ORP level? / Do the highest
# centrality ORPs have connections to the most frequent POs?

# Which are the highest centrality nodes that have been removed (from generic) at each scenario at each level
# Which are the most changed centrality nodes (from generic) at each scenario at each level

# is there a way to order entries on the x-axis in order of a metric (e.g., UWVBC to make a nicer graph,
# or osmFrequency to show frequency doesn't correlate to betweenness/interdependence)

# tube map style ranking graph
dtNodewise$location = 
  factor(dtNodewise$location, c("generic", "Bristol", "Edinburgh", "London, City of", "Manchester"))

# Rankings for functional purposes
dtNodewise %>%
  filter(layer == 1) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(1, 8, 1))

# Rankins for values and priority measures
dtNodewise %>%
  filter(layer == 2) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(1, 14, 1))

# Rankings for generalised functions
dtNodewise %>%
  filter(layer == 3) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(1, 36, 1))

# Rankings for top 5 generalised functions processes
dtNodewise %>%
  filter(layer == 3, rank_WVBCbyLayer < 6) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(1, 5, 1))

# Rankings for 6-20 generalised functions processes
dtNodewise %>%
  filter(layer == 3, rank_WVBCbyLayer > 5, rank_WVBCbyLayer < 21) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(6, 20, 1))

# Rankings for 20-28 generalised functions processes
dtNodewise %>%
  filter(layer == 3, rank_WVBCbyLayer > 19, rank_WVBCbyLayer < 29) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(20, 28, 1))

# Rankings for 29-36 generalised functions processes
dtNodewise %>%
  filter(layer == 3, rank_WVBCbyLayer > 28) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(29, 36, 1))

# Rankings for top 5 object-related processes
dtNodewise %>%
  filter(layer == 4, rank_WVBCbyLayer < 6) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(1, 5, 1))

# Rankings for top 12 object-related processes
dtNodewise %>%
  filter(layer == 4, rank_WVBCbyLayer < 12) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(1, 11, 1))

# Rankings 13-40 for object-related processes
dtNodewise %>%
  filter(layer == 4, rank_WVBCbyLayer > 12, rank_WVBCbyLayer < 41) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(13, 40, 1))

# Rankings 18-32 object-related processes
dtNodewise %>%
  filter(layer == 4, rank_WVBCbyLayer > 18, rank_WVBCbyLayer < 41) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(19, 40, 1))

# Rankings 18-32 object-related processes
dtNodewise %>%
  filter(layer == 4, rank_WVBCbyLayer > 40, rank_WVBCbyLayer < 61) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(41, 60, 1))

# Rankings 18-32 object-related processes
dtNodewise %>%
  filter(layer == 4, rank_WVBCbyLayer > 40, rank_WVBCbyLayer < 61) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(41, 60, 1))


# Rankings for physical objects, without generic
dtNodewise %>%
  filter(layer == 5, rank_WVBCbyLayer < 11) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(1, 10, 1))


# Rankings for physical objects, without generic
dtNodewise %>%
  filter(layer == 5, location != "generic", rank_WVBCbyLayer < 11) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(1, 10, 1))



# Rankings for 6-19 generalised functions processes
dtNodewise %>%
  filter(layer == 3, rank_WVBCbyLayer > 9, rank_WVBCbyLayer < 17) %>%
  ggplot(aes(x = location, y = rank_WVBCbyLayer, group = nodeName)) +
  geom_line(aes(color = nodeName), size = 1) +
  geom_point(aes(color = nodeName), size = 2) +
  scale_y_reverse(breaks = seq(10, 16, 1))

# -------------------------------------------------------------------------
# MISC --------------------------------------------------------------------
# -------------------------------------------------------------------------
# Results for entire network (case study location with hazard, applying both indicators and object weightings from OSMtidy)
di_weightedHazards$scenario_buffer_1000m %>% transform_igraphList_igraph %>% results_nodeWise
di_weightedHazards$scenario_buffer_1000m %>% transform_igraphList_igraph %>% results_networkSummary


# Results per groups of layers
di_generic %>% results_nodeWise_byLevel(format = "list")
di_generic %>% results_networkSummary_byLevel


di_generic %>%
  transform_igraphList_igraph %>%
  transform_igraph_abstractionHierarchy

# Some cool cluster stuff from igraph that I don't understand
di_generic %>% transform_igraphList_igraph %>% cluster_leading_eigen(weights = E(.)$weight)
di_generic %>% transform_igraphList_igraph %>% cluster_fast_greedy(weights = E(.)$weight)
di_generic %>% transform_igraphList_igraph %>% cluster_walktrap(weights = E(.)$weight)



# Old method for results

# Results for entire network (case study location baseline, with indicators applied)
di_weightedIndicators %>% transform_igraphList_igraph %>% results_nodeWise %>% 
  mutate(location = locationName, scenario = "indicators") %>%
  inner_join(nodeInfo, by = "nodeName") %>%
  export("nodewise_indicators", location = locationName)
# Results for network summary
di_weightedIndicators %>% transform_igraphList_igraph %>% results_networkSummary %>% 
  mutate(location = locationName, scenario = "indicators") %>%
  export("summary_indicators", location = locationName)

# Results for entire network (case study location baseline, with indicators applied and irrelevant physical objects removed)
di_weightedIndicators_and_nodesRemoved %>% transform_igraphList_igraph %>% results_nodeWise %>% 
  mutate(location = locationName, scenario = "indicators_objects") %>%
  inner_join(nodeInfo, by = "nodeName") %>%
  export("nodewise_indicators_objects", location = locationName)
di_weightedIndicators_and_nodesRemoved %>% transform_igraphList_igraph %>% results_networkSummary %>% 
  mutate(location = locationName, scenario = "indicators_objects") %>%
  export("summary_indicators_objects", location = locationName)


# All results
dlResultsAll <- function_allResults_temporary(di_generic, 
                                              di_weightedIndicators, 
                                              di_weightedHazards) # Takes a minute or two to run

list(generic = di_generic %>% 
       transform_igraphList_igraph %>% 
       results_nodeWise %>%
       mutate(version = "AH_generic") %>%
       select(version, everything()),
     
     indicators = di_weightedIndicators %>% 
       transform_igraphList_igraph %>% 
       results_nodeWise %>%
       mutate(version = "AH_indicators") %>%
       select(version, everything())) %>%
  bind_rows() %>%
  
  export(exportType = "results_nodewise", location = "Manchester")

##### weighted hazards with removed not working???

warnings()
dlResultsAll %>% summary
dlResultsAll$networkSummary
dlResultsAll$nodeWise

dlResultsAll$networkSummary %>% function_exportExcelResults_temporary("outputs/results_netSummary.xlsx")
dlResultsAll$nodeWise %>% function_exportExcelResults_temporary("outputs/results_nodeWise.xlsx")


# Original code to return top names in 80th percentile (maxed at 10 per layer)
topNames = 
  dtNodewise %>%
  select(layer, layerName, location, nodeName, UWVBC) %>%
  group_by(layer, location) %>%
  filter(UWVBC > quantile(UWVBC, 0.8))

topNames = 
  top %>%
  group_by(layer, location) %>%
  top_n(10, UWVBC) %>%
  ungroup(layer, location) %>%
  filter(layer == 5) %>%
  select(nodeName) %>%
  unique()

topNames = topNames %>% inner_join(dtNodewise, by = "nodeName")
