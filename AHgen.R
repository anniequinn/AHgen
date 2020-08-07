# -------------------------------------------------------------------------
# Annie Visser-Quinn
#
# Created: 2020-07-27
# Last revised: 2020-08-06
# -------------------------------------------------------------------------

rm(list = ls()); cat("\014")

pacman::p_load(tidyverse, igraph, janitor, data.table, tnet, sf, pbapply)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("functions/functions.R")


# INPUTS ------------------------------------------------------------------
files <- list.files("inputs"); files

# Read in adjacency matrix spreadsheet as csv or excel file type

"inputs/USAH_adjMat_2020-05-20.csv" %>% read_adjMat_csv
"inputs/USAH_adjMat_2020-05-20.xlsx" %>% read_adjMat_xlsx

dh <- "inputs/USAH_adjMat_2020-05-20.xlsx" %>% read_adjMat_xlsx; dh


# Read in node info spreadsheet as csv or excel file type

"inputs/USAH_nodeInfo_2020-05-20.csv" %>% read_nodeInfo_csv
"inputs/USAH_nodeInfo_2020-05-20.xlsx" %>% read_nodeInfo_xlsx

dk <- "inputs/USAH_nodeInfo_2020-05-20.xlsx" %>% read_nodeInfo_xlsx; dk


# OTHER FUNCTIONS ---------------------------------------------------------

# Convert adjacency matrix to edgelist
dh %>% adjMat_to_edgelist
dh %>% adjMat_to_edgelist(outputList = TRUE)


# Convert edgelist to an igraph
dh %>% adjMat_to_edgelist %>% edgelist_to_igraph(outputList = TRUE)
dh %>% adjMat_to_edgelist %>% edgelist_to_igraph(outputList = FALSE)


# Explore the nodes and edges
i = dh %>% adjMat_to_edgelist %>% edgelist_to_igraph(outputList = FALSE)

V(i)
E(i)
E(i)$weight
E(i)$layers


# Apply weighting when in edgelist format
de <- dh %>% adjMat_to_edgelist
de

set.seed(1)
de2 <- de %>% filter(layers == "l1FP_l2VPM") %>% rowwise() %>% mutate(weightNew = sample(seq(0.01,1,0.01),1:n())) %>% select(-weight)
de2

weightLinks(de, de2, "edgelist")

de3 <- dh %>% adjMat_to_edgelist %>% edgelist_to_igraph(); de3
de4 <- de2 %>% rename(weight = weightNew) %>% edgelist_to_igraph; de4

weightLinks(de3, de4, "igraph")


# INDICATORS DATABASE -----------------------------------------------------

# Location specific indicators quantified for a range of UK cities
# Available in the indicators database

read_indicatorsDatabase("inputs/linkWeights_indicatorsDatabase_2020-05-22.xlsx",
                        "Bristol")

read_indicatorsDatabase("inputs/linkWeights_indicatorsDatabase_2020-05-22.xlsx",
                        "Edinburgh")

read_indicatorsDatabase("inputs/linkWeights_indicatorsDatabase_2020-05-22.xlsx",
                        "Glasgow")








################ Remaining steps

#-----------------------------------------------------
# PART A3 - REMOVE NODES (PHYSICAL OBJECTS) -------------------------------
# -------------------------------------------------------------------------
# BASELINE PHYSICAL OBJECTS
baselinePhysicalObjects_fromFile %>% args

# Object count can be RDS or csv
# keyFilename is OSMAHkey_CS.csv by default

# Awaiting key that works with new object names
# Example uses old data but format changed to match new outputs from OSMtidy
dvPO <- 
  baselinePhysicalObjects_fromFile(
    objectCountFilename = "outputs/AHgen_Jaipur_objectCount_manual.RDS", ######
    keyFilename = "melissaInputs/OSMAHkey_CS.csv")
dvPO


# VECTOR OF NODES TO REMOVE
# No function as this seems to case specific
dv_removeNodes <- dl$nodeList$layer_5_PO$`TRUE`[!dl$nodeList$layer_5_PO$`TRUE` %in% dvPO]
dv_removeNodes


# REMOVE NODES
removeNodes %>% args
dl_nodesRemoved <- removeNodes(igraph = di_weightedIndicators, removeNodes = dv_removeNodes)
dl_nodesRemoved %>% summary

dl_nodesRemoved$removedNodes
dl_nodesRemoved$brokenLinks

dl_nodesRemoved$removedNodes %>% export(exportType = "removedNodes", location = locationName)
dl_nodesRemoved$brokenLinks %>% export(exportType = "brokenLinks", location = locationName)

di_weightedIndicators_and_nodesRemoved <- dl_nodesRemoved$igraph
di_weightedIndicators_and_nodesRemoved

ls(pattern = "di_")


# -------------------------------------------------------------------------
# PART A4 - WEIGHTING HAZARD ----------------------------------------------
# -------------------------------------------------------------------------
# HAZARD WEIGHTING DATA
weightsHazard %>% args
# objectcount can be a csv or RDS filename, or the object count directly
# baselineScenario is a string representing the name of the baseline scenario
# key is the filename of the OSMtidy-AHgen key; OSMAHkey_CS.csv by default

di_weightsHazard <- weightsHazard("outputs/AHgen_Jaipur_objectCount_manual.RDS",
                                  keyFilename = "melissaInputs/OSMAHkey_CS.csv")
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

di_weightedHazards$scenario_buffer_1000m

##### hazard and interventions
##### layering of the hazards and interventions

