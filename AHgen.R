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