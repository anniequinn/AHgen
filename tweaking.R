rm(list = ls()); cat("\014")

pacman::p_load(tidyverse, igraph, data.table, tnet, readxl)

setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

source("functions/functions.R")

dh <- "inputs/AH_microwave.xlsx" %>% read_adjMat
dk <- "inputs/AH_microwave.xlsx" %>% read_vInfo(sheet = 2)
de <- dh %>% adjMat_to_edgelist(dk); de
di <- de %>% edgelist_to_igraph(dk); di


filename <- "inputs/indicatorsDatabase_2020-05-22.xlsx"
# filename <- "inputs/ind.xlsx"

# scenario = "Edinburgh" ####
# scenario <- list(name = "scenario1_plug1", preferenceLevels = NULL)
scenario <- list(name = "Edinburgh", preferenceLevels = c("Preferred", "Alternate")) # In order
scenario

read_weightScenario("inputs/indicatorsDatabase_2020-05-22.xlsx", 
                    scenario = list(name = "Edinburgh", 
                                    preferenceLevels = c("Preferred", "Alternate")), # In order
                    rescale = FALSE)

read_weightScenario("inputs/indicatorsDatabase_2020-05-22.xlsx", 
                    scenario = list(name = "Edinburgh", 
                                    preferenceLevels = c("Preferred", "Alternate")), # In order
                    rescale = TRUE)

read_weightScenario("inputs/ind.xlsx", 
                    scenario = "scenario1_plug1",
                    rescale = FALSE)



