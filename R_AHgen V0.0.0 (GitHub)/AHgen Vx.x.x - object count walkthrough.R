# -------------------------------------------------------------------------
# AHgen - hazard cutout walkthrough.R
# Annie Visser-Quinn
#
# Created: 01-05-2020
# Last revised: 18-05-2020
# -------------------------------------------------------------------------

rm(list = ls()); cat("\014")

pacman::p_load(tidyverse, igraph, janitor, data.table, tnet, sf, pbapply)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("functions/functions.R")


# -------------------------------------------------------------------------
# DATA --------------------------------------------------------------------
# -------------------------------------------------------------------------
# Import a geotagged database of physical objects
dlGeo <- readRDS("../OSMtidy V0.0.3b/outputs/Jaipur_6_dataGeotagged_20200501-140640.RDS") # OSMtidy V0.0.3 2020-05-01
dlGeo$geotaggedDatabase %>% nrow()
dlGeo$shapefile


# -------------------------------------------------------------------------
# CUT OUT OBJECTS INSIDE HAZARD AREA(S) -----------------------------------
# -------------------------------------------------------------------------
# READ IN & CUT OUT
cutHazard %>% args

# Read in individually by filename
dlHazard1 <- cutHazard(dlGeo$geotaggedDatabase, "shapefilesHazard/Jaipur/Test 5 river 1.shp"); dlHazard1

# Read in multiple by directory
dlHazard8 <- cutHazard(dlGeo$geotaggedDatabase, "shapefilesHazard/Jaipur"); dlHazard8 %>% summary


# NAME SCENARIOS (OPTIONAL)
# Scenario names are set as the filename by default
dlHazard1$`shapefilesHazard/Jaipur/Test 5 river 1.shp`$scenario %>% unique
dlHazard8$`shapefilesHazard/Jaipur/Buffer 1000m.shp`$scenario %>% unique

# Change scenario names using nameScenarios
nameScenarios %>% args
# hazardList is the output from cutHazard
# vecNames is a vector of the scenario names

dlHazard1 <- nameScenarios(dlHazard1, "scenario_river_1"); dlHazard1

dlHazard8 %>% names # Scenario names should be in the same order as the current list order
namesHazard8 <- paste0("scenario_", c("buffer_1000m", "buffer_2000m", "buffer_200m", "buffer_500m",
                                      "floodmap", "river_1", "river_2", "water"))
namesHazard8
dlHazard8 <- nameScenarios(hazardList = dlHazard8, vecNames = namesHazard8)
dlHazard8 %>% summary
dlHazard8


# -------------------------------------------------------------------------
# OBJECT COUNT ------------------------------------------------------------
# -------------------------------------------------------------------------
# # OBJECT COUNT
objectCount %>% args
# Specify baseline and/or a list of hazards

# Baseline
objectCount(baseline = dlGeo$geotaggedDatabase)

# Hazard
objectCount(hazardList = dlHazard1)
objectCount(hazardList = dlHazard8)

# Baseline and hazard
dtHazard1 <- objectCount(baseline = dlGeo$geotaggedDatabase, hazardList = dlHazard1); dtHazard1
dtHazard8 <- objectCount(baseline = dlGeo$geotaggedDatabase, hazardList = dlHazard8); dtHazard8


# -------------------------------------------------------------------------
# EXPORT (OPTIONAL) -------------------------------------------------------
# -------------------------------------------------------------------------
dtHazard8 %>% export(exportType = "objectCount", location = "Jaipur")