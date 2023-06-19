# =========================================================================
# preProcess.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Created: 2022-06-16

# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2023-06-19
# =========================================================================

# PREP --------------------------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Load packages
library(tidyverse)
library(readxl)
library(AHgen)


# EXAMPLE DATA ------------------------------------------------------------

# Load data frame
USAH_3.0_template_baseline_adjMat <- AHgen::read_adjMat("USAH_3.0_template_baseline_adjMat_20230602.xlsx")

# Load data frame
USAH_3.0_template_baseline_vInfo_full <- read_xlsx("USAH_3.0_template_baseline_vInfo-full_20230616.xlsx")

usethis::use_data(USAH_3.0_template_baseline_adjMat)
usethis::use_data(USAH_3.0_template_baseline_vInfo_full)


# COLOURS - 100 RESILIENT CITIES CATEGORIES -------------------------------

# Reformat data frame
cols100RC_df <- 
  USAH_3.0_template_baseline_vInfo_full %>% 
  mutate(levelName_full = 
           factor(levelName_full, 
                  levels = c("Functional purposes", "Values and priority measures", 
                             "Generalised functions", "Object-related processes", 
                             "Physical objects")),
         levelName = 
           factor(levelName, 
                  levels = c("Purposes", "Outcomes", "Tasks", "Processes", "Resources"))) %>%
  select(level, levelName_full, levelName, Node, colour_node_default, 
         health_wellbeing, economy_society, infrastructure_ecosystems, leadership_strategy) %>%
  filter(level %in% c(2, 3)) %>%
  rename(cols = colour_node_default)

# Pull vectors for separate outputs
colsOutcomes_100RC <- cols100RC_df %>% filter(level == 2) %>% pull(cols)
names(colsOutcomes_100RC) <- cols100RC_df %>% filter(level == 2) %>% pull(Node)

colsTasks_100RC <- cols100RC_df %>% filter(level == 3) %>% pull(cols)
names(colsTasks_100RC) <- cols100RC_df %>% filter(level == 3) %>% pull(Node)


colsOutcomes_HW <- cols100RC_df %>% filter(level == 2 & health_wellbeing == TRUE) %>% pull(cols)
names(colsOutcomes_HW) <- cols100RC_df %>% filter(level == 2 & health_wellbeing == TRUE) %>% pull(Node)

colsOutcomes_ES <- cols100RC_df %>% filter(level == 2 & economy_society == TRUE) %>% pull(cols)
names(colsOutcomes_ES) <- cols100RC_df %>% filter(level == 2 & economy_society == TRUE) %>% pull(Node)

colsOutcomes_IE <- cols100RC_df %>% filter(level == 2 & infrastructure_ecosystems == TRUE) %>% pull(cols)
names(colsOutcomes_IE) <- cols100RC_df %>% filter(level == 2 & infrastructure_ecosystems == TRUE) %>% pull(Node)

colsOutcomes_LS <- cols100RC_df %>% filter(level == 2 & leadership_strategy == TRUE) %>% pull(cols)
names(colsOutcomes_LS) <- cols100RC_df %>% filter(level == 2 & leadership_strategy == TRUE) %>% pull(Node)


colsTasks_HW <- cols100RC_df %>% filter(level == 3 & health_wellbeing == TRUE) %>% pull(cols)
names(colsTasks_HW) <- cols100RC_df %>% filter(level == 3 & health_wellbeing == TRUE) %>% pull(Node)

colsTasks_ES <- cols100RC_df %>% filter(level == 3 & economy_society == TRUE) %>% pull(cols)
names(colsTasks_ES) <- cols100RC_df %>% filter(level == 3 & economy_society == TRUE) %>% pull(Node)

colsTasks_IE <- cols100RC_df %>% filter(level == 3 & infrastructure_ecosystems == TRUE) %>% pull(cols)
names(colsTasks_IE) <- cols100RC_df %>% filter(level == 3 & infrastructure_ecosystems == TRUE) %>% pull(Node)

colsTasks_LS <- cols100RC_df %>% filter(level == 3 & leadership_strategy == TRUE) %>% pull(cols)
names(colsTasks_LS) <- cols100RC_df %>% filter(level == 3 & leadership_strategy == TRUE) %>% pull(Node)


usethis::use_data(cols100RC_df)

usethis::use_data(colsOutcomes_100RC)
usethis::use_data(colsTasks_100RC)

usethis::use_data(colsOutcomes_HW)
usethis::use_data(colsOutcomes_ES)
usethis::use_data(colsOutcomes_IE)
usethis::use_data(colsOutcomes_LS)

usethis::use_data(colsTasks_HW)
usethis::use_data(colsTasks_ES)
usethis::use_data(colsTasks_IE)
usethis::use_data(colsTasks_LS)


# COLOURS - LEVELS --------------------------------------------------------

# Reformat data frame
colsLevels_df <-
  USAH_3.0_template_baseline_vInfo_full %>%
  mutate(levelName_full = 
           factor(levelName_full, 
                  levels = c("Functional purposes", "Values and priority measures", 
                             "Generalised functions", "Object-related processes", 
                             "Physical objects")),
         levelName = 
           factor(levelName, 
                  levels = c("Purposes", "Outcomes", "Tasks", "Processes", "Resources"))) %>%
  select(level, levelName_full, levelName, colour_level_default) %>%
  rename(cols = colour_level_default) %>%
  unique()

# Pull vectors for separate outputs
colsLevels <- colsLevels_df %>% pull(cols)
names(colsLevels) <- colsLevels_df %>% pull(levelName)

colsPurposes <- colsLevels_df %>% filter(level == 1) %>% pull(cols)
names(colsPurposes) <- colsLevels_df %>% filter(level == 1) %>% pull(levelName)

colsOutcomes <- colsLevels_df %>% filter(level == 2) %>% pull(cols)
names(colsOutcomes) <- colsLevels_df %>% filter(level == 2) %>% pull(levelName)

colsTasks <- colsLevels_df %>% filter(level == 3) %>% pull(cols)
names(colsTasks) <- colsLevels_df %>% filter(level == 3) %>% pull(levelName)

colsProcesses <- colsLevels_df %>% filter(level == 4) %>% pull(cols)
names(colsProcesses) <- colsLevels_df %>% filter(level == 4) %>% pull(levelName)

colsResources <- colsLevels_df %>% filter(level == 5) %>% pull(cols)
names(colsResources) <- colsLevels_df %>% filter(level == 5) %>% pull(levelName)

usethis::use_data(colsLevels_df)

usethis::use_data(colsLevels)

usethis::use_data(colsPurposes)
usethis::use_data(colsOutcomes)
usethis::use_data(colsTasks)
usethis::use_data(colsProcesses)
usethis::use_data(colsResources)


# COLOURS - FLOOD ---------------------------------------------------------

# Load data frame
colsFloodRiver_df <- read_xlsx("aes_colsFloodRiver_df.xlsx")

# Reformat data frame
colsFloodRiver_df <-
  colsFloodRiver_df %>%
  mutate(scenario = factor(scenario, levels = c("baseline", "1 in 100-year flood", "1 in 200-year flood")),
         cols = fct_inorder(cols))

# Pull vectors for separate outputs
colsFloodRiver <- colsFloodRiver_df %>% pull(cols)

colsBaseline <- colsFloodRiver_df %>% filter(scenario == "baseline") %>% pull(cols)
colsFloodRiver100 <- colsFloodRiver_df %>% filter(scenario == "1 in 100-year flood") %>% pull(cols)
colsFloodRiver200 <- colsFloodRiver_df %>% filter(scenario == "1 in 200-year flood") %>% pull(cols)

usethis::use_data(colsFloodRiver_df)

usethis::use_data(colsFloodRiver)

usethis::use_data(colsBaseline)
usethis::use_data(colsFloodRiver100)
usethis::use_data(colsFloodRiver200)