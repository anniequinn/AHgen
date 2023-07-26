# -------------------------------------------------------------------------
# Annie Visser-Quinn
#
# Created: 2020-07-27
# Last revised: 2020-10-17
# -------------------------------------------------------------------------

rm(list = ls()); cat("\014")

pacman::p_load(tidyverse, igraph, data.table, tnet, readxl)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("functions/functions.R")


# -------------------------------------------------------------------------
# INPUTS ------------------------------------------------------------------
# -------------------------------------------------------------------------
# Read in adjacency matrix spreadsheet as .xlsx (Excel file type)
dh <- "inputs/microwave_adjMat.xlsx" %>% read_adjMat; dh

"inputs/microwave_adjMat.xlsx" %>% read_adjMat(rescale = TRUE)
"inputs/microwave_adjMat.xlsx" %>% read_adjMat(rescale = FALSE)

# Read in vertex info as .xlsx (Excel file type)
# read_vInfo()
# or Select level, levelname and vName from adjMat

dk <- dh %>% select(level, levelName, vName); dk


# -------------------------------------------------------------------------
# INPUTS II ---------------------------------------------------------------
# -------------------------------------------------------------------------
# From adjacency matrix
de <- dh %>% adjMat_to_edgelist; de # Convert adjacency matrix to edge list
dh %>% adjMat_to_igraph # Convert adjacency matrix to igraph

# From edge list
di <- de %>% edgelist_to_igraph(vInfo = dk); di # Convert edge list to igraph
de %>% edgelist_to_adjMat(vInfo = dk) # Convert edge list to adjacency matrix

# From igraph
di %>% igraph_to_edgelist # Convert igraph to edge list
di %>% igraph_to_adjMat # Convert igraph to adjacency matrix


# -------------------------------------------------------------------------
# EXPLORE VERTICES AND EDGES ----------------------------------------------
# -------------------------------------------------------------------------
vertex_attr(di) %>% names

V(di)
V(di)$name %>% head(n = 100)
V(di)$level %>% head(n = 100)
V(di)$levelName %>% head(n = 100)

edge_attr(di) %>% names

E(di)
E(di)$weight %>% head(n = 100)
E(di)$layer %>% head(n = 100)


# -------------------------------------------------------------------------
# EDGE WEIGHTING ----------------------------------------------------------
# -------------------------------------------------------------------------
# Weight by edge list
# Weight column should be called weightNew

# Generate an example
source("functions/function_genExample.R")

de2 <- genExample(de, "l1FP_l2VPM"); de2
de2 <- weightEdges(de, de2); de2

de3 <- genExample(de); de3
de3 <- weightEdges(de, de3); de3


# # Location specific indicators quantified for a range of UK cities
# # Available in the indicators database
# 
deBristol <- read_indicatorsDatabase("inputs/indicatorsDatabase_2020-05-22.xlsx", "Bristol")
deBristol
read_indicatorsDatabase("inputs/indicatorsDatabase_2020-05-22.xlsx", "Bristol", rescale = TRUE)
# weightEdges(de, deBristol) %>% filter(layer == "l2VPM_l3GF")
# 
# deEdinburgh <- read_indicatorsDatabase("inputs/indicatorsDatabase_2020-05-22.xlsx", "Edinburgh")
# deEdinburgh
# weightEdges(de, deEdinburgh) %>% filter(layer == "l2VPM_l3GF")
# 
# deGlasgow <- read_indicatorsDatabase("inputs/indicatorsDatabase_2020-05-22.xlsx", "Glasgow")
# deGlasgow
# weightEdges(de, deGlasgow) %>% filter(layer == "l2VPM_l3GF")


# -------------------------------------------------------------------------
# METRICS -----------------------------------------------------------------
# -------------------------------------------------------------------------
calcUWVBC(di, dk)
calcWVBC(di, dk)

di2 <- de2 %>% edgelist_to_igraph(dk); di2

calcChange(before = calcWVBC(di, dk), after = calcWVBC(di2, dk), metric = "WVBC")

# Do we build in that the default weighting is 0.5??


# -------------------------------------------------------------------------
# VISUALISATION -----------------------------------------------------------
# -------------------------------------------------------------------------
key <- read_csv("inputs/visKey.csv", col_types = cols()); key

# Or create using tribble

plot_layout <- vis_layout(edgelist = de, 
                          vInfo = dk, 
                          minSpacing = 0.05, maxSpacing = 0.25, 
                          key = key)
plot_layout

plot_ggplot <- vis_ggplot(plot_layout, key = NULL)
plot_ggplot

plot_plotly <- vis_plotly(plot_ggplot, circles = FALSE)
plot_plotly


# -------------------------------------------------------------------------
# SAVE OUTPUTS ------------------------------------------------------------
# -------------------------------------------------------------------------
saveOutput(dh, "example", type = "adjMat", extension = ".csv")
saveOutput(dh, "example", type = "adjMat", extension = ".RDS")

saveOutput(de, "example", type = "edgelist", extension = ".csv")
saveOutput(de, "example", type = "edgelist", extension = ".RDS")

saveOutput(di, "example", type = "igraph", extension = ".RDS")

calcWVBC(di2, dk) %>% saveOutput("example", type = "metric", extension = ".csv")
calcWVBC(di2, dk) %>% saveOutput("example", type = "metric", extension = ".RDS")

saveOutput(plot_layout, "example_layout", type = "vis", extension = ".RDS")
saveOutput(plot_layout$edges, "example_edgeslayout", type = "vis", extension = ".csv")
saveOutput(plot_layout$vertices, "example_verticeslayout", type = "vis", extension = ".csv")

saveOutput(plot_ggplot, "example", type = "vis", extension = ".png")

saveOutput(plot_plotly, "example", type = "vis", extension = ".html")