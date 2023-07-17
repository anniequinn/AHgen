# -------------------------------------------------------------------------
# Annie Visser-Quinn
#
# Created: 2020-10-21
# Last revised: 2020-10-21
# -------------------------------------------------------------------------

rm(list = ls()); cat("\014")

pacman::p_load(tidyverse, igraph, data.table, tnet, readxl)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("functions/functions.R")

dh_mic <- "inputs/AH_microwave.xlsx" %>% read_adjMat; dh_mic
dv_mic <- "inputs/AH_microwave.xlsx" %>% read_vInfo(sheet = 2); dv_mic
de_mic <- dh_mic %>% adjMat_to_edgelist(dv_mic); de_mic

dh_mob <- "inputs/AH_mobility.xlsx" %>% read_adjMat; dh_mob
dv_mob <- dh_mob %>% select(level, levelName, vName); dv_mob
de_mob <- dh_mob %>% adjMat_to_edgelist(dv_mob); de_mob

dh_USAH <- "inputs/USAH_adjMat_2020-05-20.xlsx" %>% read_adjMat; dh_USAH
dv_USAH <- dh_USAH %>% select(level, levelName, vName); dv_USAH
de_USAH <- dh_USAH %>% adjMat_to_edgelist(dv_USAH); de_USAH


# VIS
key <- read_csv("inputs/visKey.csv", col_types = cols()); key

layout_mic <- vis_layout(edgelist = de_mic, vInfo = dv_mic, key = key); layout_mic
(layout_mic %>% vis_ggplot(key = key)) + 
  geom_text(data = layout_mic$vertices, aes(label = vName, x = x, y = y, colour = factor(level))) + 
  scale_colour_viridis_d(direction = -1)

layout_mob <- vis_layout(edgelist = de_mob, vInfo = dv_mob, key = key)
(layout_mob %>% vis_ggplot(key = key)) + 
  geom_text(data = layout_mob$vertices, aes(label = vName, x = x, y = y, colour = factor(level))) + 
  scale_colour_viridis_d(direction = -1)

layout_USAH <- vis_layout(edgelist = de_USAH, vInfo = dv_USAH, key = key)
layout_USAH %>% vis_ggplot(key = key)

# Errors in mob; No errors in mic; Can't generate USAH

source("functions/functions_internal_vis_layout.R")

# visLayout <- 
#   function(edgelist, vInfo, minSpacing = 0, maxSpacing = 100, key) { 
#     internal_horizontalLayout(edgelist = edgelist, vInfo = vInfo) %>%
#       internal_horizontalLayout_spacing(., minSpacing = minSpacing, maxSpacing = maxSpacing) %>%
#       internal_radialLayout(horizontalLayout = ., edgelist = edgelist, key = key) %>% 
#       internal_genericLayout(radialLayout = .)
#   }

dv_mic %>% cbind(internal_horizontalLayout(de_mic, dv_mic))
dv_mob %>% cbind(internal_horizontalLayout(de_mob, dv_mob))
dv_USAH %>% cbind(internal_horizontalLayout(de_USAH, dv_USAH))

dv_mic %>% cbind(internal_horizontalLayout(de_mic, dv_mic) %>% internal_horizontalLayout_spacing())
dv_mob %>% cbind(internal_horizontalLayout(de_mob, dv_mob) %>% internal_horizontalLayout_spacing())

dv_mic %>% setNames(c("level2", "levelName2", "vName2")) %>%
  cbind(internal_horizontalLayout(de_mic, dv_mic) %>% internal_horizontalLayout_spacing() %>% internal_radialLayout(de_mic, key)) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_label(aes(label = vName2, colour = factor(level2))) +
  geom_point(aes(colour = factor(level)), size = 4)

dv_mob %>% setNames(c("level2", "levelName2", "vName2")) %>%
  cbind(internal_horizontalLayout(de_mob, dv_mob) %>% internal_horizontalLayout_spacing() %>% internal_radialLayout(de_mob, key)) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_label(aes(label = vName2, colour = factor(level2))) +
  geom_point(aes(colour = factor(level)), size = 4)

dv_mob %>% setNames(c("level2", "levelName2", "vName2")) %>%
  cbind(internal_horizontalLayout(de_mob, dv_mob)) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_label(aes(label = vName2, colour = factor(level2))) +
  geom_point(aes(colour = factor(level)), size = 4)


#############

edgelist <- de_USAH; edgelist
vInfo <- dv_USAH; vInfo

