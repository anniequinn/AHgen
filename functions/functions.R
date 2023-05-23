require(readxl)
require(openxlsx)
require(tidyverse)
require(igraph)
require(tnet)
require(ggraph)

source("functions/function_read_adjMat.R")
source("functions/function_read_vInfo.R")  
source("functions/function_read_indicators.R")

source("functions/function_checkDiff.R")
source("functions/function_checkDuplicates.R")
source("functions/function_checkNames.R")
source("functions/function_checkSymmetry.R")
source("functions/function_checkRedundancy.R")

source("functions/function_assign_subnetworks.R")
source("functions/function_update_subnetworks.R")

source("functions/function_preview.R")
source("functions/functions_convert.R")

source("functions/function_weightEdges.R")

source("functions/function_sbc.R")
source("functions/function_calcDegrees.R")
source("functions/functions_calc.R")

source("functions/function_getResultsNew.R")

source("functions/function_summarise_ah.R")

source("functions/function_weight_hangingVertices.R")

source("functions/function_detect_desc.R")
source("functions/function_apply_geoData.R")

source("functions/function_apply_stressors.R")

source("functions/function_compareOSMtidy.R")
source("functions/function_gen_edgesNew.R")
source("functions/function_apply_scenario.R")

source("functions/function_vis_layout.R")

source("functions/function_saveOutput.R")
source("functions/function_exportExcel.R")
source("functions/function_filenameTimestamp.R")

source("functions/function_compareResults.R")
source("functions/function_getVertices.R")
source("functions/function_getEdges.R")
source("functions/function_getvExcluded.R")
source("functions/function_getRankDegree.R")
source("functions/function_getRankEC.R")
source("functions/function_getRankSBC.R")