require(readxl)
require(openxlsx)
require(tidyverse)
require(igraph)
require(tnet)
require(ggraph)

source("functions/function_read_adjMat.R")
source("functions/function_read_vInfo.R")  
source("functions/function_read_weightScenario.R")

source("functions/function_checkSymmetry.R")
source("functions/function_checkRedundancy.R")

source("functions/function_assign_subnetworks.R")
source("functions/function_update_subnetworks.R")

source("functions/function_preview.R")
source("functions/functions_convert.R")
   
source("functions/function_weightEdges.R")

source("functions/functions_calc.R")

source("functions/function_getResults.R")

source("functions/function_summarise_ah.R")

source("functions/function_vis_layout.R")

source("functions/function_saveOutput.R")