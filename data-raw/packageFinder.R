# =========================================================================
# packageFinder.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Created: 2023-07-07

# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2023-07-07
# =========================================================================


# PREP --------------------------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Load tidyverse
library(tidyverse)


# FUNCTION ----------------------------------------------------------------

# https://sebastiansauer.github.io/finds_funs/

find_funs <- function(functionName) {
  # Returns dataframe with two columns:
  # `package_name`: packages(s) which the function is part of (chr)
  # `builtin_package`:  whether the package comes with standard R (a 'builtin'  package)
  
  # Arguments:
  # functionName: name of function for which the package(s) are to be identified.
  
  
  if ("tidyverse" %in% rownames(installed.packages()) == FALSE) {
    cat("tidyverse is needed for this fuction. Please install. Stopping")
    stop()}
  
  suppressMessages(library(tidyverse))
  
  
  # search for help in list of installed packages
  help_installed <- help.search(paste0("^",functionName,"$"), agrep = FALSE)
  
  # extract package name from help file
  pckg_hits <- help_installed$matches[,"Package"]
  
  if (length(pckg_hits) == 0) pckg_hits <- "No_results_found"
  
  
  # get list of built-in packages
  
  pckgs <- installed.packages()  %>% as_tibble
  pckgs %>%
    dplyr::filter(Priority %in% c("base","recommended")) %>%
    dplyr::select(Package) %>%
    distinct -> builtin_pckgs_df
  
  # check for each element of 'pckg hit' whether its built-in and loaded (via match). Then print results.
  
  results <- data_frame(
    functionName = functionName,
    package_name = pckg_hits,
    builtin_pckage = match(pckg_hits, builtin_pckgs_df$Package, nomatch = 0) > 0,
    loaded = match(paste("package:",pckg_hits, sep = ""), search(), nomatch = 0) > 0
  )
  
  return(results)
  
}


# PROCESS -----------------------------------------------------------------

# Read in list of function names and pull into a vector
functionsList = 
  readxl::read_xlsx("functionsTracker_packagesList_20230707.xlsx") %>%
  pull(functions)

# Apply find_funs to list of function names
packagesList = functionsList %>% lapply(find_funs)

# Find number of possible packages for each function name
grp = sapply(packagesList, nrow)

# Separate outputs by number of possible packages
packagesList <- split(packagesList, grp)

# Create and tidy final output
certain <- list()
certain <- packagesList[1] %>% bind_rows

# Manual checks
# If you need to inspect a longer list
# long <- packagesList[8] %>% bind_rows