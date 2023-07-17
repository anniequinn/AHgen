export_countCompared <- function(countCompared, directory = NULL, 
                                 version, location, baseline, scenario) {
  
  filenameRDS <- 
    filenameTimestamp(
      prefix = paste0(
        directory, 
        "countCompared_", version, "_", location, "_", baseline, "-", scenario),
      extension = ".RDS")
  
  countCompared %>% saveRDS(filenameRDS)
  
  message(paste0("Files saved as: "))
  
  message(paste0("\n\t", filenameRDS))
  
  filenameXLSX <- 
    filenameTimestamp(
      prefix = paste0(
        directory, 
        "countCompared_", version, "_", location, "_", baseline, "-", scenario),
      extension = ".xlsx")
  
  countCompared %>% export_excel(filenameXLSX)
  
  message(paste0("Files saved as: "))
  
  message(paste0("\n\t", filenameXLSX))
  
}