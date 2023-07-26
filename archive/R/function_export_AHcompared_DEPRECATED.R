export_AHcompared <- function(AH_compared, directory = NULL, 
                              name, version, benchmark, scenarios) {
  
  filenameRDS <- 
    filenameTimestamp(
      prefix = paste0(
        directory, 
        "comparison_", name, "_", version, "_", benchmark, "-", scenarios),
      extension = ".RDS")
  
  AH_compared %>% saveRDS(filenameRDS)
  
  message(paste0("Files saved as: "))
  
  message(paste0("\n\t", filenameRDS))
  
  filenameXLSX <- 
    filenameTimestamp(
      prefix = paste0(
        directory, 
        "comparison_", name, "_", version, "_", benchmark, "-to-", scenarios),
      extension = ".xlsx")
  
  AH_compared$results %>% xlsx::write.xlsx(filenameXLSX)
  
  message(paste0("Files saved as: "))
  
  message(paste0("\n\t", filenameXLSX))
  
}