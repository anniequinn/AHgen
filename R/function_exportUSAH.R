exportUSAH <- function(USAH, directory = NULL, version, location, scenario) {
  
  filenameRDS <- 
    filenameTimestamp(
      prefix = paste0(
        directory, "USAH_", version, "_", location, "_", scenario),
      extension = ".RDS")
  
  USAH %>% saveRDS(filenameRDS)
  
  message(paste0("Files saved as: "))
  
  message(paste0("\n\t", filenameRDS))
  
  filenameXLSX <- 
    filenameTimestamp(
      prefix = paste0(
        directory, "USAH_", version, "_", location, "_", scenario, "_results"),
      extension = ".xlsx")
  
  USAH$results %>% write.xlsx(filenameXLSX)
  
  message(paste0("Files saved as: "))
  
  message(paste0("\n\t", filenameXLSX))
  
}