export_AH <- function(AH_input, directory = NULL, 
                      name, version, location, scenario) {
  
  filenameRDS <- 
    filenameTimestamp(
      prefix = paste0(
        directory, name, "_", version, "_", location, "_", scenario),
      extension = ".RDS")
  
  AH_input %>% saveRDS(filenameRDS)
  
  message(paste0("Files saved as: "))
  
  message(paste0("\n\t", filenameRDS))
  
  filenameXLSX <- 
    filenameTimestamp(
      prefix = paste0(
        directory, name, "_", version, "_", location, "_", scenario, 
        "_results"),
      extension = ".xlsx")
  
  AH_input$results %>% xlsx::write.xlsx(filenameXLSX)
  
  message(paste0("Files saved as: "))
  
  message(paste0("\n\t", filenameXLSX))
  
}