exportEdges <- function(edges, directory = NULL, 
                        name, version, location, scenario) {
  
  filenameRDS <- 
    filenameTimestamp(
      prefix = paste0(
        directory, "edges_", name, "_", version, "_", location, "_", scenario),
      extension = ".RDS")
  
  edges %>% saveRDS(filenameRDS)
  
  message(paste0("Files saved as: "))
  
  message(paste0("\n\t", filenameRDS))
  
  filenameXLSX <- 
    filenameTimestamp(
      prefix = paste0(
        directory, "edges_", name, "_", version, "_", location, "_", scenario, 
        "_results"),
      extension = ".xlsx")
  
  edges %>% write.xlsx(filenameXLSX)
  
  message(paste0("Files saved as: "))
  
  message(paste0("\n\t", filenameXLSX))
  
}