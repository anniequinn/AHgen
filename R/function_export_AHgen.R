export_AHgen <- function(output, 
                         type = "AH", 
                         directory = NULL, 
                         AH_name, 
                         AH_version = NULL, 
                         AH_location = NULL, 
                         AH_benchmark = NULL, 
                         AH_scenario = NULL,
                         xl = TRUE) {
  
  require(xlsx)
  
  if(type == "AH"){
    
    prefixRDS <- AH_name
    
    prefixXLSX <- paste0(AH_name, "_results")
    
    outputXLSX <- output$results
    
  }
  
  if(type == "AH_compared"){
    
    prefixRDS <- AH_name
    
    prefixXLSX <- paste0("comparison_", AH_name, "_results")
    
    outputXLSX <- output$results
    
  }
  
  if(type == "USAH"){
    
    prefixRDS <- 
      paste0(directory, "USAH_", AH_version, "_", AH_location, "_", AH_scenario)
    
    prefixXLSX <- 
      paste0(directory, 
             "USAH_", AH_version, "_", AH_location, "_", AH_scenario, "_results")
    
    outputXLSX <- output$results
    
    }
  
  if(type == "USAH_compared"){
    
    prefixRDS <- 
      paste0(directory, "comparison_", 
             "USAH_", AH_version, "_", AH_benchmark, "_", AH_scenario)
    
    prefixXLSX <- 
      paste0(directory, "comparison_", 
             "USAH_", AH_version, "_", AH_benchmark, "_", AH_scenario, "_results")
    
    outputXLSX <- output$results

  }
  
  if(type == "AH_edges"){
    
    prefixRDS <- paste0("edges_", AH_name)
    
    prefixXLSX <- prefixRDS
    
    outputXLSX <- output
    
  }
  
  if(type == "USAH_edges"){
    
    prefixRDS <- 
      paste0(directory, 
             "edges_USAH_", AH_version, "_", AH_location, "_", AH_scenario)
    
    prefixXLSX <- prefixRDS
    
    outputXLSX <- output
    
  }
  
  filenameRDS <- filenameTimestamp(prefix = prefixRDS, extension = ".RDS")
  
  output %>% saveRDS(filenameRDS)
  message(paste0("Files saved as: "))
  message(paste0("\n\t", filenameRDS))
  
  if((nrow(outputXLSX) <= 100000) & (xl == TRUE)) {
    
    filenameXLSX <- filenameTimestamp(prefix = prefixXLSX, extension = ".xlsx")
    
    outputXLSX %>% writexl::write_xlsx(filenameXLSX)
    message(paste0("Files saved as: "))
    message(paste0("\n\t", filenameXLSX))
    
  }
  
}
