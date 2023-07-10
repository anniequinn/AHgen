export_AHgen <- function(output, type = "AH", directory = NULL, name, version = NULL, 
                         location = NULL, benchmark = NULL, scenario = NULL) {
  
  require(xlsx)
  
  if(type == "AH"){
    
    prefixRDS <- name
    
    prefixXLSX <- paste0(name, "_results")
    
    outputXLSX <- output$results
    
  }
  
  if(type == "AH_compared"){
    
    prefixRDS <- name
    
    prefixXLSX <- paste0("comparison_", name, "_results")
    
    outputXLSX <- output$results
    
  }
  
  if(type == "USAH"){
    
    prefixRDS <- 
      paste0(directory, name, "_", version, "_", location, "_", scenario)
    
    prefixXLSX <- 
      paste0(directory, name, "_", version, "_", location, "_", scenario, "_results")
    
    outputXLSX <- output$results
    
    }
  
  if(type == "USAH_compared"){
    
    prefixRDS <- 
      paste0(directory, "comparison_", name, "_", version, "_", benchmark, "_", scenario)
    
    prefixXLSX <- 
      paste0(directory, "comparison_", name, "_", version, "_", benchmark, "_", scenario, "_results")
    
    outputXLSX <- output$results

  }
  
  if(type == "AH_edges"){
    
    prefixRDS <- paste0("edges_", name)
    
    prefixXLSX <- prefixRDS
    
    outputXLSX <- output
    
  }
  
  if(type == "USAH_edges"){
    
    prefixRDS <- 
      paste0(directory, "edges_", name, "_", version, "_", location, "_", scenario)
    
    prefixXLSX <- prefixRDS
    
    outputXLSX <- output
    
  }
  
  filenameRDS <- filenameTimestamp(prefix = prefixRDS, extension = ".RDS")
  
  output %>% saveRDS(filenameRDS)
  message(paste0("Files saved as: "))
  message(paste0("\n\t", filenameRDS))
  
  filenameXLSX <- filenameTimestamp(prefix = prefixXLSX, extension = ".xlsx")
  
  outputXLSX %>% xlsx::write.xlsx(filenameXLSX)
  message(paste0("Files saved as: "))
  message(paste0("\n\t", filenameXLSX))
  
}