saveOutput <- function(output, 
                       name, 
                       type = c("adjMat", "edgelist", "igraph", "metric", "vis"), 
                       extension = c(".RDS", ".csv", ".html", ".png"), sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
 
   filename <- paste0("outputs/", name, sep, type, sep, timestamp, extension)
  
  if(extension == ".RDS") { output %>% saveRDS(filename) }
  
  if(extension == ".csv") { output %>% as_tibble %>% write_csv(filename) }
  
  if(extension == ".html") { 
    require(htmlwidgets)
    filename <- normalizePath(filename, mustWork = FALSE)
    output %>% saveWidget(filename) 
  }
  
  if(extension == ".png") { 
    
    ggsave(filename, output, height = 21, width = 21, units = "cm", dpi = 600) 
    
    }
  
  printout <- paste0("File saved as: ", filename); message(printout)
  
}