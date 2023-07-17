# Function to apply location-specific indicators 
## Currently works only with USAH_input = USAH_template_baseline, need to make adjustments for any other USAH_input
apply_stressors <- function(USAH_input, 
                            indicators) {
  
  USAH_input$edgelist <- 
    USAH_input$edgelist %>% 
    weightEdges(indicators)
  
  USAH_input$adjMat <- 
    USAH_input$edgelist %>% 
    edgelist_to_adjMat(vInfo = USAH_input$vIncluded)
  
  USAH_input$igraph <- 
    USAH_input$edgelist %>% 
    edgelist_to_igraph(vInfo = USAH_input$vIncluded)
  
  USAH_input$results <- 
    getResults(igraph = USAH_input$igraph, 
               vInfo = USAH_input$vIncluded)
  
  USAH_input$summary <- 
    summarise_USAH(vInfo = USAH_input$vIncluded, 
                   edgelist = USAH_input$edgelist)
  
  return(USAH_input)
  
}