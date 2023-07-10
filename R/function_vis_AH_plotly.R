vis_AH_plotly <- function(ggplotPlot, 
                          edgeNames = c("Layer 1 - FP to VPM",
                                        "Layer 2 - VPM to GF",
                                        "Layer 3 - GF to ORP",
                                        "Layer 4 - ORP to PO"),
                          nodeNames = paste0("Level ", 1:5), 
                          circles = TRUE) {
    
    require(plotly)
    
    # If encountering problems, use development version
    # devtools::install_github("ropensci/plotly")
    
    output <- 
      plotly::ggplotly(ggplotPlot, tooltip = "Node") %>% 
      layout(legend = list(orientation = "h", x = 0, y = 1))
    
    output <- plotly::plotly_build(output)
    
    if(circles == TRUE) {
      
      output$x$data[[2]][8:9] <- edgeNames[[1]]
      output$x$data[[3]][8:9] <- edgeNames[[2]]
      output$x$data[[4]][8:9] <- edgeNames[[3]]
      output$x$data[[5]][8:9] <- edgeNames[[4]]
      
      output$x$data[[6]][8:9] <- nodeNames[[1]]
      output$x$data[[7]][8:9] <- nodeNames[[2]]
      output$x$data[[8]][8:9] <- nodeNames[[3]]
      output$x$data[[9]][8:9] <- nodeNames[[4]]
      output$x$data[[10]][8:9] <- nodeNames[[5]]
      
    }
    
    if(circles == FALSE) {
      
      output$x$data[[1]][8:9] <- edgeNames[[1]]
      output$x$data[[2]][8:9] <- edgeNames[[2]]
      output$x$data[[3]][8:9] <- edgeNames[[3]]
      output$x$data[[4]][8:9] <- edgeNames[[4]]
      
      output$x$data[[5]][8:9] <- nodeNames[[1]]
      output$x$data[[6]][8:9] <- nodeNames[[2]]
      output$x$data[[7]][8:9] <- nodeNames[[3]]
      output$x$data[[8]][8:9] <- nodeNames[[4]]
      output$x$data[[9]][8:9] <- nodeNames[[5]]
      
    }
    
    return(output)
    
  }