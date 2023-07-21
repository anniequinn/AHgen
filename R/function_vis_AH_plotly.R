vis_AH_plotly <- function(ggplotPlot, 
                          layerNames = c("Layer 1-2 - Purposes-Outcomes",
                                        "Layer 2-3 - Outcomes-Tasks",
                                        "Layer 3-4 - Tasks-Processes",
                                        "Layer 4-5 - Processes-Resources"),
                          levelNames = c("Level 1 - Purposes",
                                         "Level 2 - Outcomes",
                                         "Level 3 - Tasks", 
                                         "Level 4 - Processes",
                                         "Level 5 - Resources"), 
                          circles = TRUE) {
    
    require(plotly)
    
    # If encountering problems, use development version
    # devtools::install_github("ropensci/plotly")
    
    output <- 
      plotly::ggplotly(ggplotPlot, tooltip = "Node") %>% 
      layout(legend = list(orientation = "v", x = 1, y = 1))
    
    output <- plotly::plotly_build(output)
    
    if(circles == TRUE) {
      
      output$x$data[[2]][8:9] <- layerNames[[1]]
      output$x$data[[3]][8:9] <- layerNames[[2]]
      output$x$data[[4]][8:9] <- layerNames[[3]]
      output$x$data[[5]][8:9] <- layerNames[[4]]
      
      output$x$data[[6]][8:9] <- levelNames[[1]]
      output$x$data[[7]][8:9] <- levelNames[[2]]
      output$x$data[[8]][8:9] <- levelNames[[3]]
      output$x$data[[9]][8:9] <- levelNames[[4]]
      output$x$data[[10]][8:9] <- levelNames[[5]]
      
    }
    
    if(circles == FALSE) {
      
      output$x$data[[1]][8:9] <- layerNames[[1]]
      output$x$data[[2]][8:9] <- layerNames[[2]]
      output$x$data[[3]][8:9] <- layerNames[[3]]
      output$x$data[[4]][8:9] <- layerNames[[4]]
      
      output$x$data[[5]][8:9] <- levelNames[[1]]
      output$x$data[[6]][8:9] <- levelNames[[2]]
      output$x$data[[7]][8:9] <- levelNames[[3]]
      output$x$data[[8]][8:9] <- levelNames[[4]]
      output$x$data[[9]][8:9] <- levelNames[[5]]
      
    }
    
    return(output)
    
  }