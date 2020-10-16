vis_layout <- function(edgelist, vInfo, minSpacing = 0, maxSpacing = 100, key) { 
  
  require(ggraph)
  
  source("functions/functions_internal_vis_layout.R", local = TRUE)
  
  output <- 
    internal_horizontalLayout(edgelist = edgelist, vInfo = vInfo) %>%
    internal_horizontalLayout_spacing(., minSpacing = minSpacing, maxSpacing = maxSpacing) %>%
    internal_radialLayout(horizontalLayout = ., edgelist = edgelist, key = key) %>% 
    internal_genericLayout(radialLayout = .)
  
  return(output)
  
}

vis_ggplot <- function(layout, key = NULL,
                       vecOpacity = c(1,1,1,1),
                       vecSize = c(5,4,3.5,3,2.5)) {
  
  require(gginnards)
  
  # Edges and vertices
  p1 <- suppressWarnings({
    layout$edges %>% 
      arrange(desc(layer)) %>%
      ggplot() + 
      geom_segment(aes(x = x, xend = xend, y = y, yend = yend, 
                       alpha = layer), 
                   size = 0.3, colour = "#34495E",
                   show.legend = FALSE) + 
      
      scale_alpha_manual(values = vecOpacity,
                         labels = { 
                           layout$edges %>% 
                             arrange(desc(layer)) %>% 
                             group_by(layer) %>% 
                             summarise %>% 
                             select(layer) %>% 
                             unlist %>% 
                             as.vector
                         }
      ) +
      
      geom_point(data = layout$vertices %>% mutate(level = fct_inorder(as.character(level), ordered = TRUE)), 
                 aes(x = x, y = y,
                     size = level,
                     fill = level,
                     
                     # Warning suppressed, need to add this for plotly interaction
                     label = vName), 
                 
                 shape = 21, colour = "#34495E") + 
      
      scale_size_manual(values = vecSize, labels = unique(layout$vertices)) +
      scale_fill_viridis_d(labels = unique(layout$vertices), direction = -1)
    
  })
  
  output <- p1
  
  # Circles
  if(!is.null(key)) {
    internal_circleFun <- function(rVec, center = c(0,0), npoints = 100){
      
      tt <- seq(0, 2 * pi, length.out = npoints)
      
      lapply(rVec, function(r) { 
        
        # Circle for plotting as geom_path
        
        xx <- center[1] + r * cos(tt)
        yy <- center[2] + r * sin(tt)
        
        tibble(r = r, x = xx, y = yy)
        
      }) %>% 
        bind_rows()
      
    }
    
    p2 <- 
      ggplot() + 
      geom_path(data = internal_circleFun(key$r) %>% filter(y < 0), 
                aes(x = x, y = y, group = r), 
                size = 0.25, alpha = 1, colour = "#D6DBDF")
    
    p2b <- extract_layers(p2, "GeomPath")
    
    output <- output %>% append_layers(p2b, position = "bottom")
    
  }
  
  # Aesthetics
  output <- 
    output +
    
    theme_void() + 
    theme(panel.grid = element_blank(), # Manually added for ggplotly
          axis.line = element_blank(), # Manually added for ggplotly
          legend.position = "top") +
    coord_fixed() +
    
    guides(fill = guide_legend(title.position = "top", hjust = 0.5, 
                               override.aes = list(size = vecSize)), 
           size = FALSE,
           alpha = guide_legend(title.position = "top", hjust = 0.5)) +
    labs(fill = "AH level: ", size = "AH level:")
  
  return(output)
  
}

vis_plotly <- function(ggplotPlot, 
                       edgeNames = c("Layer 1 - FP to VPM",
                                     "Layer 2 - VPM to GF",
                                     "Layer 3 - GF to ORP",
                                     "Layer 4 - ORP to PO"),
                       vNames = paste0("Level ", 1:5),
                       circles = TRUE) {
  
  require(plotly)
  
  # If encountering problems, use development version
  # devtools::install_github("ropensci/plotly")
  
  
  
  output <- 
    ggplotly(ggplotPlot, tooltip = "vName") %>% 
    layout(legend = list(orientation = "h", x = 0, y = 1))
  
  output <- plotly_build(output)
  
  if(circles == TRUE) {
    
    output$x$data[[2]][8:9] <- edgeNames[[1]]
    output$x$data[[3]][8:9] <- edgeNames[[2]]
    output$x$data[[4]][8:9] <- edgeNames[[3]]
    output$x$data[[5]][8:9] <- edgeNames[[4]]
    
    output$x$data[[6]][8:9] <- vNames[[1]]
    output$x$data[[7]][8:9] <- vNames[[2]]
    output$x$data[[8]][8:9] <- vNames[[3]]
    output$x$data[[9]][8:9] <- vNames[[4]]
    output$x$data[[10]][8:9] <- vNames[[5]]
    
  }
  
  if(circles == FALSE) {
    
    output$x$data[[1]][8:9] <- edgeNames[[1]]
    output$x$data[[2]][8:9] <- edgeNames[[2]]
    output$x$data[[3]][8:9] <- edgeNames[[3]]
    output$x$data[[4]][8:9] <- edgeNames[[4]]
    
    output$x$data[[5]][8:9] <- vNames[[1]]
    output$x$data[[6]][8:9] <- vNames[[2]]
    output$x$data[[7]][8:9] <- vNames[[3]]
    output$x$data[[8]][8:9] <- vNames[[4]]
    output$x$data[[9]][8:9] <- vNames[[5]]
    
  }
  
  return(output)
  
}