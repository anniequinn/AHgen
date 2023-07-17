vis_AH_layout <- function(edgelist, vInfo, minSpacing = 0, maxSpacing = 100, key) { 
  
  require(ggraph)
  
  internal_horizontalLayout <- function(edgelist, vInfo) {
  
  # Add a central dummy vertex
  tmp <- vInfo %>% filter(level == 1)
  
  if(nrow(tmp) > 1) {
    
    edgelist <- 
      tibble(from = "dummyVertex", to = tmp$Node, layer = "dummyLayer", weight = 1) %>% 
      rbind(edgelist)
    
  }
  
  igraph <- edgelist %>% edgelist_to_igraph(vInfo)
  
  # Vertex levels
  if(nrow(tmp) > 1) { levels <- c(1, V(igraph)$level+1) } else { levels <- V(igraph)$level }
  
  # Determine horizontal version of layout based on sugiyama
  layoutSug <- 
    (igraph %>% 
       layout_with_sugiyama(layers = levels))$layout
  colnames(layoutSug) <- c("x", "y")
  
  layoutSug <- 
    layoutSug %>%
    as_tibble %>% 
    mutate(level = y) %>%
    select(level, x, y) %>%
    split(., .$level)

  # Fix an error which can occur
  if(nrow(layoutSug[[1]]) > 1) { 
    
    layoutSug <- rev(layoutSug)
    
    layoutSug <- 
      
      lapply(1:length(layoutSug), function(i) { 
        
        layoutSug[[i]] %>% mutate(level = i, y = i)
      
    })
    
  }
  
  layoutSug <- 
    lapply(1:length(layoutSug), function(i) { 
      
      I = i-1
      
      layoutSug[[i]] %>%
        mutate(x = scales::rescale(x, to = c(-I, I)),
               y = -y,
               pos = 1:n())
      
    }) %>% 
    bind_rows() %>%
    select(level, pos, x, y)
  
  # Remove dummy vertex
  if(nrow(tmp) > 1) { 
    layoutSug <- layoutSug %>% mutate(level = level-1, y = y+1) %>% filter(level != 0)
  }
  
  return(layoutSug)
  
}
  
  internal_horizontalLayout_spacing <- function(horizontalLayout, 
                                                minSpacing = 0.05, 
                                                maxSpacing = 0.25) {
  
  dl <- horizontalLayout %>% split(., .$level)
  
  for(j in 1:length(dl)) {
    
    tmp <- dl[[j]] %>% arrange(x,y)
    
    if(nrow(tmp) > 1) {
      
      
      diffMin = minSpacing
      diffMax = maxSpacing
      
      
      for(i in 1:(nrow(tmp)-1)) {
        
        diff = abs(tmp$x[[i]] - tmp$x[[i + 1]])
        diff
        
        if(diff < diffMin) { tmp$x[-(1:i)] <- tmp$x[-(1:i)] + (diffMin - diff) }
        if(diff > diffMax) { tmp$x[-(1:i)] <- tmp$x[-(1:i)] - (diff - diffMax) }
        
      }
      
    }
    
    dl[[j]] <- tmp
    
  }
  
  dl <- dl %>% bind_rows()
  
  return(dl)
  
}

  
  internal_radialLayout <- function(horizontalLayout, edgelist, key) { 
    
    makeRadial <- 
      function(data_internal, 
               minAngle, 
               maxAngle, 
               radiusChange) { 
        
        # theta, angle in degrees
        min = minAngle
        max = maxAngle
        
        # theta, angle in radians
        min = min * pi / 180
        max = max * pi / 180
        
        # S = R0, arc length is equal to radius multiplied by theta
        minS = min * data_internal$y[[1]]
        maxS = max * data_internal$y[[1]]
        
        # (Optional) Adjustments to radius
        R = radiusChange + data_internal$level[[1]]
        
        # Convert horizontal to radial
        data_internal %>% 
          mutate(x = scales::rescale(x, to = c(minS, maxS))) %>%
          mutate(thetaRad2 = x/y, 
                 x2 = R * cos(thetaRad2), 
                 y2 = R * sin(thetaRad2)) 
        
      }
    
    dl <- horizontalLayout %>% arrange(level, x) %>% split(., .$level)
    
    dl <- 
      
      lapply(1:length(dl), function(i) { 
        
        makeRadial(data_internal = dl[[i]], 
                   minAngle = key$min[[i]], maxAngle = key$max[[i]], 
                   radiusChange = key$addR[[i]])
        
      })
    
    dl %>% 
      bind_rows() %>% 
      arrange(level, pos) %>%
      select(level, pos, x = x2, y = y2, theta = thetaRad2) %>%
      create_layout(edgelist, layout = .)
    
  }
  
  
  internal_genericLayout <- function(radialLayout) { 
    
    require(ggraph)
    
    # Create edge function
    findEdges <- get_edges("short", collapse = "none")
    
    # Extract edge list with layout
    edges <- 
      findEdges(radialLayout) %>% 
      as_tibble %>% 
      select(from.level = node1.level,  to.level = node2.level, layer, 
             from = node1.name, to = node2.name, 
             x = node1.x, y = node1.y, xend = node2.x, yend = node2.y) 
    
    # Create node function (vertices)
    findNodes <- get_nodes()
    
    # Extract vertex list with layout
    vertices <- 
      findNodes(radialLayout) %>% 
      as_tibble %>% 
      select(level, Node = name, x, y, pos, theta)
    
    list(edges = edges, vertices = vertices)
    
  }
  
  output <- 
    internal_horizontalLayout(
      edgelist = edgelist, vInfo = vInfo) %>%
    internal_horizontalLayout_spacing(
      ., minSpacing = minSpacing, maxSpacing = maxSpacing) %>%
    internal_radialLayout(
      horizontalLayout = ., edgelist = edgelist, key = key) %>% 
    internal_genericLayout(
      radialLayout = .)
  
  return(output)
  
}


vis_AH_ggplot <- 
  function(layout, 
           key = NULL,
           vecOpacity = c(1,1,1,1),
           vecSize = c(5,4,3.5,3,2.5)) {
  
  require(gginnards)
  
  # Edges and vertices
    p1 <- 
      suppressWarnings({
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
                         label = Node), 
                     
                     shape = 21, colour = "#34495E") + 
          
          scale_size_manual(values = vecSize, labels = unique(layout$vertices)) +
          scale_fill_viridis_d(labels = unique(layout$vertices), direction = -1)
        
      })
  
  output <- p1
  
  # Circles
  if(!is.null(key)) {
    internal_circleFun <- 
      function(rVec, 
               center = c(0,0), 
               npoints = 100){
      
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


vis_AH_plotly <- 
  function(ggplotPlot, 
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
    ggplotly(ggplotPlot, tooltip = "Node") %>% 
    layout(legend = list(orientation = "h", x = 0, y = 1))
  
  output <- plotly_build(output)
  
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