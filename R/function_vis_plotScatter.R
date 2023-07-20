vis_plotScatter <- function(results, 
                            AH_benchmark = "baseline", 
                            metricName, 
                            type, 
                            levels = NULL, 
                            locations = NULL, 
                            omit.zeros = TRUE, 
                            omit.inf = TRUE,
                            family) { 
  
  require(ggplot2)
  require(ggh4x)
  require(forcats)
  require(tidytext)
  require(grid)
  
  # currently by = "location"
  # potential future adaptaion: by = "scenario" (e.g. Edinburgh COVID weeks) flip the scenario & location
  
  # Size aesthetics prep
  results <- 
    results %>%
    mutate(levelScenario = 
             case_when(level == 1 & scenario == AH_benchmark ~ 1,
                       level == 1 & scenario != AH_benchmark ~ 2,
                       level == 2 & scenario == AH_benchmark ~ 3,
                       level == 2 & scenario != AH_benchmark ~ 4,
                       level == 3 & scenario == AH_benchmark ~ 5,
                       level == 3 & scenario != AH_benchmark ~ 6,
                       level == 4 & scenario == AH_benchmark ~ 7,
                       level == 4 & scenario != AH_benchmark ~ 8,
                       level == 5 & scenario == AH_benchmark ~ 9,
                       level == 5 & scenario != AH_benchmark ~ 10)) %>%
    mutate(sizes = 
             case_when(level == 1 & scenario == AH_benchmark ~ 0.95,
                       level == 1 & scenario != AH_benchmark ~ 0.9,
                       level == 2 & scenario == AH_benchmark ~ 0.7,
                       level == 2 & scenario != AH_benchmark ~ 0.75,
                       level == 3 & scenario == AH_benchmark ~ 0.6,
                       level == 3 & scenario != AH_benchmark ~ 0.65,
                       level == 4 & scenario == AH_benchmark ~ 0.3,
                       level == 4 & scenario != AH_benchmark ~ 0.35,
                       level == 5 & scenario == AH_benchmark ~ 0.3,
                       level == 5 & scenario != AH_benchmark ~ 0.35))
  
  # Shape aesthetics prep
  shapeBaseline <- 21
  n <- 
    results %>% 
    filter(scenario != AH_benchmark) %>% select(scenario) %>% unique() %>% nrow()
  shapeScenarios <- rep(19, n)
  
  # yaxisLab prep
  if(metricName == "EC")
  {yaxisLab_metric <- "Eigenvector Centrality"}
  if(metricName == "SBC_norm")
  {yaxisLab_metric <- "Stable Betweenness Centrality"}
  
  # Specifying data to plot
  
  if(type == "values"){
    
    if(omit.zeros == TRUE) {results <- results %>% filter(value_amp != 0)}
    
    values <- "value_amp"
    
    shapesAll <- c(shapeBaseline, shapeScenarios)
    
    yaxisLab <- paste0(yaxisLab_metric, " Value")
    
  }
  
  if(type == "valuesChange"){
    
    results <- 
      results %>% 
      filter(scenario != AH_benchmark) %>% 
      mutate(scenario = forcats::fct_inorder(scenario))
    
    if(omit.zeros == TRUE) {results <- results %>% filter(change_value_amp != 0)}
    
    if(omit.inf == TRUE) {results <- results %>% filter(change_value_amp != Inf)}
    
    values <- "change_value_amp"
    
    shapesAll <- shapeScenarios
    
    yaxisLab <- paste0("Change in ", yaxisLab_metric, " Value")
    
  }
  
  if(type == "percentChange"){
    
    results <- 
      results %>% 
      filter(scenario != AH_benchmark) %>% 
      mutate(scenario = forcats::fct_inorder(scenario))
    
    if(omit.zeros == TRUE) {results <- results %>% filter(change_pct != 0)}
    
    if(omit.inf == TRUE) {results <- results %>% filter(change_pct != Inf)}
    
    values <- "change_pct"
    
    shapesAll <- shapeScenarios
    
    yaxisLab <- paste0("% Change in ", yaxisLab_metric, " Value")
    
  }
  
  if(metricName == "EC" & is.null(levels))
  {levels <- c("Purposes", "Outcomes", "Tasks")}
  if(metricName == "SBC_norm" & is.null(levels))
  {levels <- c("Tasks", "Processes", "Resources")}
  
  if(is.null(locations))
  {locations <- results %>% select(location) %>% unique() %>% pull}
  
  
  # Define different ggplot approaches
  
  if((length(levels) > 1) & (length(locations) > 1)) {
    
    results <-
      results %>%
      filter(metric == metricName, levelName %in% levels) %>%
      arrange(levelName) %>%
      mutate(levelName = forcats::fct_inorder(levelName)) %>%
      arrange(cols) %>%
      mutate(cols = forcats::fct_inorder(cols)) %>%
      arrange(levelScenario)
    
    scatterPlot <-
      results %>%
      ggplot(aes(
        x = tidytext::reorder_within(Node, -benchmark_value_amp, location), 
        y = !!sym(values))) +
      geom_hline(yintercept = 0, color = "gray85", linewidth = 0.4) +
      geom_point(aes(shape = scenario, color = scenario), 
                 size = results$sizes, stroke = 0.2) +
      tidytext::scale_x_reordered() +
      ggh4x::facet_grid2(independent = "x", levelName ~ location, 
                         labeller = label_wrap_gen(width = 15), 
                         scales = "free", switch = "y") +
      labs(y = yaxisLab) +
      scale_color_manual(values = levels(results$cols)) +
      scale_shape_manual(values = shapesAll) +
      scale_y_continuous(position = "right") +
      theme_void(base_size = 42) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey40", size = 28),
        axis.ticks.length = unit(1, "mm"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = 
          element_text(angle = 90, color = "grey40", size = 34, vjust = -5),
        legend.box.margin = margin(-4, -4, -4, -4),
        legend.key.size = unit(0.5, "line"),
        legend.justification = "center",
        legend.margin = margin(c(6, 0, 2, 0)),
        legend.position = "bottom",
        legend.spacing.x = unit(0.6, 'mm'),
        legend.text = element_text(color = "grey25", size = 32),
        legend.title = element_text(color = "grey25", size = 38),
        panel.background = element_rect(color = "gray85", fill = "gray98"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(color = "gray90", linewidth = 0.1),
        panel.spacing = unit(0.3, "lines"),
        plot.background = element_rect(color = "transparent", fill = "white"),
        plot.margin = grid::unit(c(2, 5, 2, 0), "mm"),
        strip.text.x = 
          element_text(color = "grey25", lineheight = 1,
                       margin = margin(0.2, 0, 0.2, 0, "cm"), size = 40),
        strip.text.y.left = 
          element_text(angle = 0, color = "grey25", lineheight = 0.15, 
                       margin = margin(0, 0.2, 0, 0.2, "cm"), size = 38),
        text = element_text(family = family))
    
  } else if((length(levels) == 1) & (length(locations) > 1)){
    
    results <-
      results %>%
      filter(metric == metricName, levelName %in% levels, location %in% locations) %>%
      arrange(levelName) %>%
      mutate(levelName = forcats::fct_inorder(levelName)) %>%
      arrange(cols) %>%
      mutate(cols = forcats::fct_inorder(cols)) %>%
      arrange(levelScenario)
    
    titleLab <- levels
    
    scatterPlot <-
      results %>%
      filter(metric == metricName, levelName %in% levels) %>%
      ggplot(aes(
        x = tidytext::reorder_within(Node, -benchmark_value_amp, location), 
        y = !!sym(values))) +
      geom_point(aes(shape = scenario, color = scenario), 
                 size = results$sizes, stroke = 0.2) +
      facet_wrap(~location, labeller = label_wrap_gen(width = 15), 
                 ncol = 3, scales = "free") +
      tidytext::scale_x_reordered() +
      labs(title = titleLab, y = yaxisLab) +
      scale_color_manual(values = levels(results$cols)) +
      scale_shape_manual(values = shapesAll) +
      theme_void(base_size = 42) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey40",size = 32),
        axis.ticks.length = unit(1, "mm"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 90, color = "grey40", size = 38, vjust = 7),
        legend.box.margin = margin(-4, -4, -4, -4),
        legend.key.size = unit(0.5, "line"),
        legend.justification = "center",
        legend.margin = margin(c(6, 0, 2, 0)),
        legend.position = "bottom",
        legend.spacing.x = unit(0.1, 'mm'),
        legend.text = element_text(color = "grey25", size = 38),
        legend.title = element_text(color = "grey25", size = 40),
        panel.background = element_rect(fill='white', colour='white'),
        panel.grid.major.y = element_line(color = "gray90"), 
        panel.grid.minor.y = element_line(color = "gray95"),
        panel.spacing = unit(0.2, "lines"),
        plot.background = element_rect(fill='white', colour='white'),
        plot.margin = grid::unit(c(2, 4, 4, 6), "mm"),
        plot.title = element_text(color = "grey25", size = 48, vjust = 6),
        strip.text.x = 
          element_text(color = "grey25",lineheight = 1,
                       margin = margin(0.1, 0, 0.1, 0, "cm"), size = 40),
        text = element_text(family = family))
    
  } else if((length(levels) == 1) & (length(locations) == 1)) {
    
    results <-
      results %>%
      filter(metric == metricName, levelName %in% levels, location %in% locations) %>%
      arrange(levelName) %>%
      mutate(levelName = forcats::fct_inorder(levelName)) %>%
      arrange(cols) %>%
      mutate(cols = forcats::fct_inorder(cols)) %>%
      arrange(levelScenario)
    
    titleLab <- paste0(locations, " ", levels)
    
    scatterPlot <-
      results %>%
      filter(metric == metricName, levelName %in% levels) %>%
      ggplot(aes(
        x = tidytext::reorder_within(Node, -benchmark_value_amp, location), 
        y = !!sym(values))) +
      geom_point(aes(shape = scenario, color = scenario), 
                 size = 1.2, stroke = 0.4) +
      tidytext::scale_x_reordered() +
      labs(title = titleLab, y = yaxisLab) +
      scale_color_manual(values = levels(results$cols)) +
      scale_shape_manual(values = shapesAll) +
      theme_void(base_size = 42) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey25",size = 40),
        axis.ticks.length = unit(1, "mm"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 90, color = "grey25", size = 44, vjust = 7),
        legend.box.margin = margin(-4, -4, -4, -4),
        legend.key.size = unit(0.5, "line"),
        legend.justification = "center",
        legend.margin = margin(c(6, 0, 2, 0)),
        legend.position = "bottom",
        legend.spacing.x = unit(0.1, 'mm'),
        legend.text = element_text(color = "grey25", size = 40),
        legend.title = element_text(color = "grey25", size = 46),
        panel.background = element_rect(fill='white', colour='white'),
        panel.grid.major.y = element_line(color = "gray90"), 
        panel.grid.minor.y = element_line(color = "gray95"),
        panel.spacing = unit(0.2, "lines"),
        plot.background = element_rect(fill='white', colour='white'),
        plot.margin = grid::unit(c(0, 4, 4, 6), "mm"),
        plot.title = element_text(color = "grey25", size = 54, vjust = 6),
        text = element_text(family = family))
    
  }
  
  return(scatterPlot)
  
}