plotViolin <- function(results, metricName, type = "half", 
                       levels = NULL, family = "Harding") {
  
  # yaxisLab prep
  if(metricName == "EC")
  {yaxisLab_metric <- "Eigenvector Centrality"}
  if(metricName == "SBC_norm")
  {yaxisLab_metric <- "Stable Betweenness Centrality"}
  yaxisLab <- paste0(yaxisLab_metric, " Value")
  
  # default levels
  if(metricName == "EC" & is.null(levels))
  {levels <- c("Purposes", "Outcomes", "Tasks")}
  if(metricName == "SBC_norm" & is.null(levels))
  {levels <- c("Tasks", "Processes", "Resources")}
  
  # Results data frame prep
  results <-
    results %>%
    filter(metric == metricName, levelName %in% levels)
  
  if(type == "half"){
    
    # !!! NOTE !!! Still no idea why, but colsBaseline & colsFloodRiver200 needed
    # to be swapped to create accurate visualisation
    
    violinPlot <-
      ggplot(data = results) +
      aes(x = levelName, y = value, fill = scenario) +
      see::geom_violinhalf(
        data = results %>% filter(scenario == "baseline"), 
        alpha = 0.7, aes(fill = colsFloodRiver200), flip = TRUE, linewidth = 0.1) + # using default colours from AHgen package; flipped
      see::geom_violinhalf(
        data = results %>% filter(scenario == "1 in 100-year flood"), 
        alpha = 0.7, aes(fill = colsFloodRiver100), linewidth = 0.1) + # using default colours from AHgen package
      see::geom_violinhalf(
        data = results %>% filter(scenario == "1 in 200-year flood"), 
        alpha = 0.7, aes(fill = colsBaseline), linewidth = 0.1) + # using default colours from AHgen package
      facet_grid2(
        independent = "x", levelName ~ location, 
        labeller = label_wrap_gen(width = 15), scales = "free", switch = "y") +
      labs(y = yaxisLab) +
      scale_fill_manual(name = "scenario", 
                        values = levels(results$cols), 
                        labels = levels(results$scenario)) +
      scale_y_continuous(limits = c(0, NA), position = "right") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(color = "grey40", size = 24),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_line(color = "grey90", linewidth = 0.2),
            axis.title.x = element_blank(),
            axis.title.y = element_text(color = "grey40", size = 32),
            legend.box.margin = margin(-4, -4, -4, -4),
            legend.key.size = unit(0.4, "line"),
            legend.justification = "center",
            legend.margin = margin(c(2, 0, 4, 0)),
            legend.position = "bottom",
            legend.spacing.x = unit(1.4, 'mm'),
            legend.text = element_text(color = "grey40", size = 32),
            legend.title = element_text(color = "grey40", size = 36),
            panel.background = element_rect(color = "gray85", fill = "gray98"),
            panel.border = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray90", linewidth = 0.1),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.spacing = unit(0.3, "lines"),
            plot.background = element_rect(fill = "white"),
            plot.margin = grid::unit(c(2, 4, 2, 0), "mm"),
            plot.title = element_blank(),
            strip.background = element_rect(fill = "white"),
            strip.text.x = element_text(color = "grey25", lineheight = 2, size = 34),
            strip.text.y.left = element_text(
              angle = 0, color = "grey25", lineheight = 0.15, size = 34),
            text = element_text(family = family))
    
  } else if(type == "overlap") {
    
    violinPlot <-
      ggplot(data = results) +
      aes(x = levelName, y = value, color = scenario) +
      geom_violin(alpha = 0.1, position = "identity", linewidth = 0.2) +
      facet_grid2(
        independent = "x", levelName ~ location, 
        labeller = label_wrap_gen(width = 15), scales = "free", switch = "y") +
      labs(y = yaxisLab) +
      scale_color_manual(name = "scenario", 
                         values = levels(results$cols), 
                         labels = levels(results$scenario)) + # using default colours from AHgen package
      scale_y_continuous(limits = c(0, NA), position = "right") +
      guides(color = guide_legend(override.aes = list(linewidth = 2))) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(color = "grey40", size = 24),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_line(color = "grey90", linewidth = 0.2),
            axis.title.x = element_blank(),
            axis.title.y = element_text(color = "grey40", size = 32),
            legend.box.margin = margin(-4, -4, -4, -4),
            legend.key.size = unit(0.4, "line"),
            legend.justification = "center",
            legend.margin = margin(c(2, 0, 4, 0)),
            legend.position = "bottom",
            legend.spacing.x = unit(1.4, 'mm'),
            legend.text = element_text(color = "grey25", size = 32),
            legend.title = element_text(color = "grey25", size = 36),
            panel.background = element_rect(color = "gray85", fill = "gray98"),
            panel.border = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray90", linewidth = 0.1),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.spacing = unit(0.3, "lines"),
            plot.background = element_rect(fill = "white"),
            plot.margin = grid::unit(c(2, 4, 2, 0), "mm"),
            plot.title = element_blank(),
            strip.background = element_rect(fill = "white"),
            strip.text.x = element_text(color = "grey25", lineheight = 2, size = 34),
            strip.text.y.left = element_text(
              angle = 0, color = "grey25", lineheight = 0.15, size = 34),
            text = element_text(family = family))
    
  }
  
  return(violinPlot)
  
}