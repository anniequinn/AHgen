vis_plotConfidence <- function(results, metricName, type, family = "Harding") {
  
  require(ggplot2)
  require(ggh4x)
  require(grid)
  
  if(metricName == "EC" & type == "scenarioLevel") {
    results <- 
      results$confidence$confidence_scenarioLevel_EC %>%
      dplyr::mutate(locationScenario = paste0(location, "\n", scenario))
    titleLab <- "Confidence Ratings for Eigenvector Centrality Rank"
    subtitleLab <- "by Location-Scenario and Level"
  }
  if(metricName == "SBC_norm" & type == "scenarioLevel") {
    results <- 
      results$confidence$confidence_scenarioLevel_SBCnorm %>%
      dplyr::mutate(locationScenario = paste0(location, "\n", scenario))
    titleLab <- "Confidence Ratings for Stable Betweenness Centrality Rank"
    subtitleLab <- "by Location-Scenario and Level"
  }
  if(metricName == "EC" & type == "scenario"){
    results <- 
      results$confidence$confidence_scenario_EC %>%
      dplyr::mutate(locationScenario = paste0(location, "\n", scenario))
    titleLab <- "Confidence Ratings for Eigenvector Centrality Rank"
    subtitleLab <- "by Location-Scenario (across Purposes, Outcomes, & Tasks)"
  }
  if(metricName == "SBC_norm" & type == "scenario"){
    results <- 
      results$confidence$confidence_scenario_SBCnorm %>%
      dplyr::mutate(locationScenario = paste0(location, "\n", scenario))
    titleLab <- "Confidence Ratings for Stable Betweenness Centrality Rank"
    subtitleLab <- "by Location-Scenario (across Tasks, Processes, & Resources)"
  }
  if(metricName == "EC" & type == "level"){
    results <- results$confidence$confidence_level_EC
    titleLab <- "Confidence Ratings for Eigenvector Centrality Rank"
    subtitleLab <- "by Level (across all Location-Scenarios)"
  }
  if(metricName == "SBC_norm" & type == "level"){
    results <- results$confidence$confidence_level_SBCnorm
    titleLab <- "Confidence Ratings for Stable Betweenness Centrality Rank"
    subtitleLab <- "by Level (across all Location-Scenarios)"
  }
  if(metricName == "EC" & type == "overall"){
    results <- results$confidence$confidence_overall_EC
    titleLab <- "Confidence Ratings for Eigenvector Centrality Rank"
    subtitleLab <- "Overall (across Purposes, Outcomes, & Tasks)"
  }
  if(metricName == "SBC_norm" & type == "overall"){
    results <- results$confidence$confidence_overall_SBCnorm
    titleLab <- "Confidence Ratings for Stable Betweenness Centrality Rank"
    subtitleLab <- "Overall (across Tasks, Processes, & Resources)"
  }
  
  if(type == "scenarioLevel"){
    
    confidencePlot <-
      ggplot(results, aes(x = locationScenario, 
                          y = pct_scenarioLevel,
                          fill = confidence_rankByLevel_minusPlus)) +
      geom_bar(position = 'stack', stat = 'identity') +
      ggh4x::facet_grid2(
        independent = "x", levelName ~ locationScenario, 
        labeller = label_wrap_gen(width = 18), scales = "free", switch = "y") +
      scale_fill_viridis_d(name = "confidence", alpha = 0.7) +
      tidytext::scale_x_reordered() +
      scale_y_continuous(
        breaks = seq(0, 100, 20), minor_breaks = seq(10, 90, 10), position = "right") +
      labs(title = titleLab, subtitle = subtitleLab, y = "Percent") +
      theme_void(base_size = 42) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey25", size = 24),
        axis.ticks.length = unit(1, "mm"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 270, color = "grey25", size = 36, vjust = 3),
        legend.box.margin = margin(-4, -4, -4, -4),
        legend.key.size = unit(0.5, "line"),
        legend.justification = "center",
        legend.margin = margin(c(2, 0, 2, 0)),
        legend.position = "bottom",
        legend.spacing.x = unit(0.2, 'mm'),
        legend.text = element_text(color = "grey25", size = 30),
        legend.title = element_text(color = "grey25", size = 34),
        panel.background = element_rect(fill='white', colour='white'),
        panel.grid.major.y = element_line(color = "gray70"), 
        panel.grid.minor.y = element_line(color = "gray85"),
        panel.spacing = unit(0.2, "mm"),
        plot.background = element_rect(fill='white', colour='white'),
        plot.margin = grid::unit(c(4, 4, 2, 3), "mm"),
        plot.subtitle = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 34),
        plot.title = element_text(margin = margin(0, 0, 0.1, 0), size = 38),
        strip.text.x = element_text(
          color = "grey25", lineheight = 0.15, 
          margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), size = 32, vjust = -1),
        strip.text.y = element_text(color = "grey25", size = 34),
        text = element_text(family = family))
  }
  
  if(type == "scenario"){
    
    confidencePlot <-
      ggplot(results, aes(x = locationScenario, 
                          y = pct_scenario,
                          fill = confidence_rankByLevel_minusPlus)) +
      geom_bar(position = 'stack', stat = 'identity') +
      ggh4x::facet_grid2(
        independent = "x", metric ~ locationScenario, 
        labeller = label_wrap_gen(width = 18), scales = "free", switch = "y") +
      scale_fill_viridis_d(name = "confidence", alpha = 0.7) +
      tidytext::scale_x_reordered() +
      scale_y_continuous(
        breaks = seq(0, 100, 20), minor_breaks = seq(10, 90, 10), position = "right") +
      labs(title = titleLab, subtitle = subtitleLab, y = "Percent") +
      theme_void(base_size = 42) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey25", size = 24),
        axis.ticks.length = unit(1, "mm"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 270, color = "grey25", size = 34, vjust = 3),
        legend.box.margin = margin(-4, -4, -4, -4),
        legend.key.size = unit(0.5, "line"),
        legend.justification = "center",
        legend.margin = margin(c(2, 0, 2, 0)),
        legend.position = "bottom",
        legend.spacing.x = unit(0.2, 'mm'),
        legend.text = element_text(color = "grey25", size = 34),
        legend.title = element_text(color = "grey25", size = 36),
        panel.background = element_rect(fill='white', colour='white'),
        panel.grid.major.y = element_line(color = "gray70"), 
        panel.grid.minor.y = element_line(color = "gray85"),
        panel.spacing = unit(0.2, "lines"),
        plot.background = element_rect(fill='white', colour='white'),
        plot.margin = grid::unit(c(2, 4, 4, 6), "mm"),
        plot.subtitle = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 34),
        plot.title = element_text(margin = margin(0, 0, 0.1, 0), size = 38),
        strip.text.x = element_text(
          color = "grey25",lineheight = 0.15, 
          margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), size = 32, vjust = -2),
        strip.text.y = element_blank(),
        text = element_text(family = family))
  } 
  
  if(type == "level"){
    
    confidencePlot <-
      ggplot(results, aes(x = metric, 
                          y = pct_level,
                          fill = confidence_rankByLevel_minusPlus)) +
      geom_bar(position = 'stack', stat = 'identity') +
      ggh4x::facet_grid2(
        independent = "x", levelName ~ metric, 
        labeller = label_wrap_gen(width = 18), scales = "free", switch = "y") +
      scale_fill_viridis_d(name = "confidence", alpha = 0.7) +
      tidytext::scale_x_reordered() +
      scale_y_continuous(
        breaks = seq(0, 100, 20), minor_breaks = seq(10, 90, 10), position = "right") +
      labs(title = titleLab, subtitle = subtitleLab, y = "Percent") +
      theme_void(base_size = 42) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey25",size = 24),
        axis.ticks.length = unit(1, "mm"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.box.margin = margin(-4, -4, -4, -4),
        legend.key.size = unit(0.5, "line"),
        legend.justification = "center",
        legend.margin = margin(c(2, 0, 2, 0)),
        legend.position = "bottom",
        legend.spacing.x = unit(0.2, 'mm'),
        legend.text = element_text(color = "grey25", size = 28),
        legend.title = element_text(color = "grey25", size = 32),
        panel.background = element_rect(fill='white', colour='white'),
        panel.grid.major.y = element_line(color = "gray70"), 
        panel.grid.minor.y = element_line(color = "gray85"),
        panel.spacing = unit(0.2, "lines"),
        plot.background = element_rect(fill='white', colour='white'),
        plot.margin = grid::unit(c(2, 6, 4, 2), "mm"),
        plot.subtitle = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 32),
        plot.title = element_text(margin = margin(0, 0, 0.1, 0), size = 34),
        strip.text.x = element_blank(),
        strip.text.y = element_text(
          color = "grey25",lineheight = 0.15, 
          margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), size = 32),
        text = element_text(family = family))
  } 
  
  if(type == "overall"){
    
    confidencePlot <-
      ggplot(results, aes(x = metric, 
                          y = pct_overall,
                          fill = confidence_rankByLevel_minusPlus)) +
      geom_bar(position = 'stack', stat = 'identity') +
      scale_fill_viridis_d(name = "confidence", alpha = 0.7) +
      tidytext::scale_x_reordered() +
      scale_y_continuous(breaks = seq(0, 100, 20), minor_breaks = seq(10, 90, 10)) +
      labs(title = titleLab, subtitle = subtitleLab, y = "Percent") +
      theme_void(base_size = 42) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey25",size = 34),
        axis.ticks.length = unit(1, "mm"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 90, color = "grey25", size = 38, vjust = 3),
        legend.box.margin = margin(-4, -4, -4, -4),
        legend.key.size = unit(0.5, "line"),
        legend.justification = "center",
        legend.margin = margin(c(0, 0, 2, 0)),
        legend.position = "bottom",
        legend.spacing.x = unit(0.2, 'mm'),
        legend.text = element_text(color = "grey25", size = 30),
        legend.title = element_text(color = "grey25", size = 34),
        panel.background = element_rect(fill='white', colour='white'),
        panel.grid.major.y = element_line(color = "gray90"), 
        panel.grid.minor.y = element_line(color = "gray95"),
        panel.spacing = unit(0.2, "lines"),
        plot.background = element_rect(fill='white', colour='white'),
        plot.margin = grid::unit(c(2, 4, 2, 4), "mm"),
        plot.subtitle = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 34),
        plot.title = element_text(margin = margin(0.1, 0, 0.1, 0), size = 38),
        text = element_text(family = family))
  }
  
  return(confidencePlot)
  
}