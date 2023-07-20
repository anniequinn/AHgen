vis_plotRank <- function(results, metricName, levels, AH_benchmark = "baseline",
                         change.only = FALSE, confidence.lines = FALSE, 
                         family = "Harding") {
  
  require(ggplot2)
  require(grDevices)
  require(forcats)
  require(ggrepel)
  require(stringr)
  require(grid)
  
  if(change.only == FALSE){
    
    results <- 
      results %>%
      filter(metric == metricName, levelName %in% levels) %>% # filter dataset to one metric-levelName combination
      mutate(confidenceLab = case_when(
        confidence_rankByLevel_minusPlus == "High" ~ "H",
        confidence_rankByLevel_minusPlus == "Medium" ~ "M",
        confidence_rankByLevel_minusPlus == "Low" ~ "L")) %>% # add confidence rating labels
      arrange(Node) %>%
      arrange(cols) %>%
      mutate(cols = forcats::fct_inorder(cols)) # set levels to colours as a factor
    
  }
  
  if(change.only == TRUE){
    
    results <-
      results %>%
      filter(metric == metricName, levelName %in% levels) %>%
      mutate(confidenceLab = case_when(
        confidence_rankByLevel_minusPlus == "High" ~ "H",
        confidence_rankByLevel_minusPlus == "Medium" ~ "M",
        confidence_rankByLevel_minusPlus == "Low" ~ "L")) # add confidence rating labels
    
    results_tmp <-
      results %>%
      filter(scenario != AH_benchmark) %>%
      mutate(cols_unchanged = 
               case_when(change_rankByLevel == 0 ~ "unchanged",
                         change_rankByLevel != 0 ~ "changed")) %>%
      select(location, Node, cols_unchanged)
    
    results <-
      results %>%
      full_join(results_tmp) %>%
      mutate(cols = case_when(cols_unchanged == "changed" ~ cols,
                              cols_unchanged == "unchanged" ~ "#8C8C8C")) %>%
      arrange(cols) %>%
      mutate(cols = forcats::fct_inorder(cols))
    
  }
  
  if(confidence.lines == TRUE) {
    
    results_tmp <-
      results %>%
      filter(scenario != AH_benchmark) %>%
      mutate(
        linetypes = case_when(
          confidence_rankByLevel_minusPlus == "High" ~ "solid",
          confidence_rankByLevel_minusPlus == "Medium" ~ "dotdash",
          confidence_rankByLevel_minusPlus == "Low" ~ "dotted")) %>%
      select(location, Node, linetypes)
    
    results <-
      results %>%
      full_join(results_tmp) %>%
      mutate(linetypes = factor(linetypes, levels = c("solid", "dotted", "dotdash"))) # think levels are reverting back to alphabetical order
    
  } else if(confidence.lines == FALSE) {results <- results %>% mutate(linetypes = "solid")}
  
  if(metricName == "EC") {
    
    titleLab <- paste0(levels, "- Ranked by Eigenvector Centrality")
    
    plotBase <-
      results %>%
      ggplot(aes(x = scenario, 
                 y = reorder(rank_byLevel, desc(rank_byLevel)), 
                 group = Node)) +
      geom_blank()
    
    rankPlot <-
      plotBase +
      geom_line(aes(color = cols, linetype = linetypes), alpha = 0.8, linewidth = 0.4) +
      geom_point(aes(color = cols), size = 0.4, show.legend = TRUE) +
      geom_text(data = subset(results, scenario == AH_benchmark),
                mapping = aes(label = Node, color = cols, family = family, 
                              fontface = "bold", hjust = 1), 
                nudge_x = -0.05, size = 9) +
      geom_text(data = subset(results, scenario != AH_benchmark),
                mapping = aes(label = confidenceLab, 
                              color = cols, family = family, hjust = "left"), 
                nudge_x = 0.1, size = 9) +
      facet_wrap(~location, labeller = label_wrap_gen(width = 15), 
                 ncol = 3, scales = "free") +
      scale_color_manual(values = levels(results$cols)) +
      theme(axis.text.x = element_text(colour = "grey30", size = 28, vjust = 5),
            axis.text.y = element_text(colour = "grey30", size = 24),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "none",
            panel.background = element_rect(fill = "white", colour = "transparent"),
            panel.border = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour = "grey90"),
            plot.margin = grid::unit(c(2, 4, 2, 4), "mm"),
            plot.title = element_text(color = "grey20", size = 50, vjust = -1),
            strip.background = element_rect(fill = "white"),
            strip.text.x = element_text(colour = "grey20", size = 42),
            text = element_text(colour = "grey20", family = family)) +
      labs(title = titleLab, x = "scenario", color = "Node") + 
      expand_limits(x = c(-0.2, -1))
    
  }
  
  if(metricName == "SBC_norm") {
    
    titleLab <- paste0(levels, "- Ranked by Stable Betweenness Centrality")
    
    if(levels == "Tasks")
    {rankLimit <- 
      results %>% filter(levelName == "Tasks") %>% 
      select(Node) %>% unique() %>% nrow() %>% as.numeric()}
    if(levels == "Processes" | levels == "Resources")
    {rankLimit <- 25}
    
    nCols_df <-
      results %>%
      filter(metric == metricName, levelName %in% levels, 
             scenario == AH_benchmark, rank_byLevel <= rankLimit) %>%
      group_by(location) %>%
      count()
    
    colsDefault_step1 <- 
      results %>%
      filter(metric == metricName, levelName %in% levels, 
             scenario == AH_benchmark, rank_byLevel <= rankLimit) %>%
      select(location, Node) %>%
      arrange(location, Node)
    
    colsDefault_step2 <- data.frame()
    
    for (x in seq_along(nCols_df$n)) {
      
      n <- nCols_df$n[[x]]
      
      fun_color_range <- 
        grDevices::colorRampPalette(
          c("#FF0000", "#FF5300", "#66A61E", "#0098FF", "#0045FF", 
            "#0E00FF","#8A00FF", "#FF00F8","#FF0029")) 
      
      colsNew <- fun_color_range(n)
      
      location <- nCols_df$location[[x]] # adding location here only in case it needs to be checked
      
      tmp <- data.frame(colsNew, location) # adding location here only in case it needs to be checked
      
      tmp <- tmp %>% select(-location) # removing location for binding
      
      colsDefault_step2 <- colsDefault_step2 %>% rbind(tmp)
      
    }
    
    colsDefault_step3 <-
      colsDefault_step1 %>% cbind(colsDefault_step2)
    
    results <-
      results %>% 
      filter(rank_byLevel <= rankLimit) %>%
      full_join(colsDefault_step3, by = c("location", "Node")) %>%
      mutate(cols = case_when(!is.na(colsNew) & cols_unchanged == "changed" ~ colsNew,
                              !is.na(colsNew) & cols_unchanged == "unchanged" ~ cols,
                              is.na(colsNew) ~ cols)) %>%
      arrange(cols) %>%
      mutate(cols = forcats::fct_inorder(cols))
    
    results <-
      results %>%
      filter(scenario == AH_benchmark) %>%
      group_by(location, rank_byLevel) %>% 
      filter(n() > 1) %>% 
      ungroup(location, rank_byLevel) %>%
      mutate(group = "multi-benchmark") %>%
      select(Node, location, group) %>%
      full_join(results, by = c("Node", "location")) %>%
      mutate(group = replace_na(group, "single-benchmark"))
    
    plotBase <-
      results %>%
      ggplot(aes(x = as.factor(scenario), 
                 y = reorder(rank_byLevel, desc(rank_byLevel)), 
                 group = Node)) +
      geom_blank() 
    
    rankPlot <-
      plotBase +
      geom_line(aes(colour = cols, linetype = linetypes), alpha = 0.8, linewidth = 0.4) +
      geom_point(aes(colour = cols), size = 0.4, show.legend = TRUE) +
      ggrepel::geom_text_repel(
        data = subset(results, 
                      scenario == AH_benchmark & group == "multi-benchmark"),
        box.padding = 0.1, direction = "y", hjust = "left", force = 0.2, 
        lineheight = 0.15,
        mapping = aes(label = stringr::str_wrap(Node, 22), colour = cols, 
                      family = family, fontface = "bold", hjust = 1), 
        nudge_x = -1.95, seed = 1, segment.alpha = 0.5, segment.size = 0.3, size = 10, 
        xlim = c(-Inf, Inf), ylim = c(-Inf, Inf)) +
      geom_text(
        data = subset(results, 
                      scenario == AH_benchmark & group == "single-benchmark"),
        mapping = aes(label = Node, colour = cols, family = family, 
                      fontface = "bold", hjust = 1), 
        nudge_x = -0.05, size = 11) +
      geom_text(
        data = subset(results, scenario != AH_benchmark),
        mapping = aes(label = confidenceLab, 
                      color = cols, family = family, hjust = "left"), 
        nudge_x = 0.1, size = 9) +
      coord_cartesian(clip = "off") +
      scale_color_manual(values = levels(results$cols)) +
      facet_wrap(~location, labeller = label_wrap_gen(width = 15), 
                 ncol = 3, scales = "free") +
      theme(axis.text.x = element_text(colour = "grey30", size = 36, vjust = 3),
            axis.text.y = element_text(colour = "grey30", size = 30),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "none",
            panel.background = element_rect(fill = "white", colour = "transparent"),
            panel.border = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour = "grey90"),
            panel.spacing.y = unit(1, "lines"),
            plot.margin = grid::unit(c(2, 4, 6, 4), "mm"),
            plot.title = element_text(color = "grey20", size = 50, vjust = -1),
            strip.background = element_rect(fill = "white"),
            strip.text.x = element_text(colour = "grey20", size = 42),
            text = element_text(colour = "grey20", family = family)) +
      labs(title = titleLab, x = "scenario", colour = "Node") + 
      expand_limits(x = c(-0.2, -1))
    
  }
  
  return(rankPlot)
  
}