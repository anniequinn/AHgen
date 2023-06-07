getResults <- 
  function(igraph, vInfo, name, version, location, scenario) {
  
  resultsEC <- 
    igraph %>%
    calcEC(vInfo) %>%
    rename(value = centrality) %>%
    group_by(level) %>% 
    mutate(rank_byLevel = dense_rank(desc(value))) %>%
    ungroup() %>%
    arrange(level, Node) %>%
    mutate(metric = "EC", .before = 1)
  
  resultsSBC <-
    igraph %>%
    calcSBC(vInfo) %>%
    gather(c("SBC", "SBC_norm"), key = "metric", value = "value") %>%
    group_by(metric, level) %>% 
    mutate(rank_byLevel = dense_rank(desc(value))) %>%
    ungroup() %>%
    arrange(level, Node)
  
  resultsDegrees <-
    igraph %>%
    calcDegrees(vInfo) %>%
    gather(c("degree_up1", "degree_down1", "degree_total1",
             "degree_up2", "degree_up3", 
             "degree_down2", "degree_down3",
             "nodeDegree_up1", "nodeDegree_down1", "nodeDegree_total1",
             "nodeDegree_up2", "nodeDegree_up3", "nodeDegree_up4",
             "nodeDegree_down2", "nodeDegree_down3", "nodeDegree_down4"),
           key = "metric", value = "value") %>%
    group_by(metric, level) %>% 
    mutate(rank_byLevel = dense_rank(desc(value))) %>%
    ungroup() %>%
    arrange(level, Node)
  
  output <- 
    rbind(resultsEC, resultsSBC, resultsDegrees) %>%
    mutate(levelName_full = 
             factor(levelName_full, levels = c("Functional purposes", 
                                               "Values and priority measures", 
                                               "Generalisted functions",
                                               "Object-related processes",
                                               "Physical objects")),
           levelName = 
             factor(levelName, levels = c("Purposes", 
                                          "Outcomes", 
                                          "Tasks",
                                          "Processes",
                                          "Resources")),
           metric = fct_inorder(metric),
           version = paste0(name, "_", version),
           location = location,
           scenario = scenario) %>% 
    select(version, location, scenario, level, levelName_full, levelName,
           Node, metric, value, contains("rank"))
  
  return(output)
  
  }