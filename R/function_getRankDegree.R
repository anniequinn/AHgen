# Function to get table of nodes ranked by degree
# Options are "all", "Purposes", "Outcomes", "Tasks", "Processes", and "Resources"
function_getRankDegree <-
  function(
    USAH_input,
    level = "all") {
    
    if(level == "all") {
      
      output <-
        USAH_input$results %>%
        filter(metric %in% 
                 c("nodeDegree_up1", "nodeDegree_down1", "nodeDegree_total1")) %>%
        mutate(levelName_viz = str_c(level, " - ", levelName)) %>%
        select(levelName_viz, Node, metric, value) %>%
        pivot_wider(names_from = metric, values_from = value) %>%
        arrange(levelName_viz, desc(nodeDegree_total1)) %>%
        rename(Level = levelName_viz, Node = Node, 
               `Up Degree` = nodeDegree_up1, 
               `Down Degree` = nodeDegree_down1, 
               `Total Degree` = nodeDegree_total1)
      
    } else if(level == "Purposes") {
      
      output <-
        USAH_input$results %>%
        filter(metric %in% 
                 c("nodeDegree_down1", "nodeDegree_down2", 
                   "nodeDegree_down3", "nodeDegree_down4")) %>%
        filter(level == 1) %>%
        mutate(levelName_viz = str_c(level, " - ", levelName)) %>%
        select(levelName_viz, Node, metric, value) %>%
        pivot_wider(names_from = metric, values_from = value) %>%
        rename(Level = levelName_viz, Node = Node,
               Outcomes = nodeDegree_down1, Tasks = nodeDegree_down2, 
               Processes = nodeDegree_down3, Resources = nodeDegree_down4) %>%
        select(Level, Node, Outcomes, Tasks, Processes, Resources)
      
    } else if(level == "Outcomes") {
      
      output <-
        USAH_input$results %>%
        filter(metric %in% 
                 c("nodeDegree_up1", "nodeDegree_down1", 
                   "nodeDegree_down2", "nodeDegree_down3")) %>%
        filter(level == 2) %>%
        mutate(levelName_viz = str_c(level, " - ", levelName)) %>%
        select(levelName_viz, Node, metric, value) %>%
        pivot_wider(names_from = metric, values_from = value) %>%
        rename(Level = levelName_viz, Node = Node,
               Purposes = nodeDegree_up1, Tasks = nodeDegree_down1, 
               Processes = nodeDegree_down2, Resources = nodeDegree_down3) %>%
        select(Level, Node, Purposes, Tasks, Processes, Resources)
      
    } else if(level == "Tasks") {
      
      output <-
        USAH_input$results %>%
        filter(metric %in% 
                 c("nodeDegree_up2", "nodeDegree_up1", 
                   "nodeDegree_down1", "nodeDegree_down2")) %>%
        filter(level == 3) %>%
        mutate(levelName_viz = str_c(level, " - ", levelName)) %>%
        select(levelName_viz, Node, metric, value) %>%
        pivot_wider(names_from = metric, values_from = value) %>%
        rename(Level = levelName_viz, Node = Node,
               Purposes = nodeDegree_up2, Outcomes = nodeDegree_up1, 
               Processes = nodeDegree_down1, Resources = nodeDegree_down2) %>%
        select(Level, Node, Purposes, Outcomes, Processes, Resources)
      
    } else if(level == "Processes") {
      
      output <-
        USAH_input$results %>%
        filter(metric %in% 
                 c("nodeDegree_up3", "nodeDegree_up2", 
                   "nodeDegree_up1", "nodeDegree_down1")) %>%
        filter(level == 4) %>%
        mutate(levelName_viz = str_c(level, " - ", levelName)) %>%
        select(levelName_viz, Node, metric, value) %>%
        pivot_wider(names_from = metric, values_from = value) %>%
        rename(Level = levelName_viz, Node = Node,
               Purposes = nodeDegree_up3, Outcomes = nodeDegree_up2, 
               Tasks = nodeDegree_up1, Resources = nodeDegree_down1) %>%
        select(Level, Node, Purposes, Outcomes, Tasks, Resources)
      
    } else if(level == "Resources") {
      
      output <-
        USAH_input$results %>%
        filter(metric %in% 
                 c("nodeDegree_up4", "nodeDegree_up3", 
                   "nodeDegree_up2", "nodeDegree_up1")) %>%
        filter(level == 5) %>%
        mutate(levelName_viz = str_c(level, " - ", levelName)) %>%
        select(levelName_viz, Node, metric, value) %>%
        pivot_wider(names_from = metric, values_from = value) %>%
        rename(Level = levelName_viz, Node = Node,
               Purposes = nodeDegree_up4, Outcomes = nodeDegree_up3, 
               Tasks = nodeDegree_up2, Processes = nodeDegree_up1) %>%
        select(Level, Node, Purposes, Outcomes, Tasks, Processes)
      
    }
    
    return(output)
    
  }