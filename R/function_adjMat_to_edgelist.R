adjMat_to_edgelist <- function(adjMat, vInfo) {
  
  require(igraph)
  require(tibble)
  
  colNames <- names(adjMat)
  index <- which(colNames %in% c("Node", "level", "levelName_full", "levelName"))
  
  mat <- adjMat %>% select(-index)
  mat[upper.tri(mat, diag = TRUE)] <- NA
  
  # Internal function to use pivot_longer in a similar way to reshape2::melt
  internal_melt2 <- function(input, measure.vars, 
                    variable.name = "variable", value.name = "value") { 
    input %>% 
      pivot_longer(cols = all_of(measure.vars), 
                   names_to = variable.name, values_to = value.name) 
  }
  
  step1 <- 
    adjMat %>% 
    select(Node) %>% 
    cbind(mat) %>% 
    tibble::as_tibble() %>%
    internal_melt2(measure.vars = c(2:ncol(.))) %>% 
    filter(value == 1)
  
  levelKey <- 
    tibble(level = 1:5, 
           levelName_full = c("Functional purposes", 
                              "Values and priority measures", 
                              "Generalised functions", 
                              "Object-related processes", 
                              "Physical objects"),
           levelName = c("Purposes",
                         "Outcomes",
                         "Tasks",
                         "Processes",
                         "Resources")) %>%
    mutate(abbr = paste0("l", level, c("FP", "VPM", "GF", "ORP", "PO")))
  
  step2 <- 
    adjMat %>% 
    select(-any_of(c("level", "levelName_full", "levelName"))) %>%
    left_join(vInfo, by = "Node") %>%
    select(level, levelName_full, levelName, Node) %>% 
    left_join(levelKey, by = c("level", "levelName_full", "levelName")) %>% 
    select(abbr, Node)
  
  output <- 
    step1 %>%
    select(from = variable, to = Node, weight = value) %>%
    left_join(step2 %>% select(levelFrom = abbr, from = Node), by = "from") %>%
    left_join(step2 %>% select(levelTo = abbr, to = Node), by = "to") %>%
    mutate(layer = paste(levelFrom, levelTo, sep = "_")) %>%
    select(layer, from, to, weight) %>%
    arrange(layer, from, to)
  
  return(output)
  
}