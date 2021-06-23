getResults <- function(igraph, vInfo) { 
  
  resultsWVBC <- # Unstable version: https://doi.org/10.1109/ICASSP.2015.7178599 ; stable version being developed in Python as of 2021-06-23
    igraph %>% 
    calcWVBC(vInfo) %>%
    mutate(rank_overall = dense_rank(desc(WVBC))) %>% 
    group_by(level) %>% 
    mutate(rank_byLevel = dense_rank(desc(WVBC))) %>%
    ungroup() %>%
    arrange(level, vName) %>%
    rename(centrality = WVBC) %>%
    mutate(type = "WVBC", .before = 1)
  
  resultsEC <- 
    igraph %>%
    calcEC(vInfo) %>%
    mutate(rank_overall = dense_rank(desc(centrality))) %>% 
    group_by(level) %>% 
    mutate(rank_byLevel = dense_rank(desc(centrality))) %>%
    ungroup() %>%
    arrange(level, vName) %>%
    mutate(type = "EC", .before = 1)
  
  rbind(resultsEC, resultsWVBC) %>% 
    mutate(type = fct_inorder(type)) %>% 
    select(type, level, levelName, vName, centrality, contains("rank"))
  
}