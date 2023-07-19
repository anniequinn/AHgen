check_names <- function(adjMat_template, vInfo_template_full, key) {
  
  step1 <- 
    adjMat_template %>% 
    mutate(adjMat = "TRUE") %>% 
    select(level, Node, adjMat)
  
  step2 <- 
    vInfo_template_full %>% 
    select(Node) %>% 
    mutate(vInfo_full = "TRUE")
  
  step3 <- 
    key %>% 
    select(resource) %>% 
    unique() %>% 
    rename(Node = resource) %>% 
    mutate(level = 5, key = "TRUE")
  
  output <-
    step1 %>% 
    full_join(step2, by = "Node") %>% 
    full_join(step3, by = c("Node", "level"))
  
  check_step1 <- output %>% filter(is.na(adjMat) | is.na(vInfo_full))
  
  check_step2 <- output %>% filter(level == 5, is.na(key))
  
  check_step3 <- check_step1 %>% rbind(check_step2)
  
  if(nrow(check_step3 > 0)) {
    
    print("Some Node names do not match. Please inspect and revise input data.")
    
    print(check_step3)
    
    return(output)
    
  } else {
    
    print("Node names from inputs match. Proceed!")
    
  }
  
}