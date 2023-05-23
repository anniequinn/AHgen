checkNames <- function(adjMat_template, 
                       vInfo_template_full, 
                       key) {
  
  step1 <- 
    adjMat_template %>% 
    mutate(adjMat = "TRUE") %>% 
    select(level, vName, adjMat)
  
  step2 <- 
    vInfo_template_full %>% 
    select(vName) %>% 
    mutate(vInfo_full = "TRUE")
  
  step3 <- 
    key %>% 
    select(physicalObject) %>% 
    unique() %>% 
    rename(vName = physicalObject) %>% 
    mutate(level = 5, key = "TRUE")
  
  step4 <-
    step1 %>% 
    full_join(step2, by = "vName") %>% 
    full_join(step3, by = c("vName", "level"))
  
  step5 <- step4 %>% filter(is.na(adjMat) | is.na(vInfo_full))
  
  step6 <- step4 %>% filter(level == 5, is.na(key))
  
  step7 <- step5 %>% rbind(step6)
  
  if(nrow(step7 > 0)) {
    
    print("Some vNames do not match. Please inspect and revise input data.")
    
    print(step7)
    
    return(step4)
    
  } else {
    
    print("vNames from inputs match. Proceed!")
    
  }
  
}