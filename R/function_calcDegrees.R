# =========================================================================
# function_calcDegrees.R

# Created by: Gordon Aitken (ga41@hw.ac.uk)
# Created: 2021-07-05

# Last revised: 2021-09-10
# Last revised by: Melissa Bedinger (dr.m.bedinger@gmail.com)
# =========================================================================

calcDegrees <- 
  function(igraph, 
           vInfo) {
    
    require(igraph)
    require(installr)
    
    base <- igraph
    
    #list of nodes with level and number
    va <- vertex.attributes(base)
    
    key <- 
      data.frame("vName" = unlist(va$name), 
                 "level" = unlist(va$level), 
                 "num" = 1:length(unlist(va$name)))
    
    
    #Directed graph to isolate down and up degree
    dir <- as.directed(base, mode = "arbitrary")
    
    
    ### CALCULATE DEGREE (EDGES) ###
    
    #Calculate up degree 
    di <- 
      degree(dir, mode = "in") %>% 
      as.data.frame() %>% 
      rownames_to_column()
    
    colnames(di) <- c("vName", "degree_up1")
    
    #Calculate down degree
    do <- 
      degree(dir, mode = "out") %>% 
      as.data.frame() %>% 
      rownames_to_column()
    
    colnames(do) <- c("vName", "degree_down1")
    
    dt <- merge(di,do)
    
    dt <- merge(dt, key)
    
    #calculate total degree as sum of both up and down and reorder
    dt <- dt[order(dt$num),]
    
    dt$degree_total1 <- dt$degree_up1 +dt$degree_down1
    
    rownames(dt) <- NULL
    
    #Down second and third degree for nodes which have a second down (level 1-3)
    dt$degree_down2 <- 0
    
    dt$degree_down3 <- 0
    
    for(i in which(dt$level <=3)){
      vName <- dt$vName[i]
      
      temp <- 
        dir %>% 
        get.data.frame %>% 
        as_tibble() %>% 
        filter(from == vName)
      
      t <- length(unique(temp$to))
     
       temp2 <- 
         lapply(1:t, function(x){
           
           f <- 
             dir %>% 
             get.data.frame %>% 
             as_tibble() %>% 
             filter(from == temp$to[x])
           
           return(f$to)
           
         }
      
      )
       
      r <- unlist(temp2)

      dt$degree_down2[i] <- length(r)
      
      r2 <- unique(r)
      
      temp3 <- 
        lapply(1:length(r2), function(x){
          
          f <- 
            dir %>% 
            get.data.frame %>% 
            as_tibble() %>% 
            filter(from == r[x])
          
          return(f$to)
          
      }
      
      )
      
      dt$degree_down3[i] <- length((unlist(temp3)))
      
    }
    
    #UP second and third degree for nodes which have a second up (level 3-5)
    dt$degree_up2 <- 0
    
    dt$degree_up3 <- 0
    
    for(i in which(dt$level >=3)){
      vName <- dt$vName[i]
     
      temp <- 
        dir %>% 
        get.data.frame %>% 
        as_tibble() %>% 
        filter(to == vName)
      
      t <- length(unique(temp$from))
      
      temp2 <- 
        lapply(1:t, function(x){
        f <- 
          dir %>% 
          get.data.frame %>% 
          as_tibble() %>% 
          filter(to == temp$from[x])
        
        return(f$from)
        
        }
      
      )
      
      r <- unlist(temp2)
      
      dt$degree_up2[i] <- length(r)
      
      r2 <- unique(r)
      
      temp3 <- 
        lapply(1:length(r2), function(x){
          
          f <- 
            dir %>% 
            get.data.frame %>%
            as_tibble() %>% 
            filter(to == r[x])
          
          return(f$from)
        
      }
      
      )
      
      dt$degree_up3[i] <- length(unique(unlist(temp3)))
      
    }
    
    dt <- dt %>% select(-num, -level)
      
      
    ### CALCULATE DEGREE (VERTICES) ###
    
    #Calculate up degree 
    di <- 
      degree(dir, mode = "in") %>% 
      as.data.frame() %>% 
      rownames_to_column()
    
    colnames(di) <- c("vName", "nodeDegree_up1")
    
    #Calculate down degree
    do <- 
      degree(dir, mode = "out") %>% 
      as.data.frame() %>% 
      rownames_to_column()
    
    colnames(do) <- c("vName", "nodeDegree_down1")
    
    dt_nodes <- merge(di,do)
    
    dt_nodes <- merge(dt_nodes, key)
    
    #caluculate total degree as sum of both up and down and reorder
    dt_nodes <- dt_nodes[order(dt_nodes$num),]
    
    dt_nodes$nodeDegree_total1 <- 
      dt_nodes$nodeDegree_up1 + dt_nodes$nodeDegree_down1
    
    rownames(dt_nodes) <-NULL
    
    #Down second and third degree for nodes which have a second down (level 1-3)
    dt_nodes$nodeDegree_down2 <- 0
    
    dt_nodes$nodeDegree_down3 <- 0
    
    dt_nodes$nodeDegree_down4 <- 0
    
    for(i in which(dt_nodes$level <= 3)){
      
      node <- dt_nodes$vName[i]
      
      temp <- 
        dir %>% 
        get.data.frame %>% 
        as_tibble() %>% 
        filter(from == node)
      
      t <- length(unique(temp$to))
      
      temp2 <- 
        lapply(1:t, function(x){
          
        f <- 
          dir %>% 
          get.data.frame %>% 
          as_tibble() %>% 
          filter(from == temp$to[x])
        
        return(f$to)
        
        }
      
      )
      
      r <- unlist(temp2)
      
      dt_nodes$nodeDegree_down2[i] <- length(unique(r))
      
      
      r2 <- unique(r)
      
      temp3 <- 
        lapply(1:length(r2), function(x){
          
          f <- 
            dir %>% 
            get.data.frame %>% 
            as_tibble() %>% 
            filter(from == r[x])
        
          return(f$to)
          
      }
      
      )
      
      r3 <- unlist(temp3)
      
      dt_nodes$nodeDegree_down3[i] <- length(unique(r3))
      
      if(installr::is.empty(r3) == FALSE) {
        
        r4 <- unique(r3)
        
        temp4 <- 
          lapply(1:length(r4), function(x){
            
            f <- 
              dir %>% 
              get.data.frame %>% 
              as_tibble() %>% 
              filter(from == r3[x])
            
            return(f$to)
            
          }
          
          )
        
        r5 <- unlist(temp4)
        
        dt_nodes$nodeDegree_down4[i] <- length(unique(r5))
        
      } else {
        
        dt_nodes$nodeDegree_down4[i] <- 0
        
      }
      
    }
    
    
    #UP second and third degree for nodes which have a second up (level 3-5)
    dt_nodes$nodeDegree_up2 <- 0
    
    dt_nodes$nodeDegree_up3 <- 0
    
    dt_nodes$nodeDegree_up4 <- 0
    
    for(i in which(dt_nodes$level >= 3)){
      
      node <- dt_nodes$vName[i]
      
      temp <- 
        dir %>% 
        get.data.frame %>% 
        as_tibble() %>% 
        filter(to == node)
      
      t <- length(unique(temp$from))
      
      temp2 <- 
        lapply(1:t, function(x){
          
          f <- 
            dir %>% 
            get.data.frame %>% 
            as_tibble() %>% 
            filter(to == temp$from[x])
          
        return(f$from)
          
        }
      
      )
      
      r <- unlist(temp2)
      
      dt_nodes$nodeDegree_up2[i] <- length(unique(r))
      
      
      r2 <- unique(r)
      
      temp3 <- 
        lapply(1:length(r2), function(x){
          
          f <- 
            dir %>% 
            get.data.frame %>%
            as_tibble() %>% 
            filter(to == r[x])
        
        return(f$from)
          
      }
      
      )
      
      r3 <- unlist(temp3)
      
      dt_nodes$nodeDegree_up3[i] <- length(unique(r3))
      
      if(installr::is.empty(r3) == FALSE) {
        
        r4 <- unique(r3)
        
        temp4 <- 
          lapply(1:length(r4), function(x){
            
            f <- 
              dir %>% 
              get.data.frame %>% 
              as_tibble() %>% 
              filter(to == r3[x])
            
            return(f$from)
            
          }
          
          )
        
        r5 <- unlist(temp4)
        
        dt_nodes$nodeDegree_up4[i] <- length(unique(r5))
        
      } else {
        
        dt_nodes$nodeDegree_up4[i] <- 0
        
      }
      
    }
    
    dt_nodes <- dt_nodes %>% select(-num, -level)
    
    ### COMBINE ###
  
    dt_final <- 
      dt %>% 
      full_join(dt_nodes, by = "vName") %>%
      inner_join(vInfo)
    
    return(dt_final)
    
  }