# =========================================================================
# sbc.R

# Created by: Channin Songchon (cs127@hw.ac.uk) & Gordon Aitken (ga41@hw.ac.uk)
# Created: 2021-06-28

# Last revised: 2021-08-02
# Last revised by: Melissa Bedinger (dr.m.bedinger@gmail.com)
# =========================================================================

# create sbc function (with normalization)
sbc_norm <- 
  function(graph, 
           undirected = TRUE, 
           normalize = TRUE) {
    
    df <- 
      as_adjacency_matrix(graph, 
                          type = c("both"),
                          attr = "weight", 
                          edges = FALSE, 
                          names = TRUE)
    
    df[is.na(df)] <- 0
    
    nodes <- V(graph)$name
    
    outsbc <- 
      data.frame(id = as.character(nodes), sbc = NA, stringsAsFactors = FALSE)
    
    outsbc$id <- gsub(".", " ", outsbc$id, fixed = TRUE)
    
    rownames(outsbc) <- outsbc$id
    
    outsbc$id <- NULL
    
    #Shortest path length
    
    if(undirected == TRUE) {
      
      sp <- shortest.paths(graph, mode = "all")
      
    } else {
      
      sp <- shortest.paths(graph, mode = "out")
      
    }
    
    #delete 1 vertex, calculate path length
    for (i in 1:nrow(outsbc)) {
      
      df2 <- df
      
      df2[i,] = df2[,i] = 0.0
      
      df2 <- data.matrix(df2, rownames.force = NA)
      
      if(undirected == TRUE) {
        
        g2 <- 
          graph_from_adjacency_matrix(df2, 
                                      mode = "undirected", 
                                      weighted = TRUE)
        
        sp2 <- shortest.paths(g2, mode = "all")
        
      } else {
        
        g2 <- 
          graph_from_adjacency_matrix(df2, 
                                      mode = "directed", 
                                      weighted = TRUE)
        
        sp2 <- shortest.paths(g2, mode = "out")
        
      }
      
      diff <- sp2-sp
      
      diff[is.infinite(diff)] <- NA
      
      sumval <- sum(diff, na.rm = TRUE)
      
      outsbc$sbc[i] <- sumval
      
    }
    
    if(normalize == TRUE) {
      
      # Calculate SBC with normalisation
      # Currently only used method compatible with igraph: https://igraph.org/r/doc/betweenness.html
      # Bnorm = 2*SBC/(n*n-3*n+2)
      # Ignored scaling as this created interpretation problems for max SBC node (e.g. for USAH, usually Public health showed no % change as was always scaled to 1)
      B <- outsbc$sbc 
      n <- igraph::gorder(graph) %>% as.numeric() # returns numeric number of vertices, instead of full list of vertices
      Bnorm <- 2*B/(n*n-3*n+2)
      outsbc$sbcNorm <- Bnorm
      
      outsbc
      
    }
    
    return(outsbc)
    
}