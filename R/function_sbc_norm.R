# =========================================================================
# sbc_norm.R

# Created by: Channin Songchon (cs127@hw.ac.uk) & Gordon Aitken (ga41@hw.ac.uk)
# Created: 2021-06-28

# Last revised: 2023-07-19
# Last revised by: Melissa Bedinger (dr.m.bedinger@gmail.com)
# =========================================================================

# create sbc function (with normalization)
sbc_norm <- function(igraph, undirected = TRUE, normalize = TRUE) {
  
  require(igraph)
    
  df <- 
    igraph::as_adjacency_matrix(igraph, 
                                type = c("both"),
                                attr = "weight", 
                                edges = FALSE, 
                                names = TRUE)
    
    df[is.na(df)] <- 0
    
    nodes <- igraph::V(igraph)$name
    
    output <- 
      data.frame(id = as.character(nodes), sbc = NA, stringsAsFactors = FALSE)
    
    output$id <- gsub(".", " ", output$id, fixed = TRUE)
    
    rownames(output) <- output$id
    
    output$id <- NULL
    
    #Shortest path length
    
    if(undirected == TRUE) {
      
      sp <- igraph::shortest.paths(igraph, mode = "all")
      
    } else {
      
      sp <- igraph::shortest.paths(igraph, mode = "out")
      
    }
    
    #delete 1 vertex, calculate path length
    for (i in 1:nrow(output)) {
      
      df2 <- df
      
      df2[i,] = df2[,i] = 0.0
      
      df2 <- data.matrix(df2, rownames.force = NA)
      
      if(undirected == TRUE) {
        
        g2 <- 
          igraph::graph_from_adjacency_matrix(df2, 
                                              mode = "undirected", 
                                              weighted = TRUE)
        
        sp2 <- igraph::shortest.paths(g2, mode = "all")
        
      } else {
        
        g2 <- 
          igraph::graph_from_adjacency_matrix(df2, 
                                              mode = "directed", 
                                              weighted = TRUE)
        
        sp2 <- igraph::shortest.paths(g2, mode = "out")
        
      }
      
      diff <- sp2-sp
      
      diff[is.infinite(diff)] <- NA
      
      sumval <- sum(diff, na.rm = TRUE)
      
      output$sbc[i] <- sumval
      
    }
    
    if(normalize == TRUE) {
      
      # Calculate SBC with normalisation
      # Currently only used method compatible with igraph: https://igraph.org/r/doc/betweenness.html
      # Bnorm = 2*SBC/(n*n-3*n+2)
      # Ignored scaling as this created interpretation problems for max SBC node (e.g. for USAH, usually Public health showed no % change as was always scaled to 1)
      B <- output$sbc 
      n <- igraph::gorder(igraph) %>% as.numeric() # returns numeric number of vertices, instead of full list of vertices
      Bnorm <- 2*B/(n*n-3*n+2)
      output$sbcNorm <- Bnorm
      
      output
      
    }
    
    return(output)
    
}