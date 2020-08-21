rm(list = ls()); cat("\014")

pacman::p_load(tidyverse, igraph, visNetwork)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("../"); getwd()

visualisationData <- function(input = results, edges = edges) { 
  
  Nodes <- 
    input %>%
    select(nodeName, layerName, layer, subnetwork) %>%
    as_tibble() %>%
    mutate(id = 1:n(),
           id = str_pad(id, width = 2, side = "left", pad = "0"),
           id = paste0("s", id)) %>%
    
    rename(level = layer) %>% 
    mutate(value = log(6-level),
           title = nodeName,
           label = "") %>% 
    as_tibble
  
  Links <- 
    edges %>% 
    rename(fromLabel = from, toLabel = to) %>%
    merge(Nodes %>% select(fromLabel = nodeName, from = id)) %>%
    merge(Nodes %>% select(toLabel = nodeName, to = id)) %>%
    filter(from %in% Nodes$id & to %in% Nodes$id) %>%
    as_tibble() %>%
    filter(from %in% Nodes$id & to %in% Nodes$id)
  
  list(nodes = Nodes, links = Links)
  
}

visualisation <- function(nodeData = Nodes, linkData = Links,
                          defaultColours = defaultColours, plotTitle,
                          fontSize = 35, interactive = FALSE,
                          height = "500px", width = "933px") {
  
  if(interactive == FALSE) { x = nodeData$subnetwork %>% unique; x = x[!str_detect(x, "Other")] }
  if(interactive != FALSE) { x = "All nodes" }
  
  visNetwork(nodeData, linkData, 
             height = height, 
             width = width,
             background = "white", 
             main = list(text = plotTitle, 
                         style = 
                           "font-family:Arial;
                         text-align:Center;
                         font-size: 21px")) %>%
    
    visNodes(color = list(border = defaultColours$dark,
                          background = defaultColours$light,
                          highlight = list(background = defaultColours$highlight,
                                           border = defaultColours$dark),
                          hover = list(background = defaultColours$highlight,
                                       border = defaultColours$dark)),
             font = list(size = fontSize)
    ) %>%
    
    visEdges(arrows = "to;from",
             color = list(color = defaultColours$dark,
                          select = defaultColours$dark,
                          highlight = defaultColours$dark,
                          hover = defaultColours$dark)) %>%
    
    visIgraphLayout(type = "full", 
                    layout = "layout_with_sugiyama", 
                    vgap = 25,
                    layers = nodeData$level,
                    physics = FALSE) %>%
    
    
    visInteraction(dragNodes = TRUE, 
                   dragView = TRUE, 
                   zoomView = TRUE,
                   hover = TRUE, 
                   tooltipDelay = 0, 
                   multiselect = TRUE, 
                   selectConnectedEdges = TRUE, 
                   
                   tooltipStyle = 
                     "position:fixed;
                   visibility:hidden;
                   padding:5px;
                   white-space:nowrap;
                   font-family:Arial;
                   font-size:18px;
                   background-color:transparent") %>%
    
    
    visOptions(highlightNearest = list(enabled = TRUE, 
                                       degree = list(from = 4, to = 4), 
                                       algorithm = "hierarchical",
                                       hideColor = defaultColours$hideColor), 
               selectedBy = list(variable = "subnetwork", 
                                 style = "font-family:Arial",
                                 hideColor = defaultColours$hideColor,
                                 selected = x, multiple = TRUE)
    ) 
  
}

pickColours <- 
  
  function(name, alpha = 0.1) { 
    
    file <- read_csv("colours.csv", col_types = cols())
    x <- which(file$subnetwork == name)
    file <- file[x,]
    
    list(dark = file[["dark"]], light = file[["light"]], 
         highlight = file[["highlight"]], 
         hideColor = paste0("rgba(200,200,200,", alpha, ")"))
    
  }

dl <- readRDS("tmp.RDS")

visNetwork::visIgraph(igraph = dl$di, type = "full", layout = "layout_with_sugiyama")
visIgraph

dv <- toVisNetworkData(dl$di)
dv %>% summary

dv$edges$value <- dv$edges$weight

dv$nodes$color <- dl$dc$before

dv$nodes$value <- NULL
dv$edges$value <- NULL


visNetwork(dv$nodes, dv$edges, 
           #height = height, 
           #width = width,
           background = "white", 
           main = list(text = "plotTitle", 
                       style = 
                         "font-family:Arial;
                         text-align:Center;
                         font-size: 21px")) %>%
  
  # visNodes(color = list(border = defaultColours$dark,
  #                       background = defaultColours$light,
  #                       highlight = list(background = defaultColours$highlight,
  #                                        border = defaultColours$dark),
  #                       hover = list(background = defaultColours$highlight,
  #                                    border = defaultColours$dark)),
  #          font = list(size = fontSize)
  # ) %>%
  # 
  # visEdges(arrows = "to;from",
  #          color = list(color = defaultColours$dark,
  #                       select = defaultColours$dark,
  #                       highlight = defaultColours$dark,
  #                       hover = defaultColours$dark)) %>%
  
  visIgraphLayout(type = "full", 
                  layout = "layout_with_sugiyama", 
                  vgap = 25,
                  #layers = nodeData$level,
                  physics = FALSE) 

g <- make_ring(10)
tkplot(g)

dl$di %>% plot()

pacman::p_load(ggnetwork)

?ggnetwork::geom_edges()

?ggnetwork

ggnetwork:::fortify.igraph(model = dl$di, layout = layout_with_sugiyama(dl$di))

ggnetwork(dl$di, layout = layout_with_sugiyama)

ggplot(dl$di, aes(x, y, xend = xend, yend = yend)) + geom_edges(aes(colour = weight))


tree <- make_tree(20, 3)
plot(tree, layout=layout_as_tree)
plot(tree, layout=layout_as_tree(tree, flip.y=FALSE))
plot(tree, layout=layout_as_tree(tree, circular=TRUE))

tree2 <- make_tree(10, 3) + make_tree(10, 2)
plot(tree2, layout=layout_as_tree)
plot(tree2, layout=layout_as_tree(tree2, root=c(1,11),
                                  rootlevel=c(2,1)))

plot(dl$di, layout = layout_with_sugiyama)


tmp <- layout_with_drl(dl$di)

plot(tmp)

plot(dl$di, layout = layout_with_drl)

pacman::p_load(ggraph)
flaregraph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
from <- match(flare$imports$from, flare$vertices$name)
to <- match(flare$imports$to, flare$vertices$name)
ggraph(flaregraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha = 0.1) + 
  coord_fixed()


ggraph(flaregraph, layout = 'dendrogram', circular = FALSE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha = 0.1) + 
  coord_fixed()

?ggraph
pacman::p_load(tidygraph)
set_graph_style(plot_margin = margin(1,1,1,1))
graph <- as_tbl_graph(highschool)
ggraph(graph, 'focus', focus = node_is_center()) + 
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = r), data.frame(r = 1:5), colour = 'grey') + 
  geom_edge_link() + 
  geom_node_point() + 
  coord_fixed()

graph



ggraph(dl$di, 'focus', focus = 1:12) + 
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = r), data.frame(r = 1:4), colour = 'grey') + 
  geom_edge_link() + 
  geom_node_point() + 
  coord_fixed() + 
  theme_classic()


graphlayouts::layout_igraph_focus(dl$di, 1) %>% ggplot(aes(x = x, y = y)) + geom_point()

?layout_tbl_graph_manual


# see the stuff about matrix graphs https://www.data-imaginist.com/2019/1-giraffe-2-giraffe-go/


graph <- create_notable('zachary') %>%
  mutate(group = factor(group_infomap())) %>%
  morph(to_split, group) %>%
  activate(edges) %>%
  mutate(edge_group = as.character(.N()$group[1])) %>%
  unmorph()

graph

ggraph(graph, 'fabric', sort.by = node_rank_fabric()) + 
  geom_node_range(aes(colour = group), alpha = 0.3) + 
  geom_edge_span(aes(colour = edge_group), end_shape = 'circle') + 
  coord_fixed() + 
  theme(legend.position = 'top')

?node_rank_fabric

ggraph(graph, 'matrix', sort.by = node_rank_hclust()) + 
  geom_edge_point(aes(colour = edge_group), mirror = FALSE) + 
  coord_fixed()



ggraph(graph, 'matrix') + 
  geom_edge_point(aes(colour = edge_group), mirror = TRUE) + 
  coord_fixed()

V(dl$di)$centrality <- dl$dc$before
dl$di

ggraph(dl$di, "matrix", sort.by = node_rank_hclust()) +
  #geom_edge_point(mirror = TRUE, aes(shape = layers)) +
  geom_node_point(aes(colour = centrality)) +
  coord_fixed()
