# lost version 1 of vis

rm(list = ls()); cat("\014")

pacman::p_load(tidyverse, igraph, graphlayouts, ggraph)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

di <- readRDS("AH_igraph.RDS")

# https://github.com/schochastics/graphlayouts

di

di %>% graphlayouts::layout_igraph_stress() %>% plot()

ggraph(di, layout="stress")+
  geom_edge_link0(width=0.2,colour="grey")+
  geom_node_point(col="black",size=0.3)+
  theme_graph()

ggraph(di,layout = "nicely") +
  geom_edge_link0() +
  geom_node_point() +
  theme_graph()

ggraph(di, layout = "stress",bbox = 40) +
  geom_edge_link0() +
  geom_node_point() +
  theme_graph()


# 
# ggraph(g,layout="manual",x=bb$xy[,1],y=bb$xy[,2])+
#   geom_edge_link0(aes(col=col),width=0.1)+
#   geom_node_point(aes(col=grp))+
#   scale_color_brewer(palette = "Set1")+
#   scale_edge_color_manual(values=c(rgb(0,0,0,0.3),rgb(0,0,0,1)))+
#   theme_graph()+
#   theme

score <- igraph::betweenness(di)

ggraph(di,layout = "centrality", centrality = score, tseq = seq(0,1,0.15)) +
  draw_circle(use = "cent") +
  annotate_circle(score,format="",pos="bottom") +
  geom_edge_link0(edge_color="black",edge_width=0.3)+
  geom_node_point(size=2,shape=21)

pacman::p_load(igraphdata, patchwork)
data("karate")

p1 <- ggraph(karate,layout = "focus",focus = 1) +
  draw_circle(use = "focus",max.circle = 3)+
  geom_edge_link0(edge_color="black",edge_width=0.3)+
  geom_node_point(aes(fill=as.factor(Faction)),size=2,shape=21)+
  scale_fill_manual(values=c("#8B2323", "#EEAD0E"))+
  theme_graph()+
  theme(legend.position = "none")+
  coord_fixed()+
  labs(title= "Focus on Mr. Hi")
p1

man <- layout_with_focus(di, V(di)[[1]])

ggraph(di,layout="manual",x=man$xy[,1],y=man$xy[,2])+
  draw_circle(use = "focus",max.circle = 5) +
   geom_edge_link0()+
   geom_node_point() +
  coord_fixed()

di %>% distances() %>% as_tibble() %>% hclust()


A = get.adjacency(di, sparse=FALSE)
A

# cosine similarity
S = cos(A)

# distance matrix
D = 1-S

# distance object
d = as.dist(D)

# average-linkage clustering method
cc = hclust(d, method = "average")
cc


?cluster_fast_greedy()

cluster_fast_greedy(di)

pacman::p_load(berryFunctions)

# x	2 	+	 y	2 	=	 r	2
# The point (0,r) ends up at x=rsin??, y=rcos??.

# In general, suppose that you are rotating about the origin clockwise through an angle ??. Then the point (s,t) ends up at (u,v) where
# u=scos??+tsin??andv=???ssin??+tcos??.

tmp <- di %>% get.data.frame %>% as_tibble %>% filter(layers == "l1FP_l2VPM") 
tmp <- tmp$from %>% unique

files = list.files("inputs", pattern = "node", full.names = TRUE)

tmp <- read_csv(files[[1]]) %>% select(1:3)

tmp <- tmp %>% rename(r = layer)
tmp


test <- 
  tibble(node = tmp) %>% mutate(deg = 1:n() * 360/n(), rad = deg * pi / 180,
                              r = 1, 
                              x = r * sin(rad), y = r * cos(rad))

test %>% ggplot() + geom_point(aes(x = x, y = y)) + coord_fixed()


test <- 
  tmp %>% 
  group_by(layerName) %>%
  mutate(deg = 1:n() * 360/n(), rad = deg * pi / 180,
       
       x = r * sin(rad), y = r * cos(rad))

p <- test %>% ggplot(aes(label = nodeName)) + geom_point(aes(x = x, y = y)) + coord_equal()

p %>% plotly::ggplotly(tooltip = "label")

man$xy



ggraph(di,layout="manual",x=test$x,y=test$y)+
  draw_circle(use = "focus",max.circle = 5) +
  geom_edge_link0(alpha = 0.25) +
  geom_node_point() +
  coord_fixed()

abc <- read_csv(files[[1]])
abc

def <- 
abc %>% select(1:3, contains("nature")) %>%
  rename(r = layer) %>%
  group_by(layerName) %>%
  mutate(deg = 1:n() * 360/n(), rad = deg * pi / 180,
         
         x = r * sin(rad), y = r * cos(rad))

sub = def %>% filter(subnetwork_nature == 1)

sub$nodeName

index <- 
(V(di) %>% unclass() %>% names) %in% sub$nodeName

V(di) <- V(di)[index]


nodes <- (V(di) %>% unclass() %>% names)

di - nodes[!index]

igraph::neighbors(di, V(di)[[1]], "out")


DI <- as.directed(di, "mutual")
DI

iii <- 
igraph::neighbors(DI, V(DI)[[1]], "out")
iii

iiii <- neighbors(DI, iii, "out")
iiii

neighbors(DI, iiii, "out")

check <- c(V(DI)[[1]], iii, iiii) %>% unclass %>% names
check

then <- induced_subgraph(DI, check) %>% as.undirected("mutual")
then

plot2 <- test %>% ungroup %>% filter(nodeName %in% (V(then) %>% unclass %>% names))



ggraph(then,layout="manual",x=plot2$x,y=plot2$y)+
  draw_circle(use = "focus",max.circle = 5) +
  geom_edge_link0(alpha = 0.25) +
  geom_node_point() +
  coord_fixed()

source("functions/functions.R")

dl <- di %>% get.data.frame %>% as_tibble %>% split(., .$layers) %>% modify(. %>% edgelist_to_igraph)
dl


l1 = neighbors(dl[[4]], "Auction houses")
alloutputs1 <- l1 %>% unclass %>% names %>% unique
alloutputs1

alloutputs2 <- sapply(alloutputs1, function(x) { neighbors(dl[[3]], x) %>% unclass %>% names }) %>% unlist %>% as.vector %>% unique
alloutputs2

alloutputs3 <- sapply(alloutputs2, function(x) { neighbors(dl[[2]], x) %>% unclass %>% names }) %>% unlist %>% as.vector %>% unique
alloutputs3

alloutputs4 <- sapply(alloutputs3, function(x) { neighbors(dl[[1]], x) %>% unclass %>% names }) %>% unlist %>% as.vector %>% unique
alloutputs4

obj <- c(alloutputs1, alloutputs2, alloutputs3, alloutputs4, "Auction houses")

plot3 <- test %>% ungroup %>% filter(nodeName %in% obj)
plot3

is <- induced_subgraph(DI, obj)

gg <- 
ggraph(is,layout="manual",x=plot3$x,y=plot3$y)+
  draw_circle(use = "focus",max.circle = 5) +
  geom_edge_link0(alpha = 0.25) +
  geom_node_point() +
  coord_fixed()

gg %>% plotly::ggplotly()

pacman::p_load(ggiraph)

ggiraph(gg)

geom_segment_interactive()

hm <- ggraph(is,layout="manual",x=plot3$x,y=plot3$y)
hm$data





ggraph(is,layout="manual",x=plot3$x,y=plot3$y)+
  draw_circle(use = "focus",max.circle = 5) +
  #geom_edge_link0(alpha = 0.25) +
  geom_segment_interactive() + 
  geom_node_point() +
  coord_fixed()

is
ggraph:::complete_edge_aes(is)

get_edges(is, "short", collapse = "all")

?ggraph:::get_edges()

mylayout <- ggraph::create_layout(is, plot3 %>% select(x,y))
mylayout


new <- get_edges("short", collapse = "none")

new2 <- new(mylayout)

mylayout

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

ggplot(dat,aes(x,y)) + geom_path()

final <- 
  mylayout %>% ggplot(aes(label = name)) + 
  #geom_point(aes(x = x, y = y), inherit.aes = FALSE, size = 5) +
  geom_path(data = circleFun(diameter = 2), inherit.aes = FALSE, aes(x = x, y = y), size = 0.25, alpha = 1, colour = "lightblue") +
  geom_path(data = circleFun(diameter = 4), inherit.aes = FALSE, aes(x = x, y = y), size = 0.25, alpha = 1, colour = "lightblue") +
  geom_path(data = circleFun(diameter = 6), inherit.aes = FALSE, aes(x = x, y = y), size = 0.25, alpha = 1, colour = "lightblue") +
  geom_path(data = circleFun(diameter = 8), inherit.aes = FALSE, aes(x = x, y = y), size = 0.25, alpha = 1, colour = "lightblue") +
  geom_path(data = circleFun(diameter = 10), inherit.aes = FALSE, aes(x = x, y = y), size = 0.25, alpha = 1, colour = "lightblue") +
  geom_segment(data = new2, aes(x = x, xend = xend, y = y, yend = yend), inherit.aes = FALSE, alpha = 0.25, colour = "lightgrey") +
  geom_point(aes(x = x, y = y), size = 5) +
  
  coord_fixed() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())

final %>% plotly::ggplotly(tooltip = "label")
