install.packages('igraph')
library(igraph)
m=matrix(nrow=3,ncol=3)
m[1,1]=0
m[1,2]=1
m[1,3]=1
m[2,1]=1
m[2,2]=0
m[2,3]=0
m[3,1]=0
m[3,2]=1
m[3,3]=0
m
lab=c(1,2,3)
object <- graph.adjacency(m,mode="directed") 
set.seed(1)
plot(object,vertex.label=lab)


#Example 1: Marriage and Power in 15th Century Florence

library(igraph) ## load the package 
## read the data
florence <- as.matrix(read.csv("C:/Users/Nas/Documents/R/firenze.csv"))
florence
help(graph.adjacency)

marriage <- graph.adjacency(florence,mode="undirected", diag=FALSE)
## use the help function to understand the options for the graph  
set.seed(1)
plot(marriage,layout=layout.fruchterman.reingold,vertex.label=V(marriage)$name,vertex.color="red",vertex.label.color="black", vertex.frame.color=0,vertex.label.cex=1.5)

data.frame(V(marriage)$name,degree(marriage))

## calculate and plot the shortest paths
V(marriage)$color <- 8
E(marriage)$color <- 8

PtoA <- get.shortest.paths(marriage, from="Peruzzi", to="Acciaiuoli")

E(marriage, path=PtoA[[1]])$color <- "magenta"

V(marriage)[PtoA[[1]]]$color <- "magenta"

GtoS <- get.shortest.paths(marriage, from="Ginori", to="Strozzi")

E(marriage, path=GtoS[[1]])$color <- "green"

V(marriage)[ GtoS[[1]] ]$color <- "green"

V(marriage)[ "Medici" ]$color <- "cyan"

set.seed(1)
plot(marriage,  layout=layout.fruchterman.reingold, vertex.label=V(marriage)$name,vertex.label.color="black", vertex.frame.color=0, vertex.label.cex=1.5)

data.frame(V(marriage)$name, betweenness(marriage))

