# loading karate dataset
library(igraph)

d <- read.table("les_miserables.txt")
g=graph.adjacency(as.matrix(d),mode="undirected",weighted=NULL,diag=FALSE) 
plot.igraph(g)