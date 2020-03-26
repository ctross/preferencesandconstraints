 




m_graph <- graph_from_adjacency_matrix( model_dat_Coast$Friends , mode="directed" )


ZZ <- model_dat_Coast$Transfer
diag(ZZ) <- 0
m_graph2 <- graph_from_adjacency_matrix( ZZ , mode="directed" )


size <- model_dat_Coast$GoodsValue*13 + 2
size[is.na(size)] <- mean(size, na.rm = T)

color <- ifelse(model_dat_Coast$Indigenous == 1, "turquoise4", "gray13")

# plot true out network
Cairo(file="Nets1.png", 
      type="png",
      units="in", 
      width=12, 
      height=12, 
      pointsize=36, 
      dpi=150)

#plot(m_graph, vertex.color=groups , main="posterior mean" )
#Pretty but not sure if super informative 
g <- m_graph
m <- get.edgelist(g)
m2 <- mapply(m, FUN=as.numeric)
m <- matrix(data=m2, ncol=ncol(m), nrow=nrow(m))
l <- qgraph.layout.fruchtermanreingold(m,vcount=vcount(g))

par(mar=c(1,1,1,1)+0)
plot(m_graph , vertex.color=color , vertex.size = size, edge.arrow.size =0.36, edge.width=3,
     edge.curved = 0.35, vertex.label = NA, seed = 1,layout=l, vertex.frame.color=NA,
     main="Friendship")
dev.off()

Cairo(file="Nets2.png", 
      type="png",
      units="in", 
      width=12, 
      height=12, 
      pointsize=36, 
      dpi=150)
g <- m_graph2
m <- get.edgelist(g)
m2 <- mapply(m, FUN=as.numeric)
m <- matrix(data=m2, ncol=ncol(m), nrow=nrow(m))
l <- qgraph.layout.fruchtermanreingold(m,vcount=vcount(g))
par(mar=c(1,1,1,1)+0)
plot(m_graph2 , vertex.color=color , vertex.size = size,  edge.arrow.size =0.36, edge.width=3,
     edge.curved = 0.35, vertex.label = NA, seed = 1,layout=l,vertex.frame.color=NA,
     main="Resource Transfers")
dev.off()



# Load in packages 



ZZ <- model_dat_Coast$Giving
diag(ZZ) <- 0
m_graphA1 <- graph_from_adjacency_matrix( ZZ , mode="directed" )

ZZ <- model_dat_Coast$Taking
diag(ZZ) <- 0
m_graphA2 <- graph_from_adjacency_matrix( ZZ , mode="directed" )

ZZ <- model_dat_Coast$Reducing
diag(ZZ) <- 0
m_graphA3 <- graph_from_adjacency_matrix( ZZ , mode="directed" )


size <- model_dat_Coast$GoodsValue*13 + 2
size[is.na(size)] <- mean(size, na.rm = T)

color <- ifelse(model_dat_Coast$Indigenous == 1, "turquoise4", "gray13")

# plot true out network
Cairo(file="Nets3.png", 
      type="png",
      units="in", 
      width=12, 
      height=12, 
      pointsize=36, 
      dpi=150)

#plot(m_graph, vertex.color=groups , main="posterior mean" )
#Pretty but not sure if super informative 
g <- m_graphA1
m <- get.edgelist(g)
m2 <- mapply(m, FUN=as.numeric)
m <- matrix(data=m2, ncol=ncol(m), nrow=nrow(m))
l <- qgraph.layout.fruchtermanreingold(m,vcount=vcount(g))

par(mar=c(1,1,1,1)+0)
plot(m_graphA1 , vertex.color=color , vertex.size = size,  edge.arrow.size =0.36, edge.width=3,
     edge.curved = 0.35, vertex.label = NA, seed = 1,layout=l, vertex.frame.color=NA,
     main="Giving Game")
dev.off()

Cairo(file="Nets4.png", 
      type="png",
      units="in", 
      width=12, 
      height=12, 
      pointsize=36, 
      dpi=150)

g <- m_graphA2
m <- get.edgelist(g)
m2 <- mapply(m, FUN=as.numeric)
m <- matrix(data=m2, ncol=ncol(m), nrow=nrow(m))
l <- qgraph.layout.fruchtermanreingold(m,vcount=vcount(g))

par(mar=c(1,1,1,1)+0)
plot(m_graphA2 , vertex.color=color , vertex.size = size,  edge.arrow.size =0.36, edge.width=3,
     edge.curved = 0.35, vertex.label = NA, seed = 1,layout=l,vertex.frame.color=NA,
     main="Leaving Game")
dev.off()

Cairo(file="Nets5.png", 
      type="png",
      units="in", 
      width=12, 
      height=12, 
      pointsize=36, 
      dpi=150)
g <- m_graphA3
m <- get.edgelist(g)
m2 <- mapply(m, FUN=as.numeric)
m <- matrix(data=m2, ncol=ncol(m), nrow=nrow(m))
l <- qgraph.layout.fruchtermanreingold(m,vcount=vcount(g))
par(mar=c(1,1,1,1)+0)
plot(m_graphA3 , vertex.color=color , vertex.size = size, edge.arrow.size =0.36, edge.width=3,
     edge.curved = 0.35, vertex.label = NA, seed = 1,layout=l,vertex.frame.color=NA,
     main="Reducing Game")

dev.off()


