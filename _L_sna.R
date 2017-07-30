
net = intergraph::asNetwork(m)
xy <- network::as.matrix.network.adjacency(net)
layout_vector <- c("fruchtermanreingold", "kamadakawai", "spring", "circle", "eigen", "hall", "mds", "princoord", "target")

for (i in 1:9) {
  gplot(xy, gmode="graph", vertex.col="black", mode=layout_vector[i])
  title(sub=layout_vector[i], line=0.5)
}

#' @param layour.par from sna package ..>> how to snap them(see screenshot)
# ?gplot.layout # as in github: mode is equivalent to gplot.layout (на скриншотах код из гитхаба)


ggnet2(m, mode = "fruchtermanreingold", layout.par = list(niter = 100, #cell.jitter = 0.75,
                                                          seed.coord = layout1), 
       node.size = 3, node.color = "black", edge.size = 1, edge.color = "grey") # cameo
#' @param niter default 500
#' @param max.delta Sets the maximum change in position for any given iteration 
#' etc

ggnet2(m, mode = "kamadakawai", layout.par = list(niter = 1500), 
       node.size = 3, node.color = "black", edge.size = 1, edge.color = "grey")
#' @param niter default 500
#' @param sigma Sets the base standard deviation of position change proposals. NROW(d)/4.
#' @param kkconst Sets the Kamada-Kawai vertex attraction constant. NROW(d)^2.


##__________________________extract_layout___________________________
net = intergraph::asNetwork(m)
xy <- network::as.matrix.network.adjacency(net)
layout1 <- gplot.layout.kamadakawai(xy, layout.par = list(niter = 500))

plot(m, layout=layout_from_gg2, vertex.size = 3, vertex.label=NA, edge.label=NA)

#OR
layout_test <- ggnet2(m, mode = "kamadakawai", layout.par = list(niter = 1000, seed.coord = layout1), 
                      node.size = 3, node.color = "black", edge.size = 1, edge.color = "grey")
str(layout_test)
# layout_from_gg2_2 <- cbind(layout_test$data$x, layout_test$data$y)

##_____________________________________________________________________
ggnet2(m, mode = layout.forceatlas2(m2, directed=FALSE, iterations = 4000, 
                                    plotstep=1000, plotlabels=FALSE,
                                    linlog = FALSE, nohubs = TRUE, pos = layout1, 
                                    k = 400, ks=0.1, ksmax=10,
                                    gravity=30, center=NULL, delta = 1, 
                                    tolerance = 100, dim = 2))