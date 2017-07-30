library(gatom)
library(data.table)
library(igraph)
library(ForceAtlas2)
library(ggnet)
library(network)
library(sna)
library(ggplot2)
library(ggrepel)
library(grid)

m

render(m, layout="igraph_kk", extra_tuning=T)
#' @param layout "igraph_kk", "sna_kamadakawai", //"gephi"
#' //@param layout_alg_for_tuning (compacting): lgl or gem (igraph), fruchtermanreingold (sna), gephi
#' @param extra_tuning=F: layout -> ggrepel -> nodes to ggrepel
#' @param extra_tuning=T: layout -> ggrepel -> nodes to ggrepel -> slight layout -> ggrepel -> nodes to ggrepel

# + intersection check


render <- function(m, layout, extra_tuning){
  
  # pdf("SampleGraph.pdf", width = 8.50, height = 11) #8.50 11
  
  # produce aes
  options(stringsAsFactors = FALSE)
  produce_node_attrs <- gatom:::getDotNodeStyleAttributes(as_data_frame(m, what = "vertices"))
  produce_edge_attrs <- gatom:::getDotEdgeStyleAttributes(as_data_frame(m))
  
  # produce layout
  if(layout == "igraph_kk"){layout1 <- layout_with_kk(m)}
  # "layout_nicely"
  # "layout_with_drl"
  # "layout_with_fr"
  # "layout_with_gem"
  # "layout_with_kk"
  # "layout_with_lgl"
  
  if(layout == "sna_kamadakawai"){
    net = intergraph::asNetwork(m)
    xy <- network::as.matrix.network.adjacency(net)
    layout1 <- gplot.layout.kamadakawai(xy, layout.par = list(niter = 500))}
  # "fruchtermanreingold"
  
  if(layout == "gephi"){
    m2 <- m
    edge.attributes(m2) <-  list()
    layout1 <- layout.forceatlas2(m2, directed=FALSE, iterations = 4000, 
                                  plotstep=1000, plotlabels=FALSE,
                                  linlog = FALSE, nohubs = TRUE, pos = NULL, 
                                  k = 400, ks=0.1, ksmax=10,
                                  gravity=30, center=NULL, delta = 1, 
                                  tolerance = 100, dim = 2)
    #' @param pos A data frame or matrix (NumberOfNodes x dimension) of the initial locations of points.
    #' @param k repel constant: repulsion between points
    #' @param ks speed of nodes under acting forse //when 9 - nodes don't move at all 0_o
    #' @param center Center of gravity: NULL or c(0,0)
    #' @param tolerance ??? Tolerance to swinging constant.
    #' @param linlog If TRUE the algorithm uses logarithmic attraction force `F <- log (1+F)`
    #' @param nohubs If TRUE nodes with high indegree have more central position than nodes with 
    #' outdegree (for directed graphs). // strange, in 'm' it still matters
    #' @param delta Modify attraction force, the weights are raised to the power of `delta`
  }
  
  # first render and ggrepel
  
  pdf("SampleGraph.pdf", width = 8.50, height = 11) #8.50 11
  
  p1 <- ggnet2(m, mode = layout1, 
         node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
         ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
         # node.label = V(m)$label, label.size = (produce_node_attrs$fontsize)/8, label.color = "grey10", #label.trim = 15,
         edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
         edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize/6,
         legend.size = 0, legend.position = "up") +
    geom_text_repel(aes(label=V(m)$label), # angle = ifelse(produce_node_attrs$width == 0.5, 5, 0)), 
                    size = produce_node_attrs$fontsize/6.5, 
                    #family = ifelse(produce_node_attrs$width == 0.5, 'URWGothic','Calibri'),
                    #fontface = ifelse(produce_node_attrs$width == 0.5, 'bold.italic', 'plain'),
                    min.segment.length = unit(1.5, "lines"), segment.color = "grey", 
                    force = 0.05,  # max.iter = 10e3
                    box.padding = unit(0.75, 'lines') #, label.size =5,
                    # point.padding = unit(5.6, 'lines') #, # Defaults to unit(0, "lines").
                    # label.padding = unit(0.7, 'lines') #, # Defaults to 0.25 lines.
                    ### nudge_x = ifelse(produce_node_attrs$width == 0.2, 2, 0),
                    ### nudge_y = ifelse(produce_node_attrs$width == 0.2, 2, 0)
    )
  print(p1)
  
  
  grid.force()
  
  pos <- grid.get("textrepelgrob", grep = TRUE, global = TRUE)
  
  # Coordinates on the [0,1] scale (not the original scale of the data).
  pos <- data.frame(
    x = sapply(pos, "[[", "x"),
    y = sapply(pos, "[[", "y"),
    x.orig = sapply(pos,"[[", "x.orig"),
    y.orig = sapply(pos,"[[", "y.orig")
  )
  
  
  layout1[ ,1] <- pos$x
  layout1[ ,2] <- pos$y
  
  p2 <- ggnet2(m, mode = layout1, 
         node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
         ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
         node.label = V(m)$label, label.size = (produce_node_attrs$fontsize)/6.5, label.color = "grey13", #label.trim = 15,
         edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
         edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize/6,
         legend.size = 0, legend.position = "up") 
  
  print(p2)
  
  
  if(extra_tuning == T){
    
    if(layout == "igraph_kk"){
      layout_next <- layout_with_kk(m, coords = layout1, maxiter = 50 * vcount(m))
      #' @param kkconst the Kamada-Kawai vertex attraction constant
      # DEFAULT: maxiter = 50 * vcount(m), kkconst = vcount(m)
      }
    
    if(layout == "sna_kamadakawai"){
      layout_next <- gplot.layout.kamadakawai(xy, layout.par = list(seed.coord = layout1,
                                                                    niter = 500, 
                                                                    sigma = NROW(m)/16,
                                                                    kkconst = NROW(m)^2))
      #' @param niter default 500
      #' @param sigma Sets the base standard deviation of position change proposals. NROW(d)/4.
      #' @param kkconst Sets the Kamada-Kawai vertex attraction constant. NROW(d)^2.
      #' 
      #' FOR "fruchtermanreingold":
      #' @param niter default 500
      #' @param max.delta Sets the maximum change in position for any given iteration 
      }
      
    
    p3 <- ggnet2(m, mode = layout_next, 
           node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
           ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
           node.label = V(m)$label, label.size = (produce_node_attrs$fontsize)/6.5, label.color = "grey13", #label.trim = 15,
           edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
           edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize/6,
           legend.size = 0, legend.position = "up") 
    
    print(p3)
    
    
    p4 <- ggnet2(m, mode = layout_next, 
           node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
           ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
           # node.label = V(m)$label, label.size = (produce_node_attrs$fontsize)/8, label.color = "grey10", #label.trim = 15,
           edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
           edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize/6,
           legend.size = 0, legend.position = "up") +
      geom_text_repel(aes(label=V(m)$label), # angle = ifelse(produce_node_attrs$width == 0.5, 5, 0)), 
                      size = produce_node_attrs$fontsize/6.5, 
                      #family = ifelse(produce_node_attrs$width == 0.5, 'URWGothic','Calibri'),
                      #fontface = ifelse(produce_node_attrs$width == 0.5, 'bold.italic', 'plain'),
                      min.segment.length = unit(1.5, "lines"), segment.color = "grey", 
                      force = 0.05,  # max.iter = 10e3
                      box.padding = unit(0.75, 'lines') #, label.size =5,
                      # point.padding = unit(5.6, 'lines') #, # Defaults to unit(0, "lines").
                      # label.padding = unit(0.7, 'lines') #, # Defaults to 0.25 lines.
                      ### nudge_x = ifelse(produce_node_attrs$width == 0.2, 2, 0),
                      ### nudge_y = ifelse(produce_node_attrs$width == 0.2, 2, 0)
      )
    
    print(p4)
    
    grid.force()
    
    pos <- grid.get("textrepelgrob", grep = TRUE, global = TRUE)
    
    # Coordinates on the [0,1] scale (not the original scale of the data).
    pos <- data.frame(
      x = sapply(pos, "[[", "x"),
      y = sapply(pos, "[[", "y"),
      x.orig = sapply(pos,"[[", "x.orig"),
      y.orig = sapply(pos,"[[", "y.orig")
    )
    
    
    layout_next[ ,1] <- pos$x
    layout_next[ ,2] <- pos$y
    
    p5 <- ggnet2(m, mode = layout_next, 
           node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
           ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
           node.label = V(m)$label, label.size = (produce_node_attrs$fontsize)/6.5, label.color = "grey13", #label.trim = 15,
           edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
           edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize/6,
           legend.size = 0, legend.position = "up") 
    
    print(p5)
    
  }
  dev.off()
  
}
