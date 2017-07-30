# accepts as inputs an igraph object or a data frame

# In if (!attr %in% edge_attr_names(graph)) { :
#   the condition has length > 1 and only the first element will be used
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

layout_gephi_pos <- layout.forceatlas2(m2, directed=FALSE, iterations = 4000, 
                                       plotstep=1000, plotlabels=FALSE,
                                       linlog = FALSE, nohubs = TRUE, pos = layout1, 
                                       k = 400, ks=0.1, ksmax=10,
                                       gravity=30, center=NULL, delta = 1, 
                                       tolerance = 100, dim = 2)
plot(m, layout=layout_gephi_pos, vertex.size = 3, vertex.label=NA, edge.label=NA)


plot(m, layout=layout1, vertex.size = 3, vertex.label=NA, edge.label=NA)

layout1 <- layout_gephi



###_________________________EXAMPLE_____________________________
# from # help("layout.forceatlas2")
g <- graph.ring(50)
layout0 <- layout.forceatlas2(g, iterations=3000, plotstep=500)
plot(g, layout=layout0)

###______________________FROM dataframe__________________________
mydata <-  read.table("example.sif")
colnames(mydata) <- c("from", "weight", "to")
mydata$weight <- as.numeric(mydata$weight)
# здесь связи между вершинами
mydata <- mydata[,c(1,3,2)]
str(mydata)
# здесь координаты вершин
layout2 <- layout.forceatlas2(m2, directed=FALSE, iterations = 4000, 
                              plotstep=1000, plotlabels=FALSE,
                              linlog = FALSE, nohubs = TRUE, pos = NULL, 
                              k = 400, ks=0.1, ksmax=10,
                              gravity=30, center=NULL, delta = 1, 
                              tolerance = 100, dim = 2)
# omg
plot(mydata, layout=layout2)

