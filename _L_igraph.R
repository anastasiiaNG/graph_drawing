
# par(mar=c(1,1,1,1))
# layout_with_sugiyama #Error in norm_coords(layout, -1, 1, -1, 1) : `layout' not a matrix

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
layouts <- layouts[!grepl("bipartite|circle|sphere|grid|randomly|graphopt|dh|mds|star|tree|sugiyama", layouts)]

for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(m)) 
  plot(m, layout=l, main=layout, vertex.size = 3, vertex.label=NA, edge.label=NA) 
} 

layout1 <- layout_with_kk(m) #lgl, gem, ?? components
plot(m, layout=layout1, vertex.size = 3, vertex.label=NA, edge.label=NA)

layout1_iter10 <- layout_with_kk(m, coords = layout1, maxiter = 10)
plot(m, layout=layout1_iter10, vertex.size = 3, vertex.label=NA, edge.label=NA)

#' @param kkconst = vcount(graph) the Kamada-Kawai vertex attraction constant
#' @param many_other_for_\sQuote{y} coordinates: x, y. 0_o
#' @param https://github.com/cran/igraph/blob/master/man/layout_with_kk.Rd




###________________________norm_AND_components______________________________

# This way you can create more compact or spread out layout versions.
l <- norm_coords(layout1, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot(m, layout=l, vertex.size = 3, vertex.label=NA, edge.label=NA)
plot(m, rescale=F, layout=l*1.1, vertex.size = 3, vertex.label=NA, edge.label=NA)

layout_gephi_norm <- norm_coords(layout_gephi, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot(m, layout=layout_gephi_norm, vertex.size = 3, vertex.label=NA, edge.label=NA)
plot(m, rescale=F, layout=layout_gephi_norm*1.1, vertex.size = 3, vertex.label=NA, edge.label=NA)


lego_layout <- component_wise(merge_method = "layout_with_gem")
lego_layout <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot(m, layout=lego_layout, vertex.size = 3, vertex.label=NA, edge.label=NA)

library(dplyr)
# g <- make_ring(10) + make_ring(10)
m %>%
  add_layout_(with_lgl(), component_wise()) %>%
  plot(vertex.size = 3, vertex.label=NA, edge.label=NA)




###________________________how_to_create_easy_igraph______________________________
nodes <- c('a','b','c','d')
x <- c(9,1,2,3)
y <- c(0,1,2,3)
from <- c('a','b','c')
to <- c('b','c','d')
NodeList <- data.frame(nodes, x ,y)
EdgeList <- data.frame(from, to)
a <- graph_from_data_frame(vertices = NodeList, d= EdgeList, directed = FALSE)
plot(a)


igraph_from_gephi <- graph_from_data_frame(layout1, directed = F) #, vertices=name)
plot(igraph_from_gephi)
plot(igraph_from_gephi , layout=layout1)
g3 <- graph( c("John", "Jim", "Jim", "Jill", "Jill", "John"))
igraph_from_sif <- graph_from_edgelist(as.matrix(mydata[,c(1,2)]))

###__________________________tkplot_____________________________
# tkplot(layout1) # Not a graph object
# saveModuleToDot(igraph_from_gephi, file="igraph_from_gephi.dot", name="igraph_from_gephi")
tkplot(m, coords = layout1)
tkplot(igraph_from_gephi)
tkplot(igraph_from_sif)

