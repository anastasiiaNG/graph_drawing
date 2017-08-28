
setwd("~/R-studio/gatom")
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
library(Rcpp)


# folder <- list.dirs("graphs")
# load("graphs/andrea_mir146b/do_gatom.rda")
load("graphs/bart_dendritic_cells/do_gatom.rda")
# load("graphs/shabaana_tb765/do_gatom.rda")
# load("graphs/vicky_th17_itac_2017_06/do_gatom.rda")

graphs <- paste0("ms$", names(ms))
vars <-  paste0("graph", 1:length(graphs))
# do.call("<-",list(vars, eval(parse(text = list))))

for(i in 1:length(graphs)){
  assign(vars[i], eval(parse(text = graphs[i])))
}

m_native <- graph3 # 3, 1, 4
m <- m_native

# set_vertex_attr(m, "logPval", index = V(m), value = -6)
V(m)$logPval <- -6 # random number

# str(m)
# head(as_data_frame(m, what = "vertices"))
# head(as_data_frame(m))
# V(m)
# str(V(m))
# E(m)
# str(E(m))

# ________producing dummy_nodes in the middle of the edge__________

m <- add_vertices(m, length(E(m)), attr = list(
                                            name=paste0("dummy_node", 1:length(E(m))),
                                            label=E(m)$label,
                                            logPval=6 # to make it zero
                                            )
                  )



new_edges <- c()

for(i in seq(length(E(m)))){
  pair = ends(m, E(m)[i])
  # print(pair)
  new_edges = cbind(new_edges, 
                    pair[1], paste0("dummy_node", i), 
                    paste0("dummy_node", i), pair[2])
  sink("subgraphs1_bla.txt", append=TRUE)
  # cat(paste0("subgraph cluster", i, "{", '\n', pair[1], ";\n", paste0("dummy_node", i), ";\n", pair[2],
  #                   ";\n", "}\n"))
  cat(paste0("subgraph cluster", i, "{", '\n', pair[1], " -> ", paste0("dummy_node", i), " -> ", pair[2],
                     ";\n", "}\n"))
  sink()
}

new_edges <- as.vector(new_edges)
m <- add_edges(m, new_edges, attr = list())
m <- delete_edges(m, 1:length(E(m_native)))


#_________producing subgraphs_______________





m_native <- m

m <- add_vertices(m, length(E(m)), attr = list(
  name=paste0("dummy_node_2diter", 1:length(E(m))),
  logPval=6 # to make it zero
)
)

# i=1
new_edges <- c()
for(i in seq(length(E(m)))){
  pair = ends(m, E(m)[i])
  # print(pair)
  new_edges = cbind(new_edges, 
                    pair[1], paste0("dummy_node_2diter", i), 
                    paste0("dummy_node_2diter", i), pair[2])
}
new_edges <- as.vector(new_edges)
m <- add_edges(m, new_edges, attr = list())
m <- delete_edges(m, 1:length(E(m_native)))




m_native <- m

m <- add_vertices(m, length(E(m)), attr = list(
  name=paste0("dummy_node_3diter", 1:length(E(m))),
  logPval=6 # to make it zero
)
)


# i=1
new_edges <- c()
for(i in seq(length(E(m)))){
  pair = ends(m, E(m)[i])
  # print(pair)
  new_edges = cbind(new_edges, 
                    pair[1], paste0("dummy_node_3diter", i), 
                    paste0("dummy_node_3diter", i), pair[2])
}
new_edges <- as.vector(new_edges)
m <- add_edges(m, new_edges, attr = list())
m <- delete_edges(m, 1:length(E(m_native)))





x1 <- as_data_frame(m, what = "vertices")
x2 <- as_data_frame(m)

# 
# # i=2
# new_edges <- c()
# for(i in seq(length(E(m)))){
#   pair = ends(m, E(m)[i])
#   # print(pair)
#   new_edges = rbind(new_edges, paste0(pair[1], "--dummy_node", i))
#   new_edges = rbind(new_edges, paste0("dummy_node", i, "--", pair[2]))
# }
# 
# ends(m, E(m)[1]) #[1]


pdf("..\\..\\..\\Desktop\\SampleGraph_ggrepel_w_dummy_force0.05lessfont.pdf", width = 8.50, height = 11) #8.50 11

ggnet2(m, mode = layout1, 
       node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
       ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
       # node.label = V(m)$label, label.size = (produce_node_attrs$fontsize)/8, label.color = "grey10", #label.trim = 15,
       edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
       # edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize/6,
       legend.size = 0, legend.position = "up") +
  geom_text_repel(aes(label=V(m)$label), # angle = ifelse(produce_node_attrs$width == 0.5, 5, 0)), 
                  size = produce_node_attrs$fontsize/7.5, 
                  #family = ifelse(produce_node_attrs$width == 0.5, 'URWGothic','Calibri'),
                  #fontface = ifelse(produce_node_attrs$width == 0.5, 'bold.italic', 'plain'),
                  min.segment.length = unit(1.5, "lines"), segment.color = "grey", 
                  force = 0.05,  # max.iter = 10e3
                  # box.padding = unit(0.75, 'lines') #, label.size =5,
                  # point.padding = unit(5.6, 'lines') #, # Defaults to unit(0, "lines").
                  # label.padding = unit(0.7, 'lines') #, # Defaults to 0.25 lines.
                  ### nudge_x = ifelse(produce_node_attrs$width == 0.2, 2, 0),
                  ### nudge_y = ifelse(produce_node_attrs$width == 0.2, 2, 0)
  )
dev.off()

# options(stringsAsFactors = FALSE)
# class(as.character(E(m)[1]))
# class(ends(m, E(m)[1])[1,1])
# split(as.character(E(m)[1]), "--")
# split(E(m)[1], "--")

###______________________creating_attributes__________________________
upRamp <- colorRamp(c("#cccccc", "#ff3333"))
downRamp <- colorRamp(c("#cccccc", "green"))

getDotColor <- function(log2FC) {
  if (is.na(log2FC)) {
    return("#7777ff")
  }
  
  if (log2FC > 0) {
    col <- upRamp(min(1, abs(log2FC) / 2))
  } else {
    col <- downRamp(min(1, abs(log2FC) / 2))
  }
  rgb(col[,1], col[,2], col[,3], maxColorValue=255)
}

getDotSize <- function(logPval) {
  logPval[is.na(logPval)] <- 0
  return(pmin(0.2 - logPval/100/0.3, 0.5))
}

nodeShapeMap <- c(met="circle", rxn="square")
edgeStyleMap <- c(main="solid", trans="dashed")

getDotNodeStyleAttributes <- function(attrs) {
  with(attrs, data.frame(
    label=if (!is.null(attrs$label)) label else "",
    shape=if (!is.null(attrs$nodeType)) nodeShapeMap[nodeType] else "circle",
    fixedsize="shape",
    style="filled",
    width=if (!is.null(attrs$logPval)) sapply(logPval, getDotSize) else 4,
    # fontsize=if (!is.null(attrs$logPval))sapply(logPval, getDotSize) * 45 else 2,
    fontsize = 2,
    color=if (!is.null(attrs$log2FC)) sapply(log2FC, getDotColor) else "grey",
    fillcolor=if (!is.null(attrs$log2FC)) sapply(log2FC, getDotColor) else "white"
  ))
}


getDotEdgeStyleAttributes <- function(attrs) {
  logPval <- if (!is.null(attrs$logPval)) attrs$logPval else 1
  with(attrs, data.frame(
    label=if (!is.null(attrs$label)) label else "",
    style=if (!is.null(attrs$rptype)) edgeStyleMap[rptype] else "solid",
    penwidth=sapply(logPval, getDotSize) * 20,
    fontsize=sapply(logPval, getDotSize) * 45,
    color=if (!is.null(attrs$log2FC)) sapply(log2FC, getDotColor) else "grey"
  ))
}


# v <- as_data_frame(m2, what = "vertices"); str(v)
#rm(produce_edge_attrs, produce_node_attrs)

options(stringsAsFactors = FALSE)
produce_node_attrs <- getDotNodeStyleAttributes(as_data_frame(m, what = "vertices"))
produce_edge_attrs <- getDotEdgeStyleAttributes(as_data_frame(m))



produce_node_attrs$color
produce_node_attrs$width
produce_node_attrs$fontsize

produce_edge_attrs$color
produce_edge_attrs$penwidth
produce_edge_attrs$fontsize
