
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
# load("graphs/bart_dendritic_cells/do_gatom.rda")
# load("graphs/shabaana_tb765/do_gatom.rda")
load("graphs/vicky_th17_itac_2017_06/do_gatom.rda")

graphs <- paste0("ms$", names(ms))
vars <-  paste0("graph", 1:length(graphs))
# do.call("<-",list(vars, eval(parse(text = list))))

for(i in 1:length(graphs)){
  assign(vars[i], eval(parse(text = graphs[i])))
}

m <- graph3 # 3, 1, 4


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

getDotNodeStyleAttributes <- function(attrs) {
  with(attrs, data.frame(
    label=if (!is.null(attrs$label)) label else "",
    shape=if (!is.null(attrs$nodeType)) nodeShapeMap[nodeType] else "circle",
    fixedsize="true",
    style="filled",
    width=if (!is.null(attrs$logPval)) sapply(logPval, getDotSize) else 4 ,
    fontsize=if (!is.null(attrs$logPval))sapply(logPval, getDotSize) * 45 else 2,
    color=if (!is.null(attrs$log2FC)) sapply(log2FC, getDotColor) else "grey",
    fillcolor=if (!is.null(attrs$log2FC)) sapply(log2FC, getDotColor) else "white"
  ))
}

# v <- as_data_frame(m2, what = "vertices"); str(v)
# v <- as_data_frame(m, what = "vertices"); str(v)
#rm(produce_edge_attrs, produce_node_attrs)

options(stringsAsFactors = FALSE)
produce_node_attrs <- getDotNodeStyleAttributes(as_data_frame(m, what = "vertices"))
produce_edge_attrs <- gatom:::getDotEdgeStyleAttributes(as_data_frame(m))



produce_node_attrs$color
produce_node_attrs$width
produce_node_attrs$fontsize

produce_edge_attrs$color
produce_edge_attrs$penwidth
produce_edge_attrs$fontsize
