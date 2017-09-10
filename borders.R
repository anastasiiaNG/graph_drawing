
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


data("networkEx")
data("org.Mm.eg.gatom.annoEx")
data("met.kegg.dbEx")

data("met.de.rawEx")
data("gene.de.rawEx")

g <- makeAtomGraph(network=networkEx, 
                   org.gatom.anno=org.Mm.eg.gatom.annoEx, 
                   gene.de=gene.de.rawEx,
                   met.db=met.kegg.dbEx, 
                   met.de=met.de.rawEx)
print(g)

gs <- scoreGraph(g, k.gene = 25, k.met=25)

set.seed(42)
m <- solveSgmwcsRandHeur(gs, max.iterations = 2000)
m_native <- m

## >>>>> 1. create layout
layout1 <- layout_with_kk(m)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
layout1 <- range01(layout1)

## >>>>> 2. create df >lines_to_check< (taken from "intersections.R") where all necessary info is stored
nt <- as_data_frame(m, "vertices")
nt$x <- layout1[,1]
nt$y <- layout1[,2]
# head(layout1)
et <- as_data_frame(m, "edges")
i = 1
# node_start <- "C00022_0"
# which(nt$name==node_start) # 1
for(node_start in et$from){
  #print(node_start)
  et$x[i] <- nt$x[nt$name==node_start]
  #print(et$x[i])
  et$y[i] <- nt$y[nt$name==node_start]
  et$s_num[i] <- which(nt$name==node_start)
  i <- i + 1
}
i = 1
for(node_end in et$to){
  et$xend[i] <- nt$x[nt$name==node_end]
  et$yend[i] <- nt$y[nt$name==node_end]
  et$e_num[i] <- which(nt$name==node_end)
  i <- i + 1
}

# head(et)
# head(nt)
# str(nt)
# str(et)


lines_to_check <- data.frame(
  x = et$x,
  y = et$y,
  xend = et$xend,
  yend = et$yend,
  IDstart = as.character(et$from),
  IDend = as.character(et$to),
  IDs_num = et$s_num,
  IDe_num = et$e_num)

## >>>>> 3. dummy nodes at the middle of the edges 
lines_to_check$middle_x <- (lines_to_check$x + lines_to_check$xend) / 2
lines_to_check$middle_y <- (lines_to_check$y + lines_to_check$yend) / 2
#

## >>>>> 4. produce attributes 
options(stringsAsFactors = FALSE)
produce_node_attrs <- gatom:::getDotNodeStyleAttributes(as_data_frame(m, what = "vertices"))
produce_edge_attrs <- gatom:::getDotEdgeStyleAttributes(as_data_frame(m))

produce_node_attrs$fontsize <- produce_node_attrs$fontsize/6.5
produce_edge_attrs$fontsize <- produce_edge_attrs$fontsize/6

# str(lines_to_check)
# head(lines_to_check)
# str(et)
# str(produce_edge_attrs) # fontsize, label
# str(produce_node_attrs)

## >>>>> 5. make_content_for_REPEL_BOXES.cpp----------------
## >>>>> modifying https://github.com/slowkow/ggrepel/blob/06c6f29f3a5a258282d6f1caa5d5f120540efaaf/R/geom-text-repel.R
box.padding <- unit(1e-6, "lines")
box_padding_x <- convertWidth(box.padding, "npc", valueOnly = TRUE) # 7.649402e-08 VS 4.8 (1)
box_padding_y <- convertHeight(box.padding, "npc", valueOnly = TRUE) # ... -4.8


edges <- cbind(lines_to_check$IDs_num, lines_to_check$IDe_num)

nlabel_semisizes <- lapply(seq(produce_node_attrs$label), function(i) {
  
  t <- textGrob(produce_node_attrs$label[i],
                x=nt$x[i],
                y=nt$y[i],
                default.units = "npc",
                
                gp=gpar(fontsize=produce_node_attrs$fontsize[i] * .pt
                ),
                name = "text" #,
                # vp = viewport(xscale = c(0, 1), yscale = c(0, 1))
  )
  
  gw <- convertWidth(grobWidth(t), "npc", TRUE) / 4
  
  gh <- convertHeight(grobHeight(t), "npc", TRUE) / 10
  
  c(
    "semi_w" = gw + box_padding_x,
    "semi_h" = gh + box_padding_y
  )
})
nlabel_semisizes <- data.frame(do.call(rbind, nlabel_semisizes))

elabel_semisizes <- lapply(seq(produce_edge_attrs$label), function(i) {
  
  t <- textGrob(produce_edge_attrs$label[i],
                x=lines_to_check$middle_x[i],
                y=lines_to_check$middle_y[i],
                default.units = "npc",
                
                gp=gpar(fontsize=produce_edge_attrs$fontsize[i] * .pt
                ),
                name = "text"
  )
  
  gw <- convertWidth(grobWidth(t), "npc", TRUE) / 4
  
  gh <- convertHeight(grobHeight(t), "npc", TRUE) / 10
  
  c(
    "semi_w" = gw + box_padding_x,
    "semi_h" = gh + box_padding_y
  )
})
elabel_semisizes <- data.frame(do.call(rbind, elabel_semisizes))


render_ebefore1 <- function(layout1_x, layout1_y, label_semisizes, labels, fontsize) {
  
  grid.rect(x = layout1_x, y = layout1_y,
            width = label_semisizes[,1] * 2, height = label_semisizes[,2] * 2,
            # just = "right", hjust = NULL, vjust = NULL,
            default.units = "npc", name = NULL,
            
            gp=gpar() 
            # vp = viewport(layout.pos.row=2, layout.pos.col=2:3)
  )
  
  grid.text(labels, 
            x=layout1_x, 
            y=layout1_y, 
            default.units = "npc",
            
            gp=gpar(fontsize=fontsize * .pt 
            ),  
            name = "text" #,
            # vp = viewport(xscale = c(0, 1), yscale = c(0, 1))
  )
}

dev.off()
render_ebefore1(lines_to_check$middle_x, lines_to_check$middle_y, elabel_semisizes, 
                E(m)$label, fontsize = produce_edge_attrs$fontsize)
render_ebefore1(layout1[,1], layout1[,2], nlabel_semisizes, 
                V(m)$label, fontsize = produce_node_attrs$fontsize)

