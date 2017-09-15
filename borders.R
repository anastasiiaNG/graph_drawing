
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
png(filename="../../../Desktop/device.png", res = 144, width = 8.5, height = 11, units = "in")

## >>>>> 0. create graph
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
layout1[,1] <- range01(layout1[,1])
layout1[,2] <- range01(layout1[,2])

## >>>>> 2. create df >lines_to_check< (taken from "intersections.R") where all necessary info is stored
store_all_info <- function(m, layout1){

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

  lines_to_check <- data.frame(
    x = et$x,
    y = et$y,
    xend = et$xend,
    yend = et$yend,
    IDstart = as.character(et$from),
    IDend = as.character(et$to),
    IDs_num = et$s_num,
    IDe_num = et$e_num)

  lines_to_check$middle_x <- (lines_to_check$x + lines_to_check$xend) / 2
  lines_to_check$middle_y <- (lines_to_check$y + lines_to_check$yend) / 2

  return(list(
    nt=nt,
    et=et,
    lines_to_check=lines_to_check)
  )
}

nt <- store_all_info(m, layout1)$nt
et <- store_all_info(m, layout1)$et
lines_to_check <- store_all_info(m, layout1)$lines_to_check


## >>>>> 3. produce attributes
options(stringsAsFactors = FALSE)
produce_node_attrs <- gatom:::getDotNodeStyleAttributes(as_data_frame(m, what = "vertices"))
produce_edge_attrs <- gatom:::getDotEdgeStyleAttributes(as_data_frame(m))

produce_node_attrs$fontsize <- produce_node_attrs$fontsize/6.5
produce_edge_attrs$fontsize <- produce_edge_attrs$fontsize/6

# str(produce_edge_attrs) # fontsize, label
# str(produce_node_attrs)


## >>>>> 4. make_content_for_REPEL_BOXES.cpp----------------
## >>>>> modifying https://github.com/slowkow/ggrepel/blob/06c6f29f3a5a258282d6f1caa5d5f120540efaaf/R/geom-text-repel.R
# box.padding <- unit(0, "lines")
box.padding <- unit(0.04 * produce_node_attrs$fontsize, "lines")
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

  gw <- convertWidth(grobWidth(t), "npc", TRUE) / 2

  gh <- convertHeight(grobHeight(t), "npc", TRUE) / 2

  c(
    "semi_w" = gw + box_padding_x[i],
    "semi_h" = gh + box_padding_y[i]
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

  gw <- convertWidth(grobWidth(t), "npc", TRUE) / 2

  gh <- convertHeight(grobHeight(t), "npc", TRUE) / 2

  c(
    "semi_w" = gw + box_padding_x[i],
    "semi_h" = gh + box_padding_y[i]
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

## >>>>> 5. rescale layout1 to put all boxes inside [0:1]
# new_borders_x:
new_borders_x_bot <- max(nlabel_semisizes$semi_w) # 0.1245814
new_borders_x_top <- 1 - max(nlabel_semisizes$semi_w)
rescale_par_x <- new_borders_x_top - new_borders_x_bot # 1 - 2 * max(nlabel_semisizes$semi_w)

# new_borders_h:
new_borders_y_bot <- max(nlabel_semisizes$semi_h) # 0.007567988
new_borders_y_top <- 1 - max(nlabel_semisizes$semi_h)
rescale_par_y <- new_borders_y_top - new_borders_y_bot

layout1[,1] <- new_borders_x_bot + layout1[,1] * rescale_par_x
layout1[,2] <- new_borders_y_bot + layout1[,2] * rescale_par_y

# recalculate:
nt <- store_all_info(m, layout1)$nt
et <- store_all_info(m, layout1)$et
lines_to_check <- store_all_info(m, layout1)$lines_to_check


## >>>>> 6. look at the boxes before force_alg
# dev.off()
# grid.newpage()
# png(filename="../../../Desktop/device.png", res = 144, width = 8.5, height = 11, units = "in")
render_ebefore1(lines_to_check$middle_x, lines_to_check$middle_y, elabel_semisizes,
                E(m)$label, fontsize = produce_edge_attrs$fontsize)
render_ebefore1(layout1[,1], layout1[,2], nlabel_semisizes,
                V(m)$label, fontsize = produce_node_attrs$fontsize)


dev.off()
