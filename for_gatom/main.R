library(gatom)
library(igraph)
library(data.table)
library(ggnet)
library(network)
library(intergraph)
library(sna)
library(ggplot2)
library(ggrepel)
library(grid)
library(Rcpp)
# m

system.time({ # approx. 12s

source("./utils.R")
source("./force_alg.R")

# 1.MAKE LAYOUT
layout1 <- make_layout(m, layout="sna_kk")

# 2.PRODUCE LABELS' GROBS ON CANVAS OF A PARTICULAR SIZE
### >>>>> modifying https://github.com/slowkow/ggrepel_..._geom-text-repel.R
gwidth <- 2 * (max(layout1[,1]) - min(layout1[,1]))
gheight <- 2 * (max(layout1[,2]) - min(layout1[,2]))
layout1 <- range01(layout1)
pdf(file="./device.pdf", width = gwidth, height = gheight)

### >>>>> produce aes
options(stringsAsFactors = FALSE)
produce_node_attrs <- gatom:::getDotNodeStyleAttributes(as_data_frame(m, what = "vertices"))
produce_edge_attrs <- gatom:::getDotEdgeStyleAttributes(as_data_frame(m))

produce_node_attrs$fontsize <- produce_node_attrs$fontsize/2
produce_edge_attrs$fontsize <- produce_edge_attrs$fontsize/2.5

produce_edge_attrs$penwidth <- produce_edge_attrs$penwidth/2
produce_node_attrs$width <- produce_node_attrs$width/2

box.padding <- unit(0.07 * produce_node_attrs$fontsize, "lines")
box_padding_x <- convertWidth(box.padding, "npc", valueOnly = TRUE)
box_padding_y <- convertHeight(box.padding, "npc", valueOnly = TRUE)

edges <- cbind(store_all_info(m, layout1)$lines_to_check$IDs_num,
               store_all_info(m, layout1)$lines_to_check$IDe_num)

### >>>>> produce grobs
nlabel_semisizes <- lapply(seq(produce_node_attrs$label), function(i) {

  t <- textGrob(produce_node_attrs$label[i],
                x=store_all_info(m, layout1)$nt$x[i],
                y=store_all_info(m, layout1)$nt$y[i],
                default.units = "npc",

                gp=gpar(fontsize=produce_node_attrs$fontsize[i] * .pt
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
nlabel_semisizes <- data.frame(do.call(rbind, nlabel_semisizes))

elabel_semisizes <- lapply(seq(produce_edge_attrs$label), function(i) {

  t <- textGrob(produce_edge_attrs$label[i],
                x=store_all_info(m, layout1)$lines_to_check$middle_x[i],
                y=store_all_info(m, layout1)$lines_to_check$middle_y[i],
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

### >>>>> rescale layout1 to put all boxes inside [0:1]
# new_borders_x:
new_borders_x_bot <- max(nlabel_semisizes$semi_w)
new_borders_x_top <- 1 - max(nlabel_semisizes$semi_w)
rescale_par_x <- new_borders_x_top - new_borders_x_bot

# new_borders_h:
new_borders_y_bot <- max(nlabel_semisizes$semi_h)
new_borders_y_top <- 1 - max(nlabel_semisizes$semi_h)
rescale_par_y <- new_borders_y_top - new_borders_y_bot

layout1[,1] <- new_borders_x_bot + layout1[,1] * rescale_par_x
layout1[,2] <- new_borders_y_bot + layout1[,2] * rescale_par_y

dev.off()

# 3.PUT LABELS ON THE PLOT
layout2 <- force_alg(layout1,
                     nlabel_semisizes,
                     elabel_semisizes,
                     edges,
                     n_iter = 100, force = 1e-5)

    pdf(file="./graph.pdf", width = gwidth, height = gheight)
    plot(ggnet2(m, mode = layout2$layouts[[length(layout2$layouts)]], layout.exp = 0.2*(60/length(V(m))),
                size = produce_node_attrs$width, max_size = 25, node.color = produce_node_attrs$color,
                node.label = V(m)$label, label.size = produce_node_attrs$fontsize, label.color = "grey13", #label.trim = 15,
                edge.size = produce_edge_attrs$penwidth, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
                edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize,
                legend.size = 0, legend.position = "up"))
    dev.off()

})
