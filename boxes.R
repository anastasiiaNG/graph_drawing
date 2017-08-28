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


elabel_boxes <- lapply(seq(produce_edge_attrs$label), function(i) {
  
  t <- textGrob(produce_edge_attrs$label[i], 
                x=lines_to_check$middle_x[i], 
                y=lines_to_check$middle_y[i], 
                default.units = "npc",
                
                gp=gpar(fontsize=produce_edge_attrs$fontsize[i] * .pt 
                ),  
                name = "text"
  )
  
  gw <- convertWidth(grobWidth(t), "npc", TRUE) / 3 
  
  gh <- convertHeight(grobHeight(t), "npc", TRUE) / 3
  
  
  c(
    "x1" = lines_to_check$middle_x[i] - gw - box_padding_x,
    "y1" = lines_to_check$middle_y[i] - gh - box_padding_y,
    "x2" = lines_to_check$middle_x[i] + gw + box_padding_x,
    "y2" = lines_to_check$middle_y[i] + gh + box_padding_y
  )
  
})
elabel_boxes <- do.call(rbind, elabel_boxes)


nlabel_boxes <- lapply(seq(produce_node_attrs$label), function(i) {
  
  t <- textGrob(produce_node_attrs$label[i], 
                x=nt$x[i], 
                y=nt$y[i], 
                default.units = "npc",
                
                gp=gpar(fontsize=produce_node_attrs$fontsize[i] * .pt 
                ),  
                name = "text" #,
                # vp = viewport(xscale = c(0, 1), yscale = c(0, 1))
  )
  
  
  gw <- convertWidth(grobWidth(t), "npc", TRUE) / 3 
  
  gh <- convertHeight(grobHeight(t), "npc", TRUE) / 3
  
  
  c(
    "x1" = nt$x[i] - gw - box_padding_x,
    "y1" = nt$y[i] - gh - box_padding_y,
    "x2" = nt$x[i] + gw + box_padding_x,
    "y2" = nt$y[i] + gh + box_padding_y
  )
})
nlabel_boxes <- do.call(rbind, nlabel_boxes)


edges <- cbind(lines_to_check$IDs_num, lines_to_check$IDe_num)

# extra params in reserve
# k_xy <- produce_edge_attrs$fontsize/100
# len_y <- produce_edge_attrs$fontsize/10
# len_x <- nchar(produce_edge_attrs$label)/10
# plot.new()
# gw <- strwidth(produce_edge_attrs$label[i])/10
# gh <- strheight(produce_edge_attrs$label[i])/10


## >>>>> 5.1. visualize_content_for_REPEL_BOXES.cpp----------------
plot(NA, xlim=c(0, 1), ylim=c(0, 1), xlab="x", ylab="y")
rect(elabel_boxes[,1][c(1,2)], elabel_boxes[,2][c(1,2)], elabel_boxes[,3][c(1,2)], elabel_boxes[,4][c(1,2)])
rect(elabel_boxes[,1], elabel_boxes[,2], elabel_boxes[,3], elabel_boxes[,4])

rect(nlabel_boxes[,1][c(1,2)], nlabel_boxes[,2][c(1,2)], nlabel_boxes[,3][c(1,2)], nlabel_boxes[,4][c(1,2)])
rect(nlabel_boxes[,1], nlabel_boxes[,2], nlabel_boxes[,3], nlabel_boxes[,4])

rect(centers_of_RB2[,1], centers_of_RB2[,2], centers_of_RB2[,3], centers_of_RB2[,4])


render_after <- function(rect, labels, fontsize) {
  
  width <- rect[,3] - rect[,1]
  height <- rect[,4] - rect[,2]
  
  grid.rect(x = rect[,1] + width /2, y = rect[,2] + height /2,
          width = width, height = height,
          # just = "right", hjust = NULL, vjust = NULL,
          default.units = "npc", name = NULL,
          
          gp=gpar() 
          # vp = viewport(layout.pos.row=2, layout.pos.col=2:3)
          )
  
  grid.text(labels, 
          x=rect[,1] + width /2, 
          y=rect[,2] + height /2, 
          default.units = "npc",
          
          gp=gpar(fontsize=fontsize * .pt 
          ),  
          name = "text" #,
          # vp = viewport(xscale = c(0, 1), yscale = c(0, 1))
  )
}

dev.off()
render_after(centers_of_RB2, produce_node_attrs$label, produce_node_attrs$fontsize)
dev.off()
render_after(nlabel_boxes, produce_node_attrs$label, produce_node_attrs$fontsize)
dev.off()
render_after(elabel_boxes, produce_edge_attrs$label, produce_edge_attrs$fontsize)


render_ebefore <- function(rect, centers_labels_x, centers_labels_y, labels, fontsize) {
  
  width <- rect[,3] - rect[,1]
  height <- rect[,4] - rect[,2]
  
  grid.rect(x = rect[,1] + width /2, y = rect[,2] + height /2,
            width = width, height = height,
            # just = "right", hjust = NULL, vjust = NULL,
            default.units = "npc", name = NULL,
            
            gp=gpar() 
            # vp = viewport(layout.pos.row=2, layout.pos.col=2:3)
  )
  
  grid.text(labels, 
            x=centers_labels_x, 
            y=centers_labels_y, 
            default.units = "npc",
            
            gp=gpar(fontsize=fontsize * .pt 
            ),  
            name = "text" #,
            # vp = viewport(xscale = c(0, 1), yscale = c(0, 1))
  )
}

dev.off()
render_ebefore(elabel_boxes, lines_to_check$middle_x, lines_to_check$middle_y, 
              produce_edge_attrs$label, produce_edge_attrs$fontsize)

# str(nlabel_boxes)
# head(nlabel_boxes)
# str(elabel_boxes)
# head(elabel_boxes)
# str(edges)
# head(edges)

## >>>>> 6. call Rcpp----------------
# model data:
##xlim <- c(0, 1)
##ylim <- c(0, 1)
##set.seed(22)
##boxes <- cbind(rnorm(57), rnorm(57), rnorm(57), rnorm(57))

pdf("..\\..\\..\\Desktop\\_Graph_pre.pdf", width = 8.50, height = 11)
ggnet2(m, mode = layout1, 
       node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
       ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
       node.label = V(m)$label, label.size = produce_node_attrs$fontsize, label.color = "grey13", #label.trim = 15,
       edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
       edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize,
       legend.size = 0, legend.position = "up") 
dev.off()

sourceCpp("repel_boxes.cpp")
centers_of_RB <- repel_boxes(data_points=layout1, 
                             point_padding_x=unit(0, "lines"), point_padding_y=unit(0, "lines"), 
                             boxes=nlabel_boxes, 
                             xlim=c(NA, NA), ylim=c(NA, NA)
                             )

centers_of_RB_di <- cbind((centers_of_RB[,3] + centers_of_RB[,1])/2, (centers_of_RB[,4] + centers_of_RB[,2])/2)
pdf("..\\..\\..\\Desktop\\Graph_gg_returnDF_evening.pdf", width = 8.50, height = 11)
ggnet2(m, mode = as.matrix(centers_of_RB_di), 
       node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
       ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
       node.label = V(m)$label, label.size = produce_node_attrs$fontsize, label.color = "grey13", #label.trim = 15,
       edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
       edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize,
       legend.size = 0, legend.position = "up") 
dev.off()

sourceCpp("repel_boxes2.cpp")
centers_of_RB2 <- repel_boxes3(data_points=layout1, 
                               nlabel_boxes=nlabel_boxes, 
                               edge_list=edges, 
                               elabel_boxes=elabel_boxes,
                               xlim = c(NA, NA), ylim = c(NA, NA)
                               )
# xlim = c(0, 1), ylim = c(0, 1)
centers_of_RB2_di <- cbind((centers_of_RB2[,3] + centers_of_RB2[,1])/2, (centers_of_RB2[,4] + centers_of_RB2[,2])/2)
pdf("..\\..\\..\\Desktop\\9_Graph_ggmod2_128kI_3F_1S_grobOn3-noconv.pdf", width = 8.50, height = 11)
ggnet2(m, mode = as.matrix(centers_of_RB2_di), 
       node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
       ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
       node.label = V(m)$label, label.size = (produce_node_attrs$fontsize), label.color = "grey13", #label.trim = 15,
       edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
       edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize,
       legend.size = 0, legend.position = "up") 
dev.off()
