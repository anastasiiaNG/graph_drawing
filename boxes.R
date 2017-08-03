## >>>>> 1. create layout
layout1 <- layout_with_kk(m)

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
  et$x[i] <- nt$x[nt$name==node_start]
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

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

lines_to_check <- data.frame(
  x = range01(et$x),
  y = range01(et$y),
  xend = range01(et$xend),
  yend = range01(et$yend),
  IDstart = as.character(et$from),
  IDend = as.character(et$to),
  IDs_num = et$s_num,
  IDe_num = et$e_num)

## >>>>> 3. фиктивные ноды посередине ребра_additional_nodes_positions
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

# The padding around each bounding box.
box.padding = 0.25 # to make spaces between label boxes wider
box.padding <- unit(0.25, "lines")
box_padding_x <- convertWidth(box.padding, "native", valueOnly = TRUE)
box_padding_y <- convertHeight(box.padding, "native", valueOnly = TRUE)

# i=1
# boxes = c()
# {check.overlap = T, lineheight} in textGrob

elabel_boxes <- lapply(seq(produce_edge_attrs$label), function(i) {
  
  t <- textGrob(produce_edge_attrs$label[i], 
              x=lines_to_check$middle_x[i],
              y=lines_to_check$middle_y[i], 
              default.units = "native",
              
              gp=gpar(fontsize=produce_edge_attrs$fontsize[i] * .pt 
                      ),
              name = "text"
              )
  
  gw <- convertWidth(grobWidth(t), "native", TRUE) / 2
  gh <- convertHeight(grobHeight(t), "native", TRUE) / 2
  
  c(
    "x1" = lines_to_check$middle_x[i] - gw - box_padding_x, #+ x$nudges$x[i],
    "y1" = lines_to_check$middle_y[i] - gh - box_padding_y, #+ x$nudges$y[i],
    "x2" = lines_to_check$middle_x[i] + gw + box_padding_x, #+ x$nudges$x[i],
    "y2" = lines_to_check$middle_y[i] + gh + box_padding_y  #+ x$nudges$y[i]
  )
})


# head(nt)
# head(lines_to_check)
# head(produce_node_attrs)

nlabel_boxes <- lapply(seq(produce_node_attrs$label), function(i) {
  
  t <- textGrob(produce_node_attrs$label[i], 
                x=nt$x[i], 
                y=nt$y[i], 
                default.units = "native",
                
                gp=gpar(fontsize=produce_node_attrs$fontsize[i] * .pt 
                        ),  
                name = "text"
                )
  
  gw <- convertWidth(grobWidth(t), "native", TRUE) / 2
  gh <- convertHeight(grobHeight(t), "native", TRUE) / 2
  
  c(
    "x1" = nt$x[i] - gw - box_padding_x, # + x$nudges$x[i],
    "y1" = nt$y[i] - gh - box_padding_y, # + x$nudges$y[i],
    "x2" = nt$x[i] + gw + box_padding_x, # + x$nudges$x[i],
    "y2" = nt$y[i] + gh + box_padding_y  # + x$nudges$y[i]
  )
})



# for(i in seq(nrow(nlabel_boxes))){
#   print(centroid(nlabel_boxes[i,]))
# }


nlabel_boxes <- do.call(rbind, nlabel_boxes)

elabel_boxes <- do.call(rbind, elabel_boxes)

edges <- cbind(lines_to_check$IDs_num, lines_to_check$IDe_num)

#nlabel_boxes <- range01(nlabel_boxes)
#elabel_boxes <- range01(elabel_boxes)
layout1 <- range01(layout1)


#boxes
str(nlabel_boxes)
head(nlabel_boxes)
str(elabel_boxes)
head(elabel_boxes)
str(edges)
head(edges)

str(lines_to_check)
head(nt)
head(lines_to_check)
head(produce_node_attrs)

## >>>>> 6. call Rcpp----------------

# sourceCpp("test.cpp")
# double_me(2)

# model data:
##xlim <- c(0, 1)
##ylim <- c(0, 1)
##set.seed(22)
##boxes <- cbind(rnorm(57), rnorm(57), rnorm(57), rnorm(57))

pdf("..\\..\\..\\Desktop\\Graph_pre.pdf", width = 8.50, height = 11)
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
                             point_padding_x=1e-6, point_padding_y=1e-6, 
                             boxes=nlabel_boxes, 
                             xlim=c(NA, NA), ylim=c(NA, NA)
                             )
pdf("..\\..\\..\\Desktop\\Graph_gg.pdf", width = 8.50, height = 11)
ggnet2(m, mode = as.matrix(centers_of_RB), 
       node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
       ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
       node.label = V(m)$label, label.size = produce_node_attrs$fontsize, label.color = "grey13", #label.trim = 15,
       edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
       edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize,
       legend.size = 0, legend.position = "up") 
dev.off()

# why it doesn't work
centers_of_RB <- ggrepel:::repel_boxes(data_points=layout1, 
                                        point_padding_x=1e-6, point_padding_y=1e-6, 
                                        boxes=nlabel_boxes, 
                                        xlim=c(NA, NA), ylim=c(NA, NA)
                                        )
ggrepel:::repel_boxes(data_points=layout1, 
                      # point_padding_x=1e-6, point_padding_y=1e-6, 
                      point_padding_x=1, point_padding_y=1,
                      boxes=nlabel_boxes, 
                      xlim=c(NA, NA), ylim=c(NA, NA)
                      )


centers_of_RB2 <- repel_boxes2(data_points=layout1, 
                               nlabel_boxes=nlabel_boxes, 
                               edge_list=edges, 
                               elabel_boxes=elabel_boxes,
                               xlim = c(NA, NA), ylim = c(NA, NA)
                              )
pdf("..\\..\\..\\Desktop\\Graph_ggmod.pdf", width = 8.50, height = 11)
ggnet2(m, mode = as.matrix(centers_of_RB2), 
       node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
       ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
       node.label = V(m)$label, label.size = (produce_node_attrs$fontsize), label.color = "grey13", #label.trim = 15,
       edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
       edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize,
       legend.size = 0, legend.position = "up") 
dev.off()






######_AFTER_boxes_BEING_REPELED_FROM_each_other_######################


# grobs (after >repels<)
x <- unit(centers_of_RB$x, "native")
y <- unit(centers_of_RB$y, "native")

# textRepelGrob
# final <- lapply(seq(produce_node_attrs$label), function(i) {
  
  t <- textGrob(produce_node_attrs$label, 
                x=x + box.padding, 
                y=y + box.padding, 
                default.units = "native",
                
                gp=gpar(fontsize=produce_node_attrs$fontsize * .pt 
                ),  
                name = "text"
  )
  
  x1 <- convertWidth(x - 0.5 * grobWidth(t), "native", TRUE)
  x2 <- convertWidth(x + 0.5 * grobWidth(t), "native", TRUE)
  y1 <- convertHeight(y - 0.5 * grobHeight(t), "native", TRUE)
  y2 <- convertHeight(y + 0.5 * grobHeight(t), "native", TRUE)
  
  center <- cbind(x1, y1, x2, y2)
  
final <- c()
  for(i in seq(nrow(center))){
     final <- rbind(final, centroid(center[i,]))
     }




#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## >>>>> 7. visualize new positions with nodes already under it
pdf("..\\..\\..\\Desktop\\Graph_pre.pdf", width = 8.50, height = 11)
ggnet2(m, mode = layout1, 
       node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
       ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
       node.label = V(m)$label, label.size = produce_node_attrs$fontsize, label.color = "grey13", #label.trim = 15,
       edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
       edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize,
       legend.size = 0, legend.position = "up") 
dev.off()


pdf("..\\..\\..\\Desktop\\Graph_ggmod.pdf", width = 8.50, height = 11)
ggnet2(m, mode = as.matrix(final), 
       node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
       ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
       node.label = V(m)$label, label.size = produce_node_attrs$fontsize, label.color = "grey13", #label.trim = 15,
       edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
       edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize,
       legend.size = 0, legend.position = "up") 
dev.off()



for (i in seq(produce_edge_attrs$label)){
  labelRepelGrob(produce_edge_attrs$label[i],
               x[i], 
               y[i])
} 

#########################################################
x <- stats::runif(20)
y <- stats::runif(20)
grid.newpage()
grid.text("SOMETHING NICE AND BIG", x=x, y=y, check.overlap = T,
          gp=gpar(fontsize=20, col="grey"))


i <- 1
grid.text(t)
grid.text(produce_node_attrs$label[i], 
         x=nt$x[i], y=nt$y[i], default.units = "native",
         # check.overlap = T,
         gp=gpar(fontsize=produce_node_attrs$fontsize[i] * .pt 
                 # col=produce_node_attrs$color[i],
                 # lineheight = produce_node_attrs$width[i] # in our origin this is for circle inder label
         ),  
         name = "text"
)



lines_to_check$middle_x <- (lines_to_check$middle_x-min(lines_to_check$middle_x))/(max(lines_to_check$middle_x)-min(lines_to_check$middle_x))
lines_to_check$middle_y <- (lines_to_check$middle_y-min(lines_to_check$middle_y))/(max(lines_to_check$middle_y)-min(lines_to_check$middle_y))

grid.newpage()
grid.text(produce_edge_attrs$label, x=lines_to_check$middle_x, y=lines_to_check$middle_y, 
          gp=gpar(fontsize=produce_edge_attrs$fontsize, col=produce_edge_attrs$color))