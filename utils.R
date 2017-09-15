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


get_nlabel_boxes <- function(layout1, nlabel_semisizes){
  return(data.frame("x1"=layout1[, 1] - nlabel_semisizes$semi_w,
                    "y1"=layout1[, 2] - nlabel_semisizes$semi_h,
                    "x2"=layout1[, 1] + nlabel_semisizes$semi_w,
                    "y2"=layout1[, 2] + nlabel_semisizes$semi_h
  )
  )
}

get_elabel_boxes <- function(layout1, edges, elabel_semisizes){
  middles <- matrix(0, nrow = nrow(edges), ncol = 2)
  for(i in seq(nrow(edges))){
    middles[i, 1] <- (layout1[edges[i, 1], 1] + layout1[edges[i, 2], 1]) / 2
    middles[i, 2] <- (layout1[edges[i, 1], 2] + layout1[edges[i, 2], 2]) / 2
  }
  return(data.frame("x1"=middles[, 1] - elabel_semisizes$semi_w,
                    "y1"=middles[, 2] - elabel_semisizes$semi_h,
                    "x2"=middles[, 1] + elabel_semisizes$semi_w,
                    "y2"=middles[, 2] + elabel_semisizes$semi_h
  )
  )
}

intersect <- function(box1, box2){
  return (box1$x1 < box2$x2 && box1$y1 < box2$y2 && box1$x2 > box2$x1 && box1$y2 > box2$y1)
}

quad_dist <- function(c1, c2){
  return ((c2$x - c1$x)^2 + (c2$y - c1$y)^2)
}

semiwidth <- function(box){
  return ((box$x2 - box$x1) / 2)
}

semiheight <- function(box){
  return((box$y2 - box$y1) / 2)
}

center <- function(c1, c2){
  return (data.frame("x" = (c1[1] + c2[1]) / 2, "y" = (c1[2] + c2[2]) / 2))
}

centroid <- function(box){
  return (data.frame("x" = box$x1 + semiwidth(box), "y" = box$y1 + semiheight(box)))
}

unit_vector <- function(c1, c2){
  return (c((c1$x - c2$x) / sqrt(quad_dist(c1, c2)), (c1$y - c2$y) / sqrt(quad_dist(c1, c2))))
}


#' in my Coulomb's law (F = k * (q1 * q2) / r^2)
#' F is represented as vector
#' @param k = force (user's choice)
#' @param q1 = q2 = 1
#' max(n_iter) = 999 # friction because

# debug:
n1 <- 1
n2 <- 16
e1 <- 1
e2 <- 2
c1 <- centroid(nlabel_boxes[1, ])
c2 <- centroid(nlabel_boxes[2, ])
unit_vector(c1, c2)
force = 1e-6
layout1 <- layout_with_kk(m)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
layout1 <- range01(layout1)
# (if unit_vector[n1,1] > 1.5 * unit_vector[n1,2] u may use *semiwidth)
# :debug

# nlabel_boxes <- as.data.frame(nlabel_boxes)
# elabel_boxes <- as.data.frame(elabel_boxes)

# which(nt$label=="alpha−D−Glucose 6−phosphate") #53

layout2 <- force_alg(layout1,
                     nlabel_semisizes,
                     elabel_semisizes,
                     edges,
                     n_iter = 500, force = 1e-5)

make_giff2 <- function(layouts){
  for (i in seq_along(layouts)) {
    layout1 <- layouts[[i]]
    png(filename=paste0("../../../Desktop/r/1010_10-5_1000/", i, ".png"), res = 144, width = 8.5, height = 11, units = "in")
    plot(ggnet2(m, mode = as.matrix(layout1),
                node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
                ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
                node.label = V(m)$label, label.size = produce_node_attrs$fontsize, label.color = "grey13", #label.trim = 15,
                edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
                edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize,
                legend.size = 0, legend.position = "up"))
    dev.off()

  }
}
make_giff2(layout2$layouts)

# $ ffmpeg -f image2 -i 08_09_f5e-5_%d.png video.gif
# octopus@octopus-TM-P643-M:~/r/0909_10-5_1000$ n=0
# octopus@octopus-TM-P643-M:~/r/0909_10-5_1000$ for file in ./*png; do test $n -eq 0 && cp "$file" selected/; n=$((n+1)); n=$((n%10)); done
# octopus@octopus-TM-P643-M:~/r/0909_10-5_1000/selected$ convert -delay 10 $(ls -v *.png) 0909_10-5_1000_each10.gif
# os.system('convert ...')

force_ends <- 10 * (layout2$force_ends - layout2$layouts[[999]])

pdf("..\\..\\..\\Desktop\\forces_on_1000.pdf", width = 8.50, height = 11)
ggnet2(m, mode = as.matrix(layout2$layouts[[999]]),
       node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
       ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
       node.label = V(m)$label, label.size = produce_node_attrs$fontsize, label.color = "grey13", #label.trim = 15,
       edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
       edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize,
       legend.size = 0, legend.position = "up") +
  geom_segment(data=as.data.frame(layout1),
               aes(x=layout2$layouts[[999]][,1], y=layout2$layouts[[999]][,2], xend=force_ends[,1], yend=force_ends[,2]),
               colour="red", size=0.5)
dev.off()