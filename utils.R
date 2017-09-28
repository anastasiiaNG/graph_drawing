get_nlabel_boxes <- function(layout1, nlabel_semisizes){
  return(data.frame("x1"=layout1[, 1] - nlabel_semisizes$semi_w,
                    "y1"=layout1[, 2] - nlabel_semisizes$semi_h,
                    "x2"=layout1[, 1] + nlabel_semisizes$semi_w,
                    "y2"=layout1[, 2] + nlabel_semisizes$semi_h
  )
  )
}

get_elabel_boxes <- function(layout1, edges, elabel_semisizes){
  return(data.frame("x1"=(layout1[edges[,1], 1] + layout1[edges[,2], 1]) / 2 - 
                      elabel_semisizes$semi_w,
                    "y1"=(layout1[edges[,1], 2] + layout1[edges[,2], 2]) / 2 - 
                      elabel_semisizes$semi_h,
                    "x2"=(layout1[edges[,1], 1] + layout1[edges[,2], 1]) / 2 + 
                      elabel_semisizes$semi_w,
                    "y2"=(layout1[edges[,1], 2] + layout1[edges[,2], 2]) / 2 + 
                      elabel_semisizes$semi_h
  )
  )
}

intersect <- function(box1, box2){
  return (box1$x1 < box2$x2 & box1$y1 < box2$y2 & box1$x2 > box2$x1 & box1$y2 > box2$y1)
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


# which(nt$label=="Succinate") #13


make_giff2 <- function(layouts){
  for (i in seq_along(layouts)) {
    layout1 <- layouts[[i]]
    png(filename=paste0("../../../Desktop/r/2809g2/", i, ".png"), res = 144, width = 8.5, height = 11, units = "in")
    plot(ggnet2(m, mode = as.matrix(layout1), layout.exp = 0.2,
                node.size = produce_node_attrs$width, max_size=5, node.color = produce_node_attrs$color,
                ## alpha = ifelse(produce_node_attrs$width == 0.5, 0, 1),
                node.label = V(m)$label, label.size = produce_node_attrs$fontsize, label.color = "grey13", #label.trim = 15,
                edge.size = produce_edge_attrs$penwidth/5, edge.color = produce_edge_attrs$color, edge.label.fill = NA,
                edge.label = E(m)$label, edge.label.size = produce_edge_attrs$fontsize,
                legend.size = 0, legend.position = "up"))
    dev.off()

  }
}

system.time({
layout2 <- force_alg(layout1,
                     nlabel_semisizes,
                     elabel_semisizes,
                     edges,
                     n_iter = 100, force = 5*1e-4)
})
#  for n_iter = 5   old7.8   new11.78 new2=0.65
#  for n_iter = 10  old15.15 new23.42
#  for n_iter = 15  old22.71 new34.77 new2=1.14
#  for n_iter = 150 new2=4.85, 16.50, 9.98 [24s for 800] depends on padding and force

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
