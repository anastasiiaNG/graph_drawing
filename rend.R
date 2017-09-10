layout1 <- layout_with_kk(m)
plot(m, layout=layout1, vertex.size = 3, vertex.label=NA, edge.label=NA)
plot(m, layout=layout2$layouts[[999]], vertex.size = 3, vertex.label=NA, edge.label=NA)
# Error in plot.new() : figure margins too large
# par(mar=c(1,1,1,1))
plot(m, vertex.size = 3, vertex.label=NA, edge.label=NA)

lines_to_check$NameStart <- nt$label[lines_to_check$IDs_num]
lines_to_check$NameEnd <- nt$label[lines_to_check$IDe_num]

lines_to_check$IDs_num
lines_to_check$NameStart
lines_to_check$IDe_num
lines_to_check$NameEnd


plot(NA, xlim=c(0, 1), ylim=c(0, 1), xlab="x", ylab="y")
plot(NA, xlim=c(-5, 5), ylim=c(-5, 5), xlab="x", ylab="y")
plot(NA, xlim=c(-10, 10), ylim=c(-10, 10), xlab="x", ylab="y")
plot(NA, xlim=c(-100, 100), ylim=c(-100, 100), xlab="x", ylab="y")

rect(layout1[,5], layout1[,6], layout1[,7], layout1[,8])
rect(layout1[,9], layout1[,10], layout1[,11], layout1[,12])

rect(nlabel_boxes[,1][c(1,2)], nlabel_boxes[,2][c(1,2)], nlabel_boxes[,3][c(1,2)], nlabel_boxes[,4][c(1,2)])
rect(elabel_boxes[,1][c(1,2)], elabel_boxes[,2][c(1,2)], elabel_boxes[,3][c(1,2)], elabel_boxes[,4][c(1,2)])

rect(nlabel_boxes[,1], nlabel_boxes[,2], nlabel_boxes[,3], nlabel_boxes[,4])
rect(elabel_boxes[,1], elabel_boxes[,2], elabel_boxes[,3], elabel_boxes[,4])

rect(layout2$nlabel_boxes[,1], layout2$nlabel_boxes[,2], layout2$nlabel_boxes[,3], layout2$nlabel_boxes[,4])
rect(layout2$elabel_boxes[,1], layout2$elabel_boxes[,2], layout2$elabel_boxes[,3], layout2$elabel_boxes[,4])

rect(centers_of_RB[,1], centers_of_RB2[,2], centers_of_RB2[,3], centers_of_RB2[,4])
rect(centers_of_RB2[,1], centers_of_RB2[,2], centers_of_RB2[,3], centers_of_RB2[,4])


# curve
ggplot(data=nt, aes(x=nt$x, y=nt$y, color=nt$log2FC)) +
  geom_point() +
  geom_text(aes(label=nt$label, size = produce_node_attrs$fontsize/2)) +
  geom_segment(data=et, aes(x=et$x, y=et$y, xend=et$xend, yend=et$yend), colour="red", size=0.5) 
#geom_point(x=lines_to_check$middle_x, y=lines_to_check$middle_y, color="black", size=1)
# aes(geom_point(nlabel_boxes))
# geom_text(data=et, aes(label=et$label))



ggplot(data=as.data.frame(layout1), aes(x=layout1[,1], y=layout1[,2], color=nt$log2FC)) +
  geom_point() +
  geom_text(aes(label=nt$label, size = range01(produce_node_attrs$fontsize))) +
  geom_segment(data=layout1, aes(x=layout1[,5], y=layout1[,6], xend=layout1[,7], yend=layout1[,8]), colour="red", size=0.5) 


z <- as.data.frame(rbind(cbind(nt$x, nt$y), cbind((et$x + et$xend) / 2, (et$y + et$yend) / 2)))

ggplot(z, aes(z[,1],z[,2])) + geom_point() +
  geom_segment(data=as.data.frame(nlabel_boxes),
               aes(x=nlabel_boxes[,1], y=nlabel_boxes[,2], xend=nlabel_boxes[,3], yend=nlabel_boxes[,4]),
               colour="red", size=0.5) +
  # geom_segment(data=as.data.frame(nlabel_boxes),
  #              aes(x=nlabel_boxes[,3], y=nlabel_boxes[,4], xend=nlabel_boxes[,1], yend=nlabel_boxes[,2]),
  #              colour="red", size=0.5) +
  geom_segment(data=as.data.frame(elabel_boxes),
               aes(x=elabel_boxes[,1], y=elabel_boxes[,2], xend=elabel_boxes[,3], yend=elabel_boxes[,4]),
               colour="green", size=0.5) +
  geom_text(label=c(produce_node_attrs$label, produce_edge_attrs$label))
# geom_segment(data=as.data.frame(elabel_boxes),
#              aes(x=elabel_boxes[,3], y=elabel_boxes[,4], xend=elabel_boxes[,1], yend=elabel_boxes[,2]),
#              colour="red", size=0.5)


x <- as.matrix(centers_of_RB2_di)[,1]
y <- as.matrix(centers_of_RB2_di)[,2]

ggplot(data=nt, aes(x=x, y=y, color=nt$log2FC)) +
  geom_point() +
  geom_text(aes(label=nt$label, size = produce_node_attrs$fontsize/8)) #+
#geom_segment(data=et, aes(x=et$x, y=et$y, xend=et$xend, yend=et$yend), colour="red", size=0.5) 
#geom_point(x=lines_to_check$middle_x, y=lines_to_check$middle_y, color="black", size=1)
# aes(geom_point(nlabel_boxes))
# geom_text(data=et, aes(label=et$label))



## --------------------B_E_F_O_R_E------------------

render_ebefore2 <- function(rect, centers_labels_x, centers_labels_y, labels, fontsize) {
  
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
render_ebefore2(elabel_boxes, lines_to_check$middle_x, lines_to_check$middle_y, 
               produce_edge_attrs$label, produce_edge_attrs$fontsize)
dev.off()
render_ebefore2(nlabel_boxes, nt$x, nt$y, 
               produce_node_attrs$label, produce_node_attrs$fontsize)
dev.off()
render_ebefore2(elabel_boxes-0.1, lines_to_check$middle_x-0.1, lines_to_check$middle_y-0.1, 
               E(m)$label, fontsize = 5)
dev.off()
render_ebefore2(nlabel_boxes-0.1, lines_to_check$x-0.1, lines_to_check$y-0.1, 
               V(m)$label, fontsize = 5)



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



## --------------------A_F_T_E_R------------------

render_after2 <- function(rect, labels, fontsize) {
  
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
render_after2(nlabel_boxes, produce_node_attrs$label, produce_node_attrs$fontsize)
dev.off()
render_after2(elabel_boxes, produce_edge_attrs$label, produce_edge_attrs$fontsize)

dev.off()
render_after2(centers_of_RB2, produce_node_attrs$label, produce_node_attrs$fontsize)
dev.off()
render_after2(centers_of_RB2, V(m)$label, fontsize)

dev.off()
render_after2(layout1[,5:8], V(m)$label, produce_node_attrs$fontsize)
dev.off()
render_after2(layout1[,9:12], E(m)$label, produce_edge_attrs$fontsize)

dev.off()
render_after2(elabel_boxes, E(m)$label, fontsize=5)


dev.off()
render_after2(layout2$nlabel_boxes[,1:4], V(m)$label, produce_node_attrs$fontsize)
render_after2(layout2$elabel_boxes[,1:4], E(m)$label, produce_edge_attrs$fontsize)



render_after1 <- function(layout1, labels, fontsize) {

  grid.text(produce_node_attrs$label,
            x=layout1[,1],
            y=layout1[,2],
            default.units = "npc",
            
            gp=gpar(fontsize=produce_node_attrs$fontsize/6 * .pt
            ),
            name = "text" #,
            # vp = viewport(xscale = c(0, 1), yscale = c(0, 1))
  )
}

dev.off()
render_after1(layout1[ ,c(1,2)], produce_edge_attrs$label, produce_edge_attrs$fontsize)

dev.off()
render_after1(centers_of_RB2, produce_node_attrs$label, produce_node_attrs$fontsize)

