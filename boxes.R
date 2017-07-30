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

lines_to_check <- data.frame(
  x = et$x,
  y = et$y,
  xend = et$xend,
  yend = et$yend,
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


# str(lines_to_check)
# head(lines_to_check)
# str(et)
# str(produce_edge_attrs) # fontsize, label
# str(produce_node_attrs)


## >>>>> 5. make_content_for_REPEL_BOXES.cpp----------------

# The padding around each bounding box.
box.padding = 0.25
# IT DOESN'T WORK: 
# >Error in convertUnit(x, unitTo, "x", "dimension", "x", "dimension", valueOnly = valueOnly) : 
# > аргумент 'x' должен быть объектом-единицей
#box_padding_x <- convertWidth(box.padding, "npc", valueOnly = TRUE)
#box_padding_y <- convertHeight(box.padding, "npc", valueOnly = TRUE)

# The padding around each point. <<<<__this one was deleted
point.padding = 1e-6
#point_padding_x <- convertWidth(point.padding, "native", valueOnly = TRUE)
#point_padding_y <- convertHeight(point.padding, "native", valueOnly = TRUE)

label.padding = 0.25

#i=1
#boxes = c()

elabel_boxes <- lapply(seq(produce_edge_attrs$label), function(i) {
  
  t <- textGrob(produce_node_attrs$label[i], 
              x=lines_to_check$middle_x[i] + label.padding,
              y=lines_to_check$middle_y[i] + label.padding, 
              # check.overlap = T,
              gp=gpar(fontsize=produce_edge_attrs$fontsize[i] * .pt, 
                      col=produce_edge_attrs$color[i],
                      lineheight = produce_edge_attrs$penwidth[i]),  
              name = "text")
  
  r <- roundrectGrob(x=lines_to_check$middle_x[i], 
                     y=lines_to_check$middle_y[i], 
                     width = grobWidth(t), # + 2 * label.padding, # оба операнда должны быть единицами
                     height = grobHeight(t), # + 2 * label.padding,
                     # r = produce_node_attrs$fontsize,
                     gp = gpar(col = produce_node_attrs$color[i]
                             # fill = scales::alpha(row$fill, row$alpha),
                             # lwd = produce_edge_attrs$fontsize * .pt))
                             ),
                     name = "box"
                     )
  
  gw <- convertWidth(grobWidth(t), "native", TRUE) / 2
  gh <- convertHeight(grobHeight(t), "native", TRUE) / 2
  
  c(
    "x1" = lines_to_check$middle_x[i] - gw - box.padding, #+ x$nudges$x[i],
    "y1" = lines_to_check$middle_y[i] - gh - box.padding, #+ x$nudges$y[i],
    "x2" = lines_to_check$middle_x[i] + gw + box.padding, #+ x$nudges$x[i],
    "y2" = lines_to_check$middle_y[i] + gh + box.padding #+ x$nudges$y[i]
  )
})


# head(nt)
# head(lines_to_check)
# head(produce_node_attrs)

nlabel_boxes <- lapply(seq(produce_node_attrs$label), function(i) {
  
  t <- textGrob(produce_node_attrs$label[i], 
                x=nt$x[i] + label.padding,
                y=nt$y[i] + label.padding, 
                # check.overlap = T,
                gp=gpar(fontsize=produce_node_attrs$fontsize[i] * .pt, 
                        col=produce_node_attrs$color[i],
                        lineheight = produce_node_attrs$penwidth[i]),  
                name = "text")
  
  r <- roundrectGrob(x=lines_to_check$middle_x[i], 
                     y=lines_to_check$middle_y[i], 
                     width = grobWidth(t), # + 2 * label.padding, # оба операнда должны быть единицами
                     height = grobHeight(t), # + 2 * label.padding,
                     # r = produce_node_attrs$fontsize,
                     gp = gpar(col = produce_node_attrs$color[i]
                               # fill = scales::alpha(row$fill, row$alpha),
                               # lwd = produce_edge_attrs$fontsize * .pt))
                     ),
                     name = "box"
  )
  
  gw <- convertWidth(grobWidth(t), "native", TRUE) / 2
  gh <- convertHeight(grobHeight(t), "native", TRUE) / 2
  
  c(
    "x1" = nt$x[i] - gw - box.padding, #+ x$nudges$x[i],
    "y1" = nt$y[i] - gh - box.padding, #+ x$nudges$y[i],
    "x2" = nt$x[i] + gw - box.padding, #+ x$nudges$x[i],
    "y2" = nt$y[i] + gh + box.padding #+ x$nudges$y[i]
  )
})

nlabel_boxes <- do.call(rbind, nlabel_boxes)

elabel_boxes <- do.call(rbind, elabel_boxes)
# elabel_boxes <- cbind(elabel_boxes, lines_to_check$IDs_num, lines_to_check$IDe_num)
# colnames(elabel_boxes)[5:6] <- c("Numstart", "Numend")

edges <- cbind(lines_to_check$IDs_num, lines_to_check$IDe_num)


#boxes
str(nlabel_boxes)
head(nlabel_boxes)
str(elabel_boxes)
head(elabel_boxes)
str(edges)
head(edges)



## >>>>> 6. call Rcpp----------------

# sourceCpp("test.cpp")
# double_me(2)

# model data:
##xlim <- c(0, 1)
##ylim <- c(0, 1)
##set.seed(22)
##boxes <- cbind(rnorm(57), rnorm(57), rnorm(57), rnorm(57))

sourceCpp("repel_boxes.cpp")
centers_of_RB <- repel_boxes(layout1, 
                             point_padding_x=1e-6, point_padding_y=1e-6, 
                             nlabel_boxes, 
                             xlim = c(0, 1), ylim = c(0, 1))


sourceCpp("repel_boxes2.cpp")
centers_of_RB2 <- repel_boxes2(layout1, 
                              nlabel_boxes, 
                              edges, 
                              elabel_boxes,
                              xlim = c(0, 1), ylim = c(0, 1))








str(lines_to_check)
head(nt)
head(lines_to_check)
head(produce_node_attrs)

######_AFTER_boxes_BEING_REPELED_FROM_each_other_######################
#----make_content_for_labelRepelGrob-------
set.seed(22)
centers_of_RB <- cbind(rnorm(57), rnorm(57))
x <- centers_of_RB[,1]
y <- centers_of_RB[,2]
x.orig <- lines_to_check$middle_x
y.orig <- lines_to_check$middle_y



hj <- resolveHJust(x, NULL) # выдаёт одно число 0_о
vj <- resolveVJust(y, NULL)
hj
i = 1

t_lrg <- textGrob(
  produce_edge_attrs$label[i],
  x + 2 * (0.5 - hj) * box.padding,
  x + 2 * (0.5 - vj) * box.padding,
  just = c(hj, vj),
  # gp = text.gp,
  name = "text"
)

r_lrg <- roundrectGrob(
  x[i] + 2 * (0.5 - hj) * box.padding,
  y[i] + 2 * (0.5 - vj) * box.padding,
  default.units = "native",
  width = grobWidth(t_lrg), # + 2 * label.padding, # оба операнда должны быть единицами
  height = grobHeight(t_lrg), # + 2 * label.padding,
  just = c(hj, vj),
  # r = r,
  # gp = rect.gp,
  name = "box"
)

convertWidth(- 0.5 * grobWidth(r_lrg), "native", TRUE) 
# x1 <- convertWidth(x[i] - 0.5 * grobWidth(r_lrg), "native", TRUE) # оба операнда должны быть единицами
x1 <- x[i] - convertWidth(0.5 * grobWidth(r_lrg), "native", TRUE)
x2 <- x[i] + convertWidth(0.5 * grobWidth(r_lrg), "native", TRUE)
y1 <- y[i] - convertHeight(0.5 * grobHeight(r_lrg), "native", TRUE)
y2 <- y[i] + convertHeight(0.5 * grobHeight(r_lrg), "native", TRUE)

# # оба операнда должны быть единицами ###############?????????????
point_pos <- c(
  convertWidth(x.orig[i], "native", TRUE),
  convertHeight(y.orig[i], "native", TRUE)
)

sourceCpp("repel_boxes.cpp")
center <- centroid(c(x1, y1, x2, y2))


# Get the coordinates of the intersection between the line from the
# original data point ***to the centroid*** and the rectangle's edges.
text_box <- c(x1, y1, x2, y2)
sourceCpp("repel_boxes.cpp")
int <- intersect_line_rectangle(point_pos, center, text_box)           # <<<<<<<<<<<<<<<<<<<<<<<<<

# Check if the data point is inside the label box.
point_inside <- FALSE
if (text_box[1] <= point_pos[1] && point_pos[1] <= text_box[3] &&
    text_box[2] <= point_pos[2] && point_pos[2] <= text_box[4]) {
  point_inside <- TRUE
}

# Nudge the original data point toward the label with point.padding._________no_change_of_point_padding______??????????
point_padding_x <- convertWidth(point.padding, "native", TRUE) / 2
point_padding_y <- convertHeight(point.padding, "native", TRUE) / 2
point_padding <- point_padding_x > 0 & point_padding_y > 0
if (point_padding) {
  point_box <- c(
    point_pos[1] - point_padding_x, point_pos[2] - point_padding_y,
    point_pos[1] + point_padding_x, point_pos[2] + point_padding_y
  )
  sourceCpp("repel_boxes.cpp")
  point_pos <- intersect_line_rectangle(center, point_pos, point_box)   # <<<<<<<<<<<___point_pos??___<<<<<<<<<<<<
}                                                                       # <<<<<<<<<<<___point_box??___<<<<<<<<<<<<



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Compute the distance between the data point and the edge of the text box.
dx <- abs(int[1] - point_pos[1])
dy <- abs(int[2] - point_pos[2])
d <- sqrt(dx * dx + dy * dy)
# Scale the unit vector by the minimum segment length.
if (d > 0) {
  mx <- convertWidth(x$min.segment.length, "native", TRUE)
  my <- convertHeight(x$min.segment.length, "native", TRUE)
  min.segment.length <- sqrt((mx * dx / d) ^ 2 + (my * dy / d) ^ 2)
}
if (!point_inside && d > 0 && euclid(int, point_pos) > min.segment.length) {
  s <- segmentsGrob(
    x0 = int[1],
    y0 = int[2],
    x1 = point_pos[1],
    y1 = point_pos[2],
    default.units = "native",
    gp = x$segment.gp,
    name = "segment",
    arrow = x$arrow
  )
  
  
  
  
  
  setChildren(x, gList(s, t))
  } else {
  setChildren(x, gList(t))
  }

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




for (i in seq(produce_edge_attrs$label)){
  labelRepelGrob(produce_edge_attrs$label[i],
               x[i], 
               y[i])
} 






labelRepelGrob <- function(
  label,
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  x.orig = unit(0.5, "npc"),
  y.orig = unit(0.5, "npc"),
  default.units = "npc",
  just = "center",
  box.padding = 0.25,
  label.padding = 0.25,
  point.padding = 1e-6,
  name = NULL,
  text.gp = gpar(),
  rect.gp = gpar(fill = "white"),
  r = unit(0.1, "snpc"),
  segment.gp = gpar(),
  vp = NULL,
  arrow = NULL,
  min.segment.length = 0.5
) {
  stopifnot(length(label) == 1)
  
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  
  gTree(
    label = label,
    x = x,
    y = y,
    x.orig = x.orig,
    y.orig = y.orig,
    just = just,
    box.padding = box.padding,
    label.padding = label.padding,
    point.padding = point.padding,
    r = r,
    name = name,
    text.gp = text.gp,
    rect.gp = rect.gp,
    segment.gp = segment.gp,
    vp = vp,
    cl = "labelrepelgrob",
    arrow = arrow,
    min.segment.length = min.segment.length
  )
}

#########################################################
x <- stats::runif(20)
y <- stats::runif(20)
grid.newpage()
grid.text("SOMETHING NICE AND BIG", x=x, y=y, check.overlap = T,
          gp=gpar(fontsize=20, col="grey"))

lines_to_check$middle_x <- (lines_to_check$middle_x-min(lines_to_check$middle_x))/(max(lines_to_check$middle_x)-min(lines_to_check$middle_x))
lines_to_check$middle_y <- (lines_to_check$middle_y-min(lines_to_check$middle_y))/(max(lines_to_check$middle_y)-min(lines_to_check$middle_y))
grid.newpage()
grid.text(produce_edge_attrs$label, x=lines_to_check$middle_x, y=lines_to_check$middle_y, 
          gp=gpar(fontsize=produce_edge_attrs$fontsize, col=produce_edge_attrs$color))


#########################################################
grid.newpage()
t0 <- grid.text(produce_node_attrs$label, 
                x=lines_to_check$middle_x, 
                y=lines_to_check$middle_y, 
                # check.overlap = T,
                gp=gpar(fontsize=produce_edge_attrs$fontsize, 
                        col=produce_edge_attrs$color))
class(t0) # [1] "text"  "grob"  "gDesc"
str(t0) # List of 11
#########################################################

grid.newpage()
i=1
for (i in seq(produce_edge_attrs$label)){
  # print(produce_edge_attrs$label[i])
  grid.text(produce_edge_attrs$label[i], 
            x=lines_to_check$middle_x[i], 
            y=lines_to_check$middle_y[i], 
            # check.overlap = T,
            gp=gpar(fontsize=produce_edge_attrs$fontsize[i], 
                    col=produce_edge_attrs$color[i]))
}
