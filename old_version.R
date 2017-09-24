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




force_alg <- function(layout1,
                      nlabel_semisizes,
                      elabel_semisizes,
                      edges,
                      xlim = new_borders_x_bot,
                      ylim = new_borders_y_bot,
                      n_iter = 8000, force = 1e-6){
  
  longer_goes_Y <- mean(nlabel_semisizes[ ,1])
  iter <- 1
  
  layouts <- list()
  
  while(iter <= n_iter){
    
    # print(c("Goes to ", iter, " iteration"))
    # intersection <- FALSE
    
    nlabel_boxes <- get_nlabel_boxes(layout1, nlabel_semisizes)
    elabel_boxes <- get_elabel_boxes(layout1, edges, elabel_semisizes)
    
    force_layout <- matrix(0, nrow = nrow(nlabel_boxes), ncol = 2)
    force_node <- matrix(0, nrow = nrow(nlabel_boxes), ncol = 4)
    
    # FORCE ACCUMULATION
    # NODES
    for (n1 in 1:(nrow(nlabel_boxes)-1)){
      # ...and nodes
      for (n2 in (n1+1):nrow(nlabel_boxes)){
        if (intersect(nlabel_boxes[n1, ], nlabel_boxes[n2, ])){
          # print(c(iter, "n-n: ", nt$label[n1], nt$label[n2]))
          intersection <- TRUE
          c1 <- centroid(nlabel_boxes[n1, ])
          c2 <- centroid(nlabel_boxes[n2, ])
          f <- force * unit_vector(c1, c2) / max(quad_dist(c1, c2), 0.01)
          force_layout[n1, ] <- force_layout[n1, ] + f / 2
          force_layout[n2, ] <- force_layout[n2, ] - f / 2
          
          if(nlabel_semisizes[n1, 1] > longer_goes_Y){
            force_layout[n1, 2] <- 2 * (force_layout[n1, 2] + 0.0001)
          }
          if(nlabel_semisizes[n2, 1] > longer_goes_Y){
            force_layout[n2, 2] <- 2 * (force_layout[n2, 2] + 0.0001)
          }
        }
      }
      
      # ...and edges (do not move it once again)
      for (e1 in 1:nrow(elabel_boxes)){
        if (intersect(nlabel_boxes[n1, ], elabel_boxes[e1, ])){
          # print(c(iter, "n-e: ", nt$label[n1], et$label[e1]))
          intersection <- TRUE
          c1 <- centroid(nlabel_boxes[n1, ])
          c2 <- centroid(elabel_boxes[e1, ])
          f <- 2 * force * unit_vector(c1, c2) / max(quad_dist(c1, c2), 0.01)
          force_layout[n1, ] <- force_layout[n1, ] + f / 2
          
          if(nlabel_semisizes[n1, 1] > longer_goes_Y){
            force_layout[n1, 2] <- 2 * (force_layout[n1, 2] + 0.0001)
          }
        }
      }
    }
    
    # EDGES
    # ...and edges
    for (e1 in 1:(nrow(elabel_boxes)-1)){
      for (e2 in (e1+1):nrow(elabel_boxes)){
        if (intersect(elabel_boxes[e1, ], elabel_boxes[e2, ])){
          # print(c(iter, "e-e: ", et$label[e1], et$label[e2]))
          itersection <- TRUE
          c1 <- centroid(elabel_boxes[e1, ])
          c2 <- centroid(elabel_boxes[e2, ])
          f <- force * unit_vector(c1, c2) / max(quad_dist(c1, c2), 0.01)
          
          force_layout[edges[e1, 1], ] <- force_layout[edges[e1, 1], ] + f / 2
          force_layout[edges[e1, 2], ] <- force_layout[edges[e1, 2], ] + f / 2
          
          force_layout[edges[e2, 1], ] <- force_layout[edges[e2, 1], ] - f / 2
          force_layout[edges[e2, 2], ] <- force_layout[edges[e2, 2], ] - f / 2
        }
      }
    }
    
    # Damping / friction / annealing
    force_layout <- force_layout * (1 - (1e-3) * iter)
    
    # Prohibit strong displacements
    force_layout[force_layout > 0.1] <- 0.1
    force_layout[force_layout < -0.1] <- -0.1
    
    
    # FORCE REALIZATION
    if (iter!=n_iter){
      layout1 <- layout1 + force_layout
      
      # Prohibit crawling away borders
      layout1[,1][layout1[,1] < new_borders_x_bot] <- new_borders_x_bot
      layout1[,1][layout1[,1] > 1 - new_borders_x_bot] <- 1 - new_borders_x_bot
      layout1[,2][layout1[,2] < new_borders_y_bot] <- new_borders_y_bot
      layout1[,2][layout1[,2] > 1 - new_borders_y_bot] <- 1 - new_borders_y_bot
      
      layouts[[iter]] <- layout1
    }
    
    iter <- iter + 1
  }
  
  # force_node[which(semiwidth(nlabel_boxes) > 0.8 * max(semiwidth(nlabel_boxes))), ][,2] <- 1.0001 * force_node[which(semiwidth(nlabel_boxes) > 0.8 * max(semiwidth(nlabel_boxes))), ][,2]
  # add random to Y:
  # force_layout[n1, 2] <- force_layout[n1, 2] + 1.5 * runif(1, min = -nlabel_semisizes$..., max = nlabel_semisizes$...)
  # force_layout[n2, 2] <- force_layout[n2, 2] - 1.5 * runif(1, min = -nlabel_semisizes$..., max = nlabel_semisizes$...)
  
  # if(!intersection){
  #   # ... spring
  # }
  
  # data from (n-1) iteration:
  return(
    list(layout=layout1,
         force_ends=layout1 + force_layout,
         nlabel_boxes=nlabel_boxes,
         elabel_boxes=elabel_boxes,
         layouts=layouts))
}