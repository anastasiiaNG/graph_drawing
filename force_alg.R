
force_alg <- function(layout1,
                      nlabel_semisizes,
                      elabel_semisizes,
                      edges,
                      xlim = new_borders_x_bot,
                      ylim = new_borders_y_bot,
                      n_iter = 8000, force = 1e-6){

  longer_goes_Y <- mean(nlabel_semisizes[ ,1])
  intersection <- TRUE
  iter <- 1

  layouts <- list()

  while(iter <= n_iter && intersection){

    # print(c("Goes to ", iter, " iteration"))
    intersection <- FALSE

    nlabel_boxes <- get_nlabel_boxes(layout1, nlabel_semisizes)
    elabel_boxes <- get_elabel_boxes(layout1, edges, elabel_semisizes)

    force_layout <- matrix(0, nrow = nrow(nlabel_boxes), ncol = 2)

    # FORCE ACCUMULATION
    # NODES
    # ...and nodes
    inter_matrix <- outer(seq_len(nrow(nlabel_boxes)), seq_len(nrow(nlabel_boxes)),
                          Vectorize(function(i,j) { intersect(nlabel_boxes[i,], nlabel_boxes[j,]) }))
    diag(inter_matrix) <- FALSE
    inters <- which(inter_matrix, arr.ind = T)

    if(length(inters)!=0){
      intersection <- TRUE

      c1 <- lapply(seq(nrow(inters)), function(i) centroid(nlabel_boxes[inters[i,1], ]))
      c1 <- do.call(rbind, c1)
      c2 <- lapply(seq(nrow(inters)), function(i) centroid(nlabel_boxes[inters[i,2], ]))
      c2 <- do.call(rbind, c2)
      f <- lapply(seq(nrow(c1)), function(i) {
                    force * unit_vector(c1[i,], c2[i,]) / max(quad_dist(c1[i,], c2[i,]), 0.01)
                    })
      f <- do.call(rbind, f) / 2

      # inters DOUBLE INTERSECT_PAIRS => may use only first COLUMN and only "+":
      invisible( lapply(seq(nrow(inters)), function(i) {
        force_layout[inters[i,1], ] <<- force_layout[inters[i,1], ] + f[i, ] }) )
      # +0.0001...
      force_layout[, 2][nlabel_semisizes[,1] > longer_goes_Y] <- 2 * force_layout[, 2][nlabel_semisizes[,1] > longer_goes_Y]
    }
    
    # ...and edges 
    inter_matrix2 <- outer(seq_len(nrow(nlabel_boxes)), seq_len(nrow(elabel_boxes)),
                           Vectorize(function(i,j) { intersect(nlabel_boxes[i,], elabel_boxes[j,]) }))
    inters2 <- which(inter_matrix2, arr.ind = T)
    
    if(length(inters2)!=0){
      intersection <- TRUE
      
      c1 <- lapply(seq(nrow(inters2)), function(i) centroid(nlabel_boxes[inters2[i,1], ]))
      c1 <- do.call(rbind, c1)
      c2 <- lapply(seq(nrow(inters2)), function(i) centroid(elabel_boxes[inters2[i,2], ]))
      c2 <- do.call(rbind, c2)
      f <- lapply(seq(nrow(c1)), function(i) {
                  force * unit_vector(c1[i,], c2[i,]) / max(quad_dist(c1[i,], c2[i,]), 0.01)
                  })
      f <- do.call(rbind, f) / 2

      invisible( lapply(seq(nrow(inters2)), function(i) {
        force_layout[inters2[i,1], ] <<- force_layout[inters2[i,1], ] + f[i, ] }) )

      force_layout[, 2][nlabel_semisizes[,1] > longer_goes_Y] <- 2 * force_layout[, 2][nlabel_semisizes[,1] > longer_goes_Y]
    }
    
    # EDGES
    # ...and edges
    inter_matrix3 <- outer(seq_len(nrow(elabel_boxes)), seq_len(nrow(elabel_boxes)),
                           Vectorize(function(i,j) { intersect(elabel_boxes[i,], elabel_boxes[j,]) }))
    diag(inter_matrix3) <- FALSE
    inters3 <- which(inter_matrix3, arr.ind = T)

    if(length(inters3)!=0){
      intersection <- TRUE

      c1 <- lapply(seq(nrow(inters3)), function(i) centroid(elabel_boxes[inters3[i,1], ]))
      c1 <- do.call(rbind, c1)
      c2 <- lapply(seq(nrow(inters3)), function(i) centroid(elabel_boxes[inters3[i,2], ]))
      c2 <- do.call(rbind, c2)
      f <- lapply(seq(nrow(c1)), function(i) {
                    force * unit_vector(c1[i,], c2[i,]) / max(quad_dist(c1[i,], c2[i,]), 0.01)
                    })
      f <- do.call(rbind, f) / 2

      invisible( lapply(seq(nrow(inters3)), function(i) {
        force_layout[edges[inters3[i,1], 1], ] <<- force_layout[edges[inters3[i,1], 1], ] + f[i, ] }) )

      invisible( lapply(seq(nrow(inters3)), function(i) {
        force_layout[edges[inters3[i,1], 2], ] <<- force_layout[edges[inters3[i,1], 2], ] + f[i, ] }) )
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
