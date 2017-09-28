
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

  while(iter <= n_iter & intersection){
    
    # Let's try to add some random, firstly, to the whole layout1
    # (to get out of the local minima) # so called "jitter"
    layout1[,1] <- layout1[,1] + 0.1 * runif(1, min = -mean(nlabel_semisizes$semi_w), max = mean(nlabel_semisizes$semi_w))
    layout1[,2] <- layout1[,2] + 0.05 * runif(1, min = -mean(nlabel_semisizes$semi_w), max = mean(nlabel_semisizes$semi_w))

    # print(c("Goes to ", iter, " iteration"))
    intersection <- FALSE

    nlabel_boxes <- get_nlabel_boxes(layout1, nlabel_semisizes)
    elabel_boxes <- get_elabel_boxes(layout1, edges, elabel_semisizes)

    force_layout <- matrix(0, nrow = nrow(nlabel_boxes), ncol = 2)

    # FORCE ACCUMULATION
    # NODES
    # ...and nodes
    inter_matrix <- outer(seq_len(nrow(nlabel_boxes)), seq_len(nrow(nlabel_boxes)),
                          function(i,j) { intersect(nlabel_boxes[i,], nlabel_boxes[j,]) })

    diag(inter_matrix) <- FALSE
    inters <- which(inter_matrix, arr.ind = T)

    if(length(inters)!=0){
      intersection <- TRUE

      c1 <- centroid(nlabel_boxes[inters[,1],])
      c2 <- centroid(nlabel_boxes[inters[,2],])

      # inters DOUBLES INTERSECT_PAIRS => may use only first COLUMN and only "+":
      invisible( lapply(seq(nrow(inters)), function(i) {
        force_layout[inters[i,1],] <<- force_layout[inters[i,1],] + 
          force * unit_vector(c1[i,], c2[i,]) / max(quad_dist(c1[i,], c2[i,]), 0.01)
        }) )

      force_layout[, 2][nlabel_semisizes[,1] > longer_goes_Y] <- 2 * force_layout[, 2][nlabel_semisizes[,1] > longer_goes_Y]
    }

    # ...and edges
    inter_matrix2 <- outer(seq_len(nrow(nlabel_boxes)), seq_len(nrow(elabel_boxes)),
                           function(i,j) { intersect(nlabel_boxes[i,], elabel_boxes[j,]) })
    inters2 <- which(inter_matrix2, arr.ind = T)

    if(length(inters2)!=0){
      intersection <- TRUE

      c1 <- centroid(nlabel_boxes[inters2[,1],])
      c2 <- centroid(elabel_boxes[inters2[,2],])
      
      # calculate for intersecting node 
      invisible( lapply(seq(nrow(inters2)), function(i) {
        force_layout[inters2[i,1],] <<- force_layout[inters2[i,1],] + 
          2 * force * unit_vector(c1[i,], c2[i,]) / max(quad_dist(c1[i,], c2[i,]), 0.01)
      }) )
      
      force_layout[, 2][nlabel_semisizes[,1] > longer_goes_Y] <- 2 * force_layout[, 2][nlabel_semisizes[,1] > longer_goes_Y]
      
      # # calculate for incedent nodes
      # invisible( lapply(seq(nrow(inters2)), function(i) {
      #   force_layout[edges[inters2[i,2], 1], ] <<- force_layout[edges[inters2[i,1], 1], ] +
      #     force * unit_vector(c2[i,], c1[i,]) / max(quad_dist(c1[i,], c2[i,]), 0.01) }) )
      # 
      # invisible( lapply(seq(nrow(inters2)), function(i) {
      #   force_layout[edges[inters2[i,2], 2], ] <<- force_layout[edges[inters2[i,1], 2], ] +
      #     force * unit_vector(c2[i,], c1[i,]) / max(quad_dist(c1[i,], c2[i,]), 0.01) }) )
    }

    # EDGES
    # ...and edges
    inter_matrix3 <- outer(seq_len(nrow(elabel_boxes)), seq_len(nrow(elabel_boxes)),
                           function(i,j) { intersect(elabel_boxes[i,], elabel_boxes[j,]) })
    diag(inter_matrix3) <- FALSE
    inters3 <- which(inter_matrix3, arr.ind = T)

    if(length(inters3)!=0){
      intersection <- TRUE

      c1 <- centroid(elabel_boxes[inters3[,1], ])
      c2 <- centroid(elabel_boxes[inters3[,2], ])

      invisible( lapply(seq(nrow(inters3)), function(i) {
        force_layout[edges[inters3[i,1], 1], ] <<- force_layout[edges[inters3[i,1], 1], ] + 
          force * unit_vector(c1[i,], c2[i,]) / max(quad_dist(c1[i,], c2[i,]), 0.01) } ))

      invisible( lapply(seq(nrow(inters3)), function(i) {
        force_layout[edges[inters3[i,1], 2], ] <<- force_layout[edges[inters3[i,1], 2], ] + 
          force * unit_vector(c1[i,], c2[i,]) / max(quad_dist(c1[i,], c2[i,]), 0.01) } ))
    }

    # Prohibit strong displacements
    force_layout[force_layout > 0.1] <- 0.1
    force_layout[force_layout < -0.1] <- -0.1


    # FORCE REALIZATION
    if (iter!=n_iter){
      
      # Friction simulation
      layout1 <- layout1 + force_layout * (1 - (1e-3) * iter)

      # Prohibit crawling away borders
      layout1[,1][layout1[,1] < new_borders_x_bot] <- new_borders_x_bot
      layout1[,1][layout1[,1] > 1 - new_borders_x_bot] <- 1 - new_borders_x_bot
      layout1[,2][layout1[,2] < new_borders_y_bot] <- new_borders_y_bot
      layout1[,2][layout1[,2] > 1 - new_borders_y_bot] <- 1 - new_borders_y_bot

      layouts[[iter]] <- layout1
    }

    iter <- iter + 1
  }

  # data from (n-1) iteration:
  return(
    list(layout=layout1,
         force_ends=layout1 + force_layout,
         nlabel_boxes=nlabel_boxes,
         elabel_boxes=elabel_boxes,
         layouts=layouts
         ))
}
