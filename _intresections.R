
#m
#layout1
nt <- as_data_frame(m, "vertices")
# по графику plot(m2, layout=layout1) видно, что порядок в списке координат layout1
# совпадает с порядком названий вершин в nt$name

nt$x <- layout1[,1]
nt$y <- layout1[,2]
#str(nt)

et <- as_data_frame(m, "edges")
#str(et) 

# making et$x and et$y fields
i = 1
for(node_start in et$from){
  et$x[i] <- nt$x[nt$name==node_start]
  et$y[i] <- nt$y[nt$name==node_start]
  i <- i + 1
}
#str(et)  

# nt[which(nt$name=="C00022_0"),] # вся строка про первую вершину

# making et$xend and et$yend fields
i = 1
for(node_end in et$to){
  et$xend[i] <- nt$x[nt$name==node_end]
  et$yend[i] <- nt$y[nt$name==node_end]
  i <- i + 1
}
#str(et)  
# nt$x
# et$x


# ifmo vizualization
# Line segment intersection r # github!!?? intersection language:R
# Sweep line algorithm
### both give first from Python >>> ideasman42/isect_segments-bentley_ottmann


# http://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/

lines_to_check <- data.frame(
  x = et$x,
  y = et$y,
  xend = et$xend,
  yend = et$yend,
  IDstart = as.character(et$from),
  IDend = as.character(et$to))


# str(lines_to_check)

# first <- lines_to_check[1, ] # p1=x, q1=y, p2=xend
# others <- lines_to_check[-1, ]
# second <- others[1, ]


onSegment <- function(x, y, xend, yend, rx, ry){
  if (xend <= max(x, rx) && xend >= min(x, rx) & yend <= max(y, ry) & yend >= min(y, ry)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

orientation <- function(x, y, xend, yend, rx, ry){
  val <- (yend - y) * (rx - xend) - (xend - x) * (ry - yend)
  if (val == 0){ return (0)}
  if (val > 0){ return (1)}
  else{return (2)}
}


intersection <- function(first, second){
  x <- first$x
  y <- first$y
  xend <- first$xend
  yend <- first$yend
  x2 <- second$x
  y2 <- second$y
  xend2 <- second$xend
  yend2 <- second$yend
  o1 <-  orientation(x, y, xend, yend, x2, y2)
  o2 <-  orientation(x, y, xend, yend, xend2, yend2)
  o3 <-  orientation(x2, y2, xend2, yend2, x, y)
  o4 <-  orientation(x2, y2, xend2, yend2, xend, yend)
  if(o1 != o2 && o3 != o4){return(TRUE)}
  if(o1 == 0 && onSegment(x, y, x2, y2, xend, yend)){return(TRUE)}
  if(o2 == 0 && onSegment(x, y, xend2, yend2, xend, yend)){return(TRUE)}
  if(o3 == 0 && onSegment(x2, y2, x, y, xend2, yend2)){return(TRUE)}
  if(o4 == 0 && onSegment(x2, y2, xend, yend, xend2, yend2)){return(TRUE)}else{
  return(FALSE)}
}

# intersection(lines_to_check[1, ], lines_to_check[7, ])
#lines_to_check[1,]
#lines_to_check[2,]
#head(lines_to_check)

n = 0  
for(row in 1:nrow(lines_to_check)){
  for(pair in 1:nrow(lines_to_check)){
    if(pair <= row) next
    
    if((as.character(lines_to_check$IDstart[row]) == as.character(lines_to_check$IDstart[pair])) |
       (as.character(lines_to_check$IDend[row]) == as.character(lines_to_check$IDend[pair])) |
       (as.character(lines_to_check$IDstart[row]) == as.character(lines_to_check$IDend[pair])) |
       (as.character(lines_to_check$IDend[row]) == as.character(lines_to_check$IDstart[pair]))) next
      # because r error: level sets of factors are different: 82 and 106 => factor -> character
    
    if (intersection(lines_to_check[row, ], lines_to_check[pair, ])){
      n = n + 1
    }
  }
}
print(n)

# class(lines_to_check[1, ]$x)
# class(et$x)

