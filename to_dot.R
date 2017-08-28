saveModuleToDot <- function(module, file, name=NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(module))
  }
  s <- getGraphDotString(module, name)
  write(s, file)
}

getGraphDotString <- function(module, name) {
  res <- c()
  res <- c(res, sprintf('graph "%s" {\n', name))
  res <- c(res, "outputorder=edgesfirst\n")
  res <- c(res, "bgcolor=transparent\n")
  res <- c(res, "overlap=\"prism\"\n")
  res <- c(res, "overlap_scaling=-4\n")
  res <- c(res, "overlap_shrink=true\n")
  res <- c(res, "splines=true\n")
  res <- c(res, "esep=55\n")
  res <- c(res, "pad=\"2,0.25\"\n")
  res <- c(res, getEdgeDotStrings(module, indent="  "))
  res <- c(res, getNodeDotStrings(module, indent="  "))
  res <- c(res, '}\n')
  
  paste(res, collapse="")
}


getNodeDotStrings <- function(module, indent="") {
  if (length(V(module)) == 0) {
    return(NULL)
  }
  attr.values <- as_data_frame(module, what="vertices")
  style.attr.values <- getDotNodeStyleAttributes(attr.values)
  # ignoring technical nodeType and big pathway attributes
  tooltip <- getDotTooltip(attr.values[, !colnames(attr.values) %in% c("pathway", "nodeType")])
  url <- attr.values$url
  
  attr.dotStrings <- getAttrDotStrings(cbind(style.attr.values, tooltip=tooltip))
  if (length(url) != 0) {
    attr.dotStrings <- cbind(attr.dotStrings,
                             getAttrDotStrings(data.frame(URL=url, target="_blank")))
  }
  
  
  if(is.null(V(module)$name))
  {
    V(module)$name <- as.character(V(module))
  }
  
  
  node.label <- V(module)$name
  node.id <- as.vector(V(module))
  node.attrs <- apply(attr.dotStrings, 1, function(x) paste(na.omit(x), collapse=", "))
  nodeStrings <- sprintf("%sn%s [ %s ];\n", indent, node.id, node.attrs)
  nodeStrings
}

getEdgeDotStrings <- function(module, indent="") {
  if (length(E(module)) == 0) {
    return(NULL)
  }
  attr.values <- as_data_frame(module, what="edges")
  style.attr.values <- getDotEdgeStyleAttributes(attr.values)
  
  # ignoring big pathway attribute
  tooltip.values <- attr.values[, !colnames(attr.values) %in% c("pathway")]
  # for readability replacing node IDs with labels
  tooltip.values$from <- V(module)[tooltip.values$from]$label
  tooltip.values$to <- V(module)[tooltip.values$to]$label
  tooltip <- getDotTooltip(tooltip.values)
  
  url <- attr.values$url
  
  
  attr.dotStrings <- getAttrDotStrings(cbind(style.attr.values, tooltip=tooltip, labeltooltip=tooltip))
  
  if (length(url) != 0) {
    attr.dotStrings <- cbind(attr.dotStrings,
                             getAttrDotStrings(data.frame(URL=url, target="_blank")))
  }
  
  edgelist.names <- get.edgelist(module, names=TRUE)
  edgelist.names <- paste(edgelist.names[,1], edgelist.names[,2], sep=" (pp) ")
  edgelist.ids <- get.edgelist(module, names=FALSE)
  
  edge.attrs <- apply(attr.dotStrings, 1, function(x) paste(na.omit(x), collapse=", "))
  edgeStrings <- sprintf("%sn%s -- n%s [ %s ];\n",
                         indent,
                         edgelist.ids[,1],
                         edgelist.ids[,2],
                         edge.attrs)
  edgeStrings
}