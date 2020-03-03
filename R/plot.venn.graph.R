# Authors: Steffen Moeller and Sarah Fischer
#          University of Rostock, Germany, 2017
plot.venn.graph<-function(data,
                          col=NULL,
                          col.scheme=rainbow,
                          col.function=NULL,
                          add=FALSE, debug=FALSE, ...)
{
  if (is.null(data)) stop("drawVennDiagram: is.null(data)")

  require(igraph)
  requireNamespace("igraph")

  g<-NULL
  if ("igraph"==class(data)) {
    if (!is.null(col.function)) {
      warning("plot.venn.graph: submit object returned from venn() for colouring with col.function, not the graph")
    }
    g <- data
  } else {
    g <- venn.graph.colouring(data, col=col, col.scheme=col.scheme, col.function=col.function, ...)
  }

  if (is.null(V(g)$size))      V(g)$size <- 20
  if (is.null(V(g)$label.cex)) V(g)$label.cex=0.7

  if ("igraph" != class(g)) {
    cat("plot.venn.graph: class(g) == ",class(g),"\n",sep="")
    stop("plot.venn.graph: Not working on a graph - likely internal error")
  } 
  igraph::plot.igraph(g, add=add, ...)

  if ("igraph" == class(data)) {
    invisible(g)
  } else {
    attr(data,"graph") <- g
    invisible(data)
  }
}
