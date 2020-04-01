# Authors: Steffen Moeller and Sarah Fischer
#          University of Rostock, Germany, 2017
plotVennGraph<-function(x,
                        col=NULL,
                        col.scheme=rainbow,
                        col.function=NULL,
                        add=FALSE, debug=FALSE, ...)
{
  if (is.null(x)) stop("plotVennGraph: is.null(x)")

  g<-NULL
  if ("igraph"==class(x)) {
    if (!is.null(col.function)) {
      warning("plotVennGraph: submit object returned from venn() for colouring with col.function, not the graph")
    }
    g <- x
  } else {
    g <- venn.graph.colouring(x, col=col, col.scheme=col.scheme, col.function=col.function, ...)
  }

  if (is.null(igraph::V(g)$size))      igraph::V(g)$size <- 20
  if (is.null(igraph::V(g)$label.cex)) igraph::V(g)$label.cex=0.7

  if ("igraph" != class(g)) {
    cat("plotVennGraph: class(g) == ",class(g),"\n",sep="")
    stop("plotVennGraph: Not working on a graph - likely internal error")
  } 
  igraph::plot.igraph(g, add=add, ...)

  if ("igraph" == class(x)) {
    invisible(g)
  } else {
    attr(x,"graph") <- g
    invisible(x)
  }
}
