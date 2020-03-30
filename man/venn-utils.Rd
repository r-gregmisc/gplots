\name{venn-utils}
\alias{drawVennPolyominoes.colouring.featureCount}
\alias{drawVennPolyominoes.colouring.pValue}
\alias{drawVennPolyominoes.fields.labels.binary}
\alias{drawVennPolyominoes.fields.labels.letter}
\alias{drawVennPolyominoes.fields.values.direct}
\alias{venn.graph.colouring}
\alias{venn.graph.simplify}
\alias{venn.observed2expected}
\title{Handling multidimensional Venn diagrams as Polyominoes or a Graph}
\description{
  A collection of routines that support the interaction of the Venn plotting
  routines with the data.
}
\usage{
venn.graph.simplify(graph,simplification=NULL,verbose=F,\dots)
drawVennPolyominoes.colouring.featureCount(current.rowname,data)
drawVennPolyominoes.colouring.pValue(current.rowname,data,factor=100)
drawVennPolyominoes.fields.labels.binary(
    current.rowname,data,x,y,x.max,y.max,cex=c(1,1,0.3),
    debug=F)
drawVennPolyominoes.fields.labels.letter(
    current.rowname, data,x,y,x.max,y.max,
    cex=c(2,1,0.6,0.4,rep(0.3,length.out=(ncol(data)-1)))[ncol(data)], debug=F)
venn.graph.colouring(data,
    col=NULL, col.scheme=rainbow, col.function=NULL,
    debug=FALSE)
drawVennPolyominoes.fields.labels.binary(
    current.rowname,
    data,x,y,x.max,y.max,
    cex=c(1,1,0.3), debug=F)
drawVennPolyominoes.fields.values.direct(
    current.rowname, data, x,y,x.max,y.max,
    #     1 2 3 4   5   6   7   8   9
    cex=c(4,3,2,1,0.6,0.6,0.4,0.4,0.3)[ncol(data)-1], debug=F)
venn.observed2expected(counts,new.total=NA)
}
\arguments{
  \item{current.rowname}{specifies row in output of Venn function}
  \item{data}{Venn matrix to integrate}
  \item{graph}{Venn graph representation for which to reduce the complexity}
  \item{simplification}{either a function that knows how to simplify, a subset of parameters from c("noUniverse","minNum1") passed as a list, passing just TRUE activates all parameters.}
  \item{factor}{parameter directing the display of P values}
  \item{x,y,x.max,y.max,col}{To be added}
  \item{cex}{font size}
  \item{col.scheme}{prepares a range of colors}
  \item{col.function}{maps values to colors of col.scheme}
  \item{counts,new.total}{To be added}
  \item{verbose}{set to true for some verbosity}
  \item{debug}{set to true for extended verbosity}
  \item{\dots}{optional graphical parameters.}
}
\details{
  Pending to be added. For now please follow the vignette.
}
\author{Steffen Möller \email{steffen.moeller@uni-rostock.de}.}
\seealso{
  \code{\link{plot.venn}}, \code{\link{venn}},
}
\keyword{misc}
