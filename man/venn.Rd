\name{venn}
\alias{venn}
\alias{plot.venn}
\alias{plot.venn.graph}
\alias{plot.venn.polyominoes}
\alias{drawVennPolyominoes}
\title{Venn Diagram}
\description{Plot a Venn diagrams for up to 5 sets.}
\usage{
venn(x, universe=NA, small=0.7, showSetLogicLabel=FALSE,
     simplify=FALSE, show.plot=TRUE, intersections=TRUE, graph=FALSE, names=NULL,
     statistics=NULL, n.perm = 100, \dots)

\method{plot}{venn}(x, y, \dots, small=0.7, showSetLogicLabel=FALSE,
     simplify=FALSE, type = "regular",
     col = NA, density = NULL, border = NULL, angle = NA, lty = par("lty"),
     debug=F)
plot.venn.graph(x,
     col=NULL, col.scheme=rainbow, col.function=NULL,
     add=FALSE, debug=FALSE, \dots)
plot.venn.polyominoes(x,y,\dots,
     col.bg="gray", col.scheme=rainbow,
     col.function=drawVennPolyominoes.colouring.featureCount,
     fields.label.function=drawVennPolyominoes.fields.labels.binary,
     fields.value.function=drawVennPolyominoes.fields.values.direct,
     cex.label=0.5,cex.value=0.9, debug=F)
drawVennPolyominoes(data,col.bg="lightgray",
     col.scheme=NULL,
     col.function=NULL,
     fields.label.function=drawVennPolyominoes.fields.labels.binary,
     fields.value.function=drawVennPolyominoes.fields.values.direct,
     cex.label=0.5,cex.value=0.9, add=F, h=400,
     debug=F)
}
\arguments{
  \item{x}{either a list containing vectors of names or indices of
     group intersections, or a data frame containing boolean indicators
     of group intersectionship (see below).}
  \item{data}{resturn value of venn(), i.e. a matrix representation of
     the Venn diagram}
  \item{universe}{subset of valid name/index elements. Values ignore
    values in code{data} not in this list will be ignored. Use \code{NA}
    to use all elements of \code{data} (the default).}
  \item{small}{character scaling of the smallest group counts.}
  \item{showSetLogicLabel}{logical flag indicating whether the internal
    group label should be displayed.}
  \item{simplify}{logical flag indicating whether unobserved groups
    should be omitted.}
  \item{show.plot}{logical flag indicating whether the plot should be
    displayed. If \code{FALSE}, simply returns the group count matrix.}
  \item{intersections}{Logical flag indicating if the returned object
    should have the attribute \code{"individuals.in.intersections"}
    featuring for every set a list of individuals that are assigned to
    it.}
  \item{graph}{set to true to create a graph representation of the
    "part-of" relation}
  \item{names}{optional vector of group names.}
  \item{statistics}{determine how P values shall be determined for
    the values in the Venn diagram. Only "permutations" has yet been
    implemented.}
  \item{n.perm}{If selecting a permutation test in statistics, this
    is how often the venn diagrams are created with random orderings of
    individual sets, so we can learn about how special a value is.}
  \item{y}{ignored.}
  \item{type}{Determines the kind of plot to perform, either "regular",
    "polyominoes", or "graph"}
  \item{col}{vector of color values, one for each dimension}
  \item{density}{vector of values between 0 and 1 - allows to adjust
    the dominance of dimensions by varying their transparency}
  \item{border}{vector of border value of circle/polygons plotted to
    represent each dimension}
  \item{angle}{vector of values passed to circle/polygons, determines
    the orientation of the ellipses - you likely also want to set the
    debug option if fiddling wiht this}
  \item{lty}{vector of values passed to circle/polygon to indicate the
    thickness of lines individually for every dimension}
  \item{debug}{set to true for extra verbosity}
  \item{\dots}{optional graphical parameters.}
  \item{add}{passed to plot routine, so the graph becomes an overlay to
    an existing plot}
  \item{col.scheme}{function to pass that generates a list of colors}
  \item{col.function}{function to pass that assigns values to these colors}
  \item{col.bg}{color of background}
  \item{cex.label}{font size for labels identifying intersections/boxes in diagram}
  \item{cex.value}{font size for values placed in the diagram}
  \item{fields.value.function}{function that knows how to place the
    value itself or something derived like a P value}
  \item{fields.label.function}{function that knows how to label a box
    in a polyominoe}
  \item{h}{height}
}
\details{
  \code{data} should be either a named list of vectors containing
  character string names (\code{"GeneAABBB"}, \code{"GeneBBBCY"}, ..,
  \code{"GeneXXZZ"}) or indexes of group intersections (1, 2, .., N), or
  a data frame containing indicator variables (\code{TRUE},
  \code{FALSE}, \code{TRUE}, ..) for group intersectionship. Group names
  will be taken from the component list element or column names.
}
\value{
  Invisibly returns an object of class "venn", containing:
  \itemize{
    \item A matrix of all possible sets of groups, and the observed
      count of items belonging to each The fist column contains observed
      counts, subsequent columns contain 0-1 indicators of group
      intersectionship.
    \item If \code{intersections=TRUE}, the attribute
      \code{intersections} will be a list of vectors containing the
      names of the elements belonging to each subset.
  }
}
\author{
  Steffen Moeller \email{steffen_moeller@gmx.de}, minor edits by Gregory
  R. Warnes \email{greg@warnes.net}.
}
\seealso{
  \code{\link{gplots-package}} gives an overview of the package.
  \code{\link[graphics]{polygon}} for a better understanding of angle, border, density, col and lty arguments.
}
\examples{
# Example using a list of item names belonging to the specified group

# Construct some fake gene names
oneName <- function() paste(sample(LETTERS,5,replace=TRUE),collapse="")
geneNames <- replicate(1000, oneName())

GroupA <- sample(geneNames, 400, replace=FALSE)
GroupB <- sample(geneNames, 750, replace=FALSE)
GroupC <- sample(geneNames, 250, replace=FALSE)
GroupD <- sample(geneNames, 300, replace=FALSE)
input <- list(GroupA, GroupB, GroupC, GroupD)
input

tmp <- venn(input)
attr(tmp, "intersections")

# List of item indexes belonging to the specified group
GroupA.i <- which(geneNames \%in\% GroupA)
GroupB.i <- which(geneNames \%in\% GroupB)
GroupC.i <- which(geneNames \%in\% GroupC)
GroupD.i <- which(geneNames \%in\% GroupD)
input.i <-list(A=GroupA.i, B=GroupB.i, C=GroupC.i, D=GroupD.i)
input.i

venn(input.i)

# Data frame of indicator ('f'lag) columns
GroupA.f <- geneNames \%in\% GroupA
GroupB.f <- geneNames \%in\% GroupB
GroupC.f <- geneNames \%in\% GroupC
GroupD.f <- geneNames \%in\% GroupD
input.df <- data.frame(A=GroupA.f, B=GroupB.f, C=GroupC.f, D=GroupD.f)
head(input.df)
venn(input.df)

# Smaller set to create empty groupings
small <- input.df[1:20,]

venn(small, simplify=FALSE)  # with empty groupings
venn(small, simplify=TRUE)   # without empty groupings

# Capture group counts, but don't plot
tmp <- venn(input, show.plot=FALSE)
tmp

# Show internal binary group labels
venn(input, showSetLogicLabel=TRUE)

# Limit universe
tmp <- venn(input, universe=geneNames[1:100])
tmp

# Determine which elements are in A and B but not in C and D
# using the 'intersections' attribute
tmp <- venn(input, intersection=TRUE)
isect <- attr(tmp, "intersection")

# Look at all of the subsets
str(isect)

# Extract and combine the subsets of interest
AandB <- unique(c(isect$A, isect$B, isect$'A:B'))
# and look at the results
str(AandB)

# The full set of elements of each intersection is provided in the
# 'interesections' attribute.

a <- venn(list(1:5,3:8), show.plot=FALSE)
intersections <- attr(a, "intersections")
print(intersections)
# $A
# [1] "1" "2"
#
# $B
# [1] "6" "7" "8"
#
# $`A:B`
# [1] "3" "4" "5"
}
\keyword{hplot}
