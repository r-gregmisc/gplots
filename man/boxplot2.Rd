\name{boxplot2}
\alias{boxplot2}
\title{Annotated Boxplot}
\description{
  This function uses \code{boxplot} to produce a boxplot which is then
  annotated with the number of observations in each group.
}
\usage{
boxplot2(\dots, top=FALSE, shrink=1, textcolor=NULL)
}
\arguments{
  \item{\dots}{ parameters passed to \code{boxplot}. }
  \item{top}{ logical indicating whether the number of observations
    should be added to the top or the bottom of the plotting
    region. Defaults to \code{FALSE}. }
  \item{shrink}{ value to shrink character size (cex) when annotating.}
  \item{textcolor}{ text color. }
}
\author{ Gregory R. Warnes \email{greg@warnes.net}}
\note{
  This function replaces \code{boxplot.n}, which has been deprecated
  avoid potential problems with S3 method dispatching.
}
\seealso{
  \code{\link{boxplot}}, \code{\link{text}}.

  \code{\link{gplots-package}} gives an overview of the package.
}

\examples{
data(state)

# n's at bottom
boxplot2( state.area ~ state.region)

# n's at top
boxplot2( state.area ~ state.region, top=TRUE)

# small red text
boxplot2( state.area ~ state.region, shrink=0.8, textcolor="red") 
}

\keyword{ hplot }
