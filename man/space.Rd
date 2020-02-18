\name{space}
\alias{space}
\title{Spaced Scatterplot}
\description{Space points in an x-y plot so they don't overlap.}
\usage{
space(x, y, s=1/50, na.rm=TRUE, direction="x")
}
\arguments{
  \item{x}{numeric vector of x coordonates.}
  \item{y}{numeric vector of x coordonates.}
  \item{s}{either a single numeric value or 2 element vector specifying
    the minimum distance between points in the x and y dimensions as a
    fraction of the x and y range. Defaults to 1/50.}
  \item{na.rm}{logical indicating whether pairs where one or both
    elements are missing should be removed. Defaults to TRUE.}
  \item{direction}{"x" or "y", indicating which direction points should
    be moved to accomplish spacine.}
}
\details{
  In an x-y plot where at least one variable has discrete levels several
  points may be plotted at or very near the same coordonates. This makes
  it difficult to guage the number of points in a specific region. A
  common method of resolving this problem is to 'jitter' the points by
  adding random noise.

  This function takes a different approach to the same problem.

  When there are two or more points with the same (x,y) value (or within
  x+-s[1] and x+-s[2]), it spaces these out in the x direction so that
  the points are separated by at least distance s.

  Another method for dealing with overploting is available in the
  \code{\link{sunflowerplot}} function.
}
\value{
  list with two components
  \item{x}{(modified) x location for each input point}
  \item{y}{y location of each input point}
}
\author{Gregory R. Warnes \email{greg@warnes.net}}
\seealso{
  \code{\link{jitter}}, \code{\link{sunflowerplot}}.

  \code{\link{gplots-package}} gives an overview of the package.
}
\examples{
x <- rep(1:5, 10)
y <- round(rnorm(length(x),x))

prepar <- par("mfrow")
par(mfrow=c(1,3))

# standard x-y plot: noverlapping points are hidden
plot(x, y)
title("Standard Plot")

# 'spaced' plot: overlapping points are spread out and visible
plot(space(x, y))
title("Plot with 'space'")


# 'spaced' plot: overlapping points are spread out along y and visible
plot(space(x, y, direction='y'))
title("Plot with 'space', direction='y' ")


# 'sunflower' plot, another approach, overlapping points are
# indicated via petals
sunflowerplot(x,y)
title("Sunflower Plot")

\testonly{
# check that missign values correctly handled
x <- c(x,NA)
y <- c(y,NA)
plot(space(x,y))
}

par(mfrow=prepar)
}
\keyword{dplot}
