\name{lowess}
\alias{lowess}
\alias{lowess.default}
\alias{lowess.formula}
\alias{plot.lowess}
\alias{plotLowess}
\title{Scatterplot Smoothing}
\description{
  This function augments the base \code{\link[stats]{lowess}} function
  by adding \code{formula} and \code{plot} methods.

  The \code{plot} method can be called directly on the object
  returned from \code{lowess} to draw a scatterplot of the original data
  with a smooth line superimposed.

  The \code{plotLowess} function both calculates the \code{lowess}
  smooth and plots the original data with a smooth line.
}
\usage{
lowess(x, \dots)

\method{lowess}{default}(x, y=NULL, f=2/3, iter=3L,
       delta=0.01*diff(range(x)), \dots)

\method{lowess}{formula}(formula,data=parent.frame(), \dots, subset,
       f=2/3, iter=3L, delta=0.01*diff(range(mf[-response])))

\method{plot}{lowess}(x, y, \dots, col.lowess="red", lty.lowess=2)

plotLowess(formula, data=parent.frame(), \dots, subset=parent.frame(),
           col.lowess="red", lty.lowess=2)
}
\arguments{
  \item{formula}{formula providing a single dependent variable (y) and a
    single independent variable (x) to use as coordinates in the
    scatterplot.}
  \item{data}{a \code{data.frame} or list from which the variables in
    \code{formula} should be taken.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{x, y}{vectors giving the coordinates of the points in the
    scatterplot. Alternatively a single plotting structure can be
    specified.}
  \item{f}{the smoother span. This gives the proportion of points in the
    plot which influence the smooth at each value. Larger values give
    more smoothness.}
  \item{iter}{the number of robustifying iterations which should be
    performed. Using smaller values of \code{iter} will make
    \code{lowess} run faster.}
  \item{delta}{values of \code{x} which lie within \code{delta} of each
    other replaced by a single value in the output from \code{lowess}.}
  \item{\dots}{parameters for methods.}
  \item{col.lowess, lty.lowess}{color and line type for plotted line.}
}
\seealso{
  The base \code{\link{lowess}} help page provides further details about
  the smoothing algorithm.

  \code{\link{loess}} is a newer smoothing algorithm with different
  defaults.

  \code{\link{gplots-package}} gives an overview of the package.
}
\examples{
# x,y method
plot(cars$speed, cars$dist, main="lowess(cars)")
lines(lowess(cars$speed, cars$dist), col=2)
lines(lowess(cars$speed, cars$dist, f=0.2), col=3)
legend(5, 120, c(paste("f=", c("2/3", ".2"))), lty=1, col=2:3)

# Formula method: plot, calculate lowess smoother, add smooth to plot
plot(dist ~ speed, data=cars, main="lowess(cars)")
lines(lowess(dist ~ speed, data=cars), col=2, lty=2)
lines(lowess(dist ~ speed, data=cars, f=0.2), col=3) # smaller bandwith
legend(5, 120, c(paste("f=", c("2/3", ".2"))), lty=1, col=2:3)

# Formula method: calculate lowess smoother, plot lowess object
lw <- lowess(dist ~ speed, data=cars)
plot(lw, main="lowess(cars)")

# Formula method: calculate and plot in a single command
plotLowess(dist ~ speed, data=cars, main="lowess(cars)")

\dontshow{
# Test non-standard evaluation
with(cars, plotLowess(dist ~ speed, main="lowess(cars)"))
with(cars, plot(lowess(dist ~ speed, main="lowess(cars)")))

idx <- cars$speed > 10
plotLowess(dist[idx] ~ speed[idx], data=cars, main="lowess(cars)")
plot(lowess(dist[idx] ~ speed[idx], data=cars, main="lowess(cars)"))

with(cars, plotLowess(dist[idx] ~ speed[idx], main="lowess(cars)"))
with(cars, plot(lowess(dist[idx] ~ speed[idx], main="lowess(cars)")))

local({
dist <- cars$dist
speed <- cars$speed
plotLowess(dist[idx] ~ speed[idx], main="lowess(cars)")
plot(lowess(dist[idx] ~ speed[idx], main="lowess(cars)"))
})
}
}
\keyword{smooth}
