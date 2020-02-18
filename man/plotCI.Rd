\name{plotCI}
\alias{plotCI}
\title{Plot Confidence Intervals}
\description{
  Given a set of x and y values and interval width or upper and lower
  bounds, plot the points with error bars. This can be a useful tool for
  visualizing confidence intervals.
}
\usage{
plotCI(x, y=NULL, uiw, liw=uiw, ui, li, err='y', ylim=NULL, xlim=NULL,
       type="p", col=par("col"), barcol=col, pt.bg=par("bg"),
       sfrac=0.01, gap=1, lwd=par("lwd"), lty=par("lty"), labels=FALSE,
       add=FALSE, xlab, ylab, minbar, maxbar, \dots)
}
\arguments{
  \item{x,y}{coordinates for the center of error bars. \code{y} defaults
    to \code{1:n}.}
  \item{uiw}{width of the upper or right error bar. Set to \code{NULL}
    or \code{NA} to omit upper bars.}
  \item{liw}{width of the lower or left error bar. Defaults to same
    value as \code{uiw}. Set to \code{NULL} or \code{NA} to omit lower
    bars.}
  \item{ui}{upper end of error bars. Defaults to \code{y + uiw} or
    \code{x + uiw} depeding on \code{err}. Set to \code{NULL} or
    \code{NA} to omit upper bars.}
  \item{li}{lower end of error bars. Defaults to \code{y - liw} or
    \code{x - liw} depedning on \code{err}. Set to \code{NULL} or
    \code{NA} to omit lower bars.}
  \item{err}{direction for error bars. Set to "y" for vertical bars. Set
    to "x" for horizontal bars. Defaults to "y".}
  \item{col}{color of plotting character used center marker of error
    bars. Default is "black".}
  \item{xlim, ylim}{range of x/y values to include in the plotting
    area.}
  \item{type}{point/line type; passed to \code{\link{points}}}
  \item{barcol}{color of the error bars. Defaults to the same value as
    \code{col}}
  \item{pt.bg}{background color of points (use \code{pch=21,
    pt.bg=par("bg")} to get open points superimposed on error bars).}
  \item{sfrac}{width of "crossbar" at the end of error bar as a fraction
    of the x plotting region. Defaults to 0.01.}
  \item{gap}{space left between the center of the error bar and the
    lines marking the error bar in units of the height (width) of the
    letter "O". Defaults to 1.0}
  \item{lwd}{width of bar lines.}
  \item{lty}{line type of bar lines.}
  \item{labels}{either a logical value indicating whether the circles
    representing the x values should be replaced with text giving the
    actual values or a vector containing labels to use instead. Defaults
    to \code{FALSE}.}
  \item{add}{logical indicating whether error bars should be added to
    the current plot. If \code{FALSE} (the defailt), a new plot will be
    created and symbols/labels for the x values will be plotted before
    drawing error bars.}
  \item{minbar}{minumum allowed value for bar ends. If specified, values
    smaller than \code{minbar} will be replaced with \code{minbar}.}
  \item{maxbar}{maximum allowed value for bar ends. If specified, values
    larger than \code{maxbar} will be replaced with \code{maxbar}.}
  \item{\dots}{optional plotting parameters}
  \item{xlab}{label for x axis.}
  \item{ylab}{label for y axis.}
}
\author{
  Original version by Bill Venables
  \email{wvenable@attunga.stats.adelaide.edu.au} posted to r-help on
  Sep. 20, 1997. Enhanced version posted to r-help by Ben Bolker
  \email{ben@zoo.ufl.edu} on Apr. 16, 2001. This version was modified
  and extended by Gregory R. Warnes \email{greg@warnes.net}. Additional
  changes suggested by Martin Maechler
  \email{maechler@stat.math.ethz.ch} integrated on July 29, 2004.
}
\seealso{
  \code{\link{plotmeans}} provides an enhanced wrapper to \code{plotCI}.

  \code{\link{gplots-package}} gives an overview of the package.
}
\examples{
  # plot means and
  data(state)
  tmp   <- split(state.area, state.region)
  means <- sapply(tmp, mean)
  stdev <- sqrt(sapply(tmp, var))
  n     <- sapply(tmp,length)
  ciw   <- qt(0.975, n) * stdev / sqrt(n)

  # plain
  plotCI(x=means, uiw=ciw)

  # prettier
  plotCI(x=means, uiw=ciw, col="black", barcol="blue", lwd=1)

  # give mean values
  plotCI(x=means, uiw=ciw, col="black", barcol="blue",
         labels=round(means,-3), xaxt="n", xlim=c(0,5))
  axis(side=1, at=1:4, labels=names(tmp), cex=0.7)

  # better yet, just use plotmeans ...
  plotmeans(state.area ~ state.region)

\dontshow{
## Just for testing
  plotCI(x=means, uiw=NA)
  plotCI(x=means, uiw=NULL)
  plotCI(x=means, uiw=ciw)
  plotCI(x=means, uiw=ciw, liw=NULL)
  plotCI(x=means, uiw=ciw, liw=NA)
  plotCI(x=means, liw=ciw, ciw=NULL)
  plotCI(x=means, liw=ciw, ciw=NA)
  ciw.na <- ciw
  ciw.na[3] <- NA
  plotCI(x=means, uiw=ciw.na, liw=ciw)
  plotCI(x=means, liw=ciw.na, uiw=ciw)
}
}
\keyword{hplot}
