\name{barplot2}
\alias{barplot2}
\alias{barplot2.default}
\title{Enhanced Bar Plots}
\description{
  An enhancement of the standard \code{barplot} function, allowing
  addition of confidence intervals and/or a background grid.
}
\usage{
\method{barplot2}{default}(height, width=1, space=NULL, names.arg=NULL,
         legend.text=NULL, beside=FALSE, horiz=FALSE, density=NULL,
         angle=45, col=NULL, prcol=NULL, border=par("fg"), main=NULL,
         sub=NULL, xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, xpd=TRUE,
         log="", axes=TRUE, axisnames=TRUE, cex.axis=par("cex.axis"),
         cex.names=par("cex.axis"), inside=TRUE, plot=TRUE, axis.lty=0,
         offset=0, plot.ci=FALSE, ci.l=NULL, ci.u=NULL,
         ci.color="black", ci.lty="solid", ci.lwd=1, ci.width=0.5,
         plot.grid=FALSE, grid.inc=NULL, grid.lty="dotted", grid.lwd=1,
         grid.col="black", add=FALSE, panel.first=NULL, panel.last=NULL,
         \dots)
}
\arguments{
  \item{height}{either a vector or matrix of values describing the bars
    which make up the plot. If \code{height} is a vector, the plot
    consists of a sequence of rectangular bars with heights given by the
    values in the vector. If \code{height} is a matrix and \code{beside}
    is \code{FALSE} then each bar of the plot corresponds to a column of
    \code{height}, with the values in the column giving the heights of
    stacked sub-bars making up the bar. If \code{height} is a matrix and
    \code{beside} is \code{TRUE}, then the values in each column are
    juxtaposed rather than stacked.}
  \item{width}{optional vector of bar widths. Recycled to have the same
    length as the number of bars drawn. Specifying a single value will
    have no visible effect unless \code{xlim} is specified.}
  \item{space}{the amount of space (as a fraction of the average bar
    width) left before each bar. May be given as a single number or one
    number per bar. If \code{height} is a matrix and \code{beside} is
    \code{TRUE}, \code{space} may be specified by two numbers, where the
    first is the space between bars in the same group, and the second
    the space between the groups. If not given explicitly, it defaults
    to \code{c(0, 1)} if \code{height} is a matrix and \code{beside} is
    \code{TRUE}, and to \code{0.2} otherwise.}
  \item{names.arg}{a vector of names to be plotted below each bar or
    group of bars. If this argument is omitted, then the names are taken
    from the \code{names} attribute of \code{height} if this is a
    vector, or the column names if it is a matrix.}
  \item{legend.text}{a vector of text used to construct a legend for the
    plot, or a logical indicating whether a legend should be included.
    This is only useful when \code{height} is a matrix and the given
    legend labels correspond to the rows of \code{height}. If
    \code{legend.text} is \code{TRUE}, the row names of \code{height}
    will be used as labels if they are not \code{NULL}.}
  \item{beside}{a logical value. If \code{FALSE}, the columns of
    \code{height} are portrayed as stacked bars, and if \code{TRUE} the
    columns are portrayed as juxtaposed bars.}
  \item{horiz}{a logical value. If \code{FALSE}, the bars are drawn
    vertically with the first bar to the left. If \code{TRUE}, the bars
    are drawn horizontally with the first at the bottom.}
  \item{density}{a vector giving the the density of shading lines, in
    lines per inch, for the bars or bar components. The default value of
    \code{NULL} means that no shading lines are drawn. Non-positive
    values of \code{density} also inhibit the drawing of shading lines.}
  \item{angle}{the slope of shading lines, given as an angle in degrees
    (counter-clockwise), for the bars or bar components.}
  \item{col}{a vector of colors for the bars or bar components. By
    default, grey is used if \code{height} is a vector, and
    \code{heat.colors(nrow(height))} if \code{height} is a matrix.}
  \item{prcol}{the color to be used for the plot region.}
  \item{border}{the color to be used for the border of the bars.}
  \item{main, sub}{overall and sub titles for the plot.}
  \item{xlab}{a label for the x axis.}
  \item{ylab}{a label for the y axis.}
  \item{xlim}{limits for the x axis.}
  \item{ylim}{limits for the y axis.}
  \item{xpd}{logical. Should bars be allowed to go outside region?}
  \item{log}{a character string which contains \code{"x"} if the x axis
    is to be logarithmic, \code{"y"} if the y axis is to be logarithmic
    and \code{"xy"} or \code{"yx"} if both axes are to be logarithmic.}
  \item{axes}{logical. If \code{TRUE}, a vertical (or horizontal, if
    \code{horiz} is \code{TRUE}) axis is drawn.}
  \item{axisnames}{logical. If \code{TRUE}, and if there are
    \code{names.arg} (see above), the other axis is drawn (with
    \code{lty = 0}) and labeled.}
  \item{cex.axis}{expansion factor for numeric axis labels.}
  \item{cex.names}{expansion factor for names.}
  \item{inside}{logical. If \code{TRUE}, the lines which divide adjacent
    (non-stacked!) bars will be drawn. Only applies when
    \code{space = 0} (which it partly is when \code{beside = TRUE}).}
  \item{plot}{logical. If \code{FALSE}, nothing is plotted.}
  \item{axis.lty}{the graphics parameter \code{lty} applied to the axis
    and tick marks of the categorical (default horzontal) axis. Note
    that by default the axis is suppressed.}
  \item{offset}{a vector indicating how much the bars should be shifted
    relative to the x axis.}
  \item{plot.ci}{logical. If \code{TRUE}, confidence intervals are
    plotted over the bars. Note that if a stacked bar plot is generated,
    confidence intervals will not be plotted even if
    \code{plot.ci = TRUE}.}
  \item{ci.l, ci.u}{the confidence intervals (\code{ci.l} = lower bound,
    \code{ci.u} = upper bound) to be plotted if \code{plot.ci} =
    \code{TRUE}. Values must have the same dim structure as
    \code{height}.}
  \item{ci.color}{the color for the confidence interval line segments.}
  \item{ci.lty}{the line type for the confidence interval line
    segments.}
  \item{ci.lwd}{the line width for the confidence interval line
    segments.}
  \item{ci.width}{length of lines used for the staple at the end of
    confidence interval line segments, as a multple of \code{width}.
    Defaults to \code{0.5}.}
  \item{plot.grid}{if \code{TRUE} a line grid will be plotted behind the
    bars.}
  \item{grid.inc}{the number of grid increments to be plotted.}
  \item{grid.lty}{the line type for the grid.}
  \item{grid.lwd}{the line width for the grid.}
  \item{grid.col}{the line color for the grid.}
  \item{add}{logical, if \code{TRUE} add barplot to current plot.}
  \item{panel.first}{expression to be evaluated after the plot region
    coordinates have been set up, but prior to the drawing of the bars
    and other plot region contents. This can be useful to draw
    additional plot region content behind the bars. This will also work
    if \code{add} = \code{TRUE}.}
  \item{panel.last}{expression to be evaluated after the bars have been
    drawn, but prior to the addition of confidence intervals, legend and
    axis annotation.}
  \item{\dots}{further graphical parameters (\code{\link{par}}) are
    passed to \code{\link{plot.window}}, \code{\link{title}} and
    \code{\link{axis}}.}
}
\value{
  A numeric vector (or matrix, when \code{beside = TRUE}), say
  \code{mp}, giving the coordinates of the bar midpoints drawn, useful
  for adding to the plot.

  If \code{beside} is \code{TRUE}, use \code{colMeans(mp)} for the
  midpoints of each \emph{group} of bars, see example.
}
\author{Marc Schwartz \email{marc\_schwartz@comcast.net}.}
\note{
  If there are \code{NA} values in \code{height} and
  \code{beside = FALSE}, values after the \code{NA} will not be plotted
  in stacked bars.
}
\seealso{
  \code{\link{plot}(\dots, type = "h")}, \code{\link{dotchart}},
  \code{\link{hist}}.

  \code{\link{gplots-package}} gives an overview of the package.
}
\examples{
tN <- table(Ni <- rpois(100, lambda=5))
r <- barplot2(tN, col='gray')

# type="h" plotting *is* `bar'plot
lines(r, tN, type='h', col='red', lwd=2)

barplot2(tN, space=1.5, axisnames=FALSE,
         sub="barplot2(..., space=1.5, axisnames=FALSE)")

barplot2(VADeaths, plot=FALSE)
barplot2(VADeaths, plot=FALSE, beside=TRUE)

mp <- barplot2(VADeaths) # default
tot <- colMeans(VADeaths)
text(mp, tot + 3, format(tot), xpd=TRUE, col="blue")
barplot2(VADeaths, beside=TRUE,
         col=c("lightblue", "mistyrose", "lightcyan",
               "lavender", "cornsilk"),
         legend=rownames(VADeaths), ylim=c(0, 100))
title(main="Death Rates in Virginia", font.main=4)

# Confidence intervals and grid
hh <- t(VADeaths)[, 5:1]
mybarcol <- "gray20"
ci.l <- hh * 0.85
ci.u <- hh * 1.15
mp <- barplot2(hh, beside=TRUE,
               col=c("lightblue", "mistyrose",
                     "lightcyan", "lavender"),
               legend=colnames(VADeaths), ylim=c(0, 100),
               main="Death Rates in Virginia", font.main=4,
               sub="Faked 95 percent error bars", col.sub=mybarcol,
               cex.names=1.5, plot.ci=TRUE, ci.l=ci.l, ci.u=ci.u,
               plot.grid=TRUE)
mtext(side=1, at=colMeans(mp), line=-2,
      text=paste("Mean", formatC(colMeans(hh))), col="red")
box()

# Horizontal bars, grid and logarithmic x axis
barplot2(1:10, log="x", plot.grid=TRUE, grid.inc=10, xlim=c(0.5, 20),
         horiz=TRUE, cex.axis=0.9, prcol="gray95")
box()

# Bar shading
barplot2(VADeaths, angle=15 + 10 * 1:5, density=20, col="black",
        legend=rownames(VADeaths))
title(main=list("Death Rates in Virginia", font=4))

# Border
barplot2(VADeaths, border="dark blue")
}
\keyword{hplot}
