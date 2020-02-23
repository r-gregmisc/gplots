\name{angleAxis}
\alias{angleAxis}
\title{Add Rotated Axis Labels}
\description{Add a labeled axis to the current plot with rotated text.}
\usage{
angleAxis(side, labels, at=1:length(labels), srt=45, adj, xpd=TRUE,
          \dots)
}
\arguments{
  \item{side}{an integer specifying which side of the plot the axis is
    to be drawn on. The axis is placed as follows: 1=below, 2=left,
    3=above and 4=right.}
  \item{labels}{character vector of labels to be placed at the
    tickpoints.}
  \item{at}{the points at which tick-marks are to be drawn. Non-finite
    (\code{Inf}, \code{NaN} or \code{NA}) values are omitted.}
  \item{srt}{the string rotation in degrees. Defaults to 45 degrees
    (clockwise).}
  \item{adj}{text justification. A value of 0 produces left-justified
    text, 0.5 centered text and 1 right-justified text. For
    \code{side=1} and \code{side=2}, the default value is \code{adj=1}.
    For \code{side=3} and \code{side=4} the default value is
    \code{adj=0}.}
  \item{xpd}{a logical value or \code{NA}. If \code{FALSE}, labels are
    clipped to the plot region, if \code{TRUE}, labels are clipped to
    the figure region, and if \code{NA}, labels are clipped to the
    device region.}
  \item{\dots}{optional arguments passed to \code{text}. Common examples
    are \code{col}, \code{cex}.}
}
\author{Gregory R. Warnes \email{greg@warnes.net}.}
\seealso{
  \code{\link{axis}.}

  \code{\link{gplots-package}} gives an overview of the package.
}
\examples{
\dontshow{set.seed(42)}
# Create a vector with some values and long labels
values <- sample(1:10)
names(values) <- sapply(letters[1:10], function(x)
                        paste(rep(x, 10), sep="", collapse=""))

# Labels are too long for the available space, so some are not plotted
barplot(values)

# To add angled labels, supress x axis labels and store locations,
at <- barplot(values, xaxt="n")
# then use angleAxis
angleAxis(1, at=at, labels=names(values))

# Angle counter-clockwise instead
at <- barplot(values, xaxt="n")
angleAxis(1, at=at, labels=names(values), srt=-45, adj=0)

# Put labels at the top
oldpar <- par("mar")
par(mar=c(1,4,5,2)+0.1)
at <- barplot(values, xaxt="n")
angleAxis(3, at=at, labels=names(values))
par(oldpar)

# Put labels on the left
oldpar <- par("mar")
par(mar=c(5,5,3,2)+0.1)
at <- barplot(values, yaxt="n", horiz=TRUE)
angleAxis(2, at=at, labels=names(values))
par(oldpar)

# Put labels on the right
oldpar <- par("mar")
par(mar=c(2,5,3,5)+0.1)
at <- barplot(values, yaxt="n", horiz=TRUE)
angleAxis(4, at=at, labels=names(values))
par(oldpar)

# Specify colors for bars and labels
at <- barplot(values, xaxt="n", col=1:10)
angleAxis(1, at=at, labels=names(values), col=1:10)
}
\keyword{aplot}
