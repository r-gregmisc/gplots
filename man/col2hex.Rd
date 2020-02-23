\name{col2hex}
\alias{col2hex}
\title{Color to Hex}
\description{Convert color names to hex RGB strings.}
\usage{
col2hex(cname)
}
\arguments{
  \item{cname}{color name(s).}
}
\value{
  Character vector giving the hex color code translation of the provided
  color names.
}
\author{Gregory R. Warnes.}
\seealso{
  \code{\link{col2rgb}}, \code{\link{colors}}, \code{\link{rgb}}.

  \code{\link{gplots-package}} gives an overview of the package.
}
\examples{
col2hex(c("red", "yellow", "lightgrey"))
}
\keyword{color}
