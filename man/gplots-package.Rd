\name{gplots-package}
\alias{gplots-package}
\alias{gplots}
\docType{package}
\title{Various R Programming Tools for Plotting Data}
\description{Various R programming tools for plotting data, including:}
\details{
  \itemize{
    \item calculating and plotting locally smoothed summary function
      (\code{\link{bandplot}}, \code{\link{wapply}}),
    \item enhanced versions of standard plots (\code{\link{barplot2}},
      \code{\link{boxplot2}}, \code{\link{heatmap.2}},
      \code{\link{smartlegend}}),
    \item manipulating colors (\code{\link{col2hex}},
      \code{\link{colorpanel}}, \code{\link{redgreen}},
      \code{\link{greenred}}, \code{\link{bluered}},
      \code{\link{redblue}}, \code{\link{rich.colors}}),
    \item calculating and plotting two-dimensional data summaries
      (\code{\link{ci2d}}, \code{\link{hist2d}}),
    \item enhanced regression diagnostic plots (\code{\link{lmplot2}},
      \code{\link{residplot}}),
    \item formula-enabled interface to \code{stats::lowess}
      (\code{\link{lowess}}),
    \item displaying textual data in plots (\code{\link{textplot}},
      \code{\link{sinkplot}}),
    \item plotting a matrix where each cell contains a dot whose size
      reflects the relative magnitude of the elements
      (\code{\link{balloonplot}}),
    \item plotting "Venn" diagrams (\code{\link{venn}}),
    \item displaying Open-Office style plots (\code{\link{ooplot}}),
    \item plotting multiple data on same region, with separate axes
      (\code{\link{overplot}}),
    \item plotting means and confidence intervals (\code{\link{plotCI}},
      \code{\link{plotmeans}}),
    \item spacing points in an x-y plot so they don't overlap
      (\code{\link{space}}).
  }
}
\author{
  Gregory Warnes, Ben Bolker, Lodewijk Bonebakker, Robert Gentleman,
  Wolfgang Huber, Andy Liaw, Thomas Lumley, Martin Maechler, Arni
  Magnusson, Steffen Moeller, Marc Schwartz, and Bill Venables.
}
\references{
  Jain, N. and Warnes, G. R. (2006) Balloon Plot: Graphical tool for
  displaying tabular data.
  \href{https://cran.r-project.org/doc/Rnews/Rnews_2006-2.pdf#page=35}{
    \emph{R News}, \bold{6}(2), 35--38}.
}
\keyword{hplot}
\keyword{color}
\keyword{dplot}
