\name{removeBaseline-methods}
\docType{methods}
\alias{removeBaseline}
\alias{removeBaseline,MassSpectrum-method}
\alias{removeBaseline,list-method}
\title{Removes the baseline of a MassSpectrum object.}
\description{
This method removes the baseline of mass spectrometry data
(represented by a \code{\linkS4class{MassSpectrum}} object).\cr
The intensity of the mass spectrometry data would be reduced by
\code{baseline}.
}
\usage{
\S4method{removeBaseline}{MassSpectrum}(object,
  method=c("SNIP", "TopHat", "ConvexHull", "median"),
  \dots)
}
\arguments{
  \item{object}{\code{\linkS4class{MassSpectrum}} object or a \code{list} of
    \code{\linkS4class{MassSpectrum}} objects.}
  \item{method}{a baseline estimation function; see
    \code{\link[MALDIquant]{.estimateBaselineSnip}},
    \code{\link[MALDIquant]{.estimateBaselineTopHat}},
    \code{\link[MALDIquant]{.estimateBaselineConvexHull}},
    \code{\link[MALDIquant]{.estimateBaselineMedian}}.
  }
  \item{\dots}{arguments to be passed to
    \code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}}.}
}
\value{
  Returns a modified \code{\linkS4class{MassSpectrum}} object with reduced
  intensities.
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}}
}
\examples{
## load package
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## choose only the first mass spectrum
s <- fiedler2009subset[[1]]

## plot spectrum
plot(s)

## subtract baseline
b <- removeBaseline(s, method="SNIP")

## draw modified spectrum on the plot
lines(b, col="blue")
}
\keyword{methods}

