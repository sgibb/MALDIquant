\name{.estimateBaselineMedian}
\alias{.estimateBaselineMedian}
\title{Estimates the baseline by moving estimators.}
\description{
This function estimates the baseline of mass spectrometry data.\cr
The baseline estimation is based on median which is used in a
moving window.\cr
It is hidden by \code{NAMESPACE} because it is a private function and
\code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}} should be used
instead.
}
\usage{
.estimateBaselineMedian(x, y,
    halfWindowSize=100)
}
\arguments{
  \item{x}{vector of x values}
  \item{y}{vector of y values}
  \item{halfWindowSize}{half size of the moving window for the median
    (based on \code{\link{runmed}})\cr
    The resulting window reaches from \code{mass[cur_index-halfWindowSize]}
    to \code{mass[cur_index+halfWindowSize]}.}
}
\value{
  Returns a two column matrix (first column: mass, second column: intensity)
  of the estimated baseline.
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}},
\code{\link[MALDIquant]{removeBaseline,MassSpectrum-method}}
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

## estimate baseline
b <- estimateBaseline(s, method="median")

## draw baseline on the plot
lines(b, col="red")
}
\keyword{internal}

