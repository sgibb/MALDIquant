\name{.movingAverage}
\alias{.movingAverage}
\title{Moving Average Smoothing Filter}
\description{This function runs a simple 2-side moving average.\cr
It is hidden by \code{NAMESPACE} because it is a private function and
\code{\link[MALDIquant]{smoothIntensity,MassSpectrum-method}} should be used
instead.}
\usage{
  .movingAverage(y, halfWindowSize=2)
}
\arguments{
  \item{y}{intensity values}

  \item{halfWindowSize}{half window size. The resulting
  window reaches from
  \code{mass[currentIndex-halfWindowSize]} to
  \code{mass[currentIndex+halfWindowSize]} (window size is
  \code{2*halfWindowSize+1}).}
}
\value{
  \code{double}, a smoothed intensity vector.
}
\examples{
## load package
library("MALDIquant")

## 9 point moving average
MALDIquant:::.movingAverage(rep(1:5, times=2), halfWindowSize=4)
# 2.778 2.778 2.778 2.778 2.778 3.222 3.222 3.222 3.222 3.222

## load example data
data("fiedler2009subset", package="MALDIquant")

## smooth spectra
s <- smoothIntensity(fiedler2009subset, method="MovingAverage",
                     halfWindowSize=2)
}
\seealso{
  \code{\link[MALDIquant]{smoothIntensity,MassSpectrum-method}},
  \code{\link[MALDIquant]{.savitzkyGolay}},
  \code{\linkS4class{MassSpectrum}}
}
\keyword{internal}

