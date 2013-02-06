\name{movingAverage}
\alias{movingAverage}
\title{Moving Average}
\description{This function runs a simple 2-side moving average.}
\usage{
  movingAverage(y, halfWindowSize=2)
}
\arguments{
  \item{y}{intensity values}

  \item{halfWindowSize}{half window size. The resulting
  window reaches from
  \code{mass[currentIndex-halfWindowSize]} to
  \code{mass[currentIndex+halfWindowSize]} (window size is
  \code{2*halfWindowSize+1}).}
}
\details{
  The \code{movingAverage} function is defined as:
  \preformatted{
movingAverage <- function(y, halfWindowSize = 2) {
  windowSize <- halfWindowSize * 2 + 1
  y <- filter(x=y, filter=rep(1, windowSize)/windowSize, sides=2)
  return(y)
}
}
}
\value{
  a smoothed intensity vector
}
\examples{
## load library
library("MALDIquant")

## 5 point moving average
movingAverage(rep(1:5, times=2), halfWindowSize=2)
# NA NA  3  3  3  3  3  3 NA NA

## 9 point moving average
movingAverage(rep(1:5, times=2), halfWindowSize=4)
# NA NA NA NA 2.777778 3.222222 NA NA NA NA

## load example data
data("fiedler2009subset", package="MALDIquant")

## smooth spectra
s <- transformIntensity(fiedler2009subset, movingAverage, halfWindowSize=2)
}
\seealso{
  \code{\link{filter}},
  \code{\linkS4class{MassSpectrum}},
  \code{\link[MALDIquant]{transformIntensity,AbstractMassObject-method}}
}

