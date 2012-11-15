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
\value{
  a smoothed intensity vector
}
\examples{
## load library
library("MALDIquant");

movingAverage(rep(1:5, times=2), halfWindowSize=2);
# NA NA  3  3  3  3  3  3 NA NA

## load example data
data("fiedler2009subset", package="MALDIquant");

## smooth spectra
s <- transformIntensity(fiedler2009subset, movingAverage, halfWindowSize=2);
}
\seealso{
  \code{\link{filter}},
  \code{\linkS4class{MassSpectrum}},
  \code{\link[MALDIquant]{transformIntensity,AbstractMassObject-method}}
}

