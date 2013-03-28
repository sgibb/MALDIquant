\name{savitzkyGolay}
\alias{savitzkyGolay}
\title{Savitzky-Golay Smoothing Filter}
\description{This function applies a Savitzky-Golay filter.}
\usage{
  savitzkyGolay(y, halfWindowSize=10, polynomialOrder=3)
}
\arguments{
  \item{y}{intensity values}

  \item{halfWindowSize}{half window size. The resulting
  window reaches from
  \code{mass[currentIndex-halfWindowSize]} to
  \code{mass[currentIndex+halfWindowSize]} (window size is
  \code{2*halfWindowSize+1}).}
  \item{polynomialOrder}{polynomial order of Savitzky-Golay Filter.}
}
\details{
  \code{halfWindowSize}: Should be lower than \emph{FWHM} of peaks (full width
  at half maximum; please find details in Bromba and Ziegler 1981).
}
\value{
  \code{double}, a smoothed intensity vector.
}
\references{
A. Savitzky and M. J. Golay. 1964.
Smoothing and differentiation of data by simplified least squares procedures.
Analytical chemistry, 36(8), 1627-1639.

M. U. Bromba and H. Ziegler. 1981.
Application hints for Savitzky-Golay digital smoothing filters.
Analytical Chemistry, 53(11), 1583-1586.
}
\examples{
## load library
library("MALDIquant")

## 5 point savitzky-golay filter
savitzkyGolay(rep(1:5, times=2), halfWindowSize=2)
# 1 2 3 4.43 3.71 2.29 1.57 3 4 5

## load example data
data("fiedler2009subset", package="MALDIquant")

## smooth spectra
s <- transformIntensity(fiedler2009subset, savitzkyGolay, halfWindowSize=12)
}
\seealso{
  \code{\link{movingAverage}},
  \code{\linkS4class{MassSpectrum}},
  \code{\link[MALDIquant]{transformIntensity,AbstractMassObject-method}}
}

