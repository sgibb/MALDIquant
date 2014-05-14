\name{smoothIntensity-methods}
\docType{methods}
\alias{smoothIntensity}
\alias{smoothIntensity,MassSpectrum-method}
\alias{smoothIntensity,list-method}
\title{Smoothes intensities of a MassSpectrum object.}
\description{
This method smoothes the intensity values of a
\code{\linkS4class{MassSpectrum}} object.
}
\usage{
\S4method{smoothIntensity}{MassSpectrum}(object,
  method=c("SavitzkyGolay", "MovingAverage"), halfWindowSize,
  \dots)
}
\arguments{
  \item{object}{\code{\linkS4class{AbstractMassObject}} object or a
    \code{list} of \code{\linkS4class{AbstractMassObject}} objects.}
  \item{method}{used smoothing method, one of \code{"SavitzkyGolay"} or
    \code{"MovingAverage"}.}
  \item{halfWindowSize}{half window size. The resulting
    window reaches from \code{mass[currentIndex-halfWindowSize]} to
    \code{mass[currentIndex+halfWindowSize]} (window size is
    \code{2*halfWindowSize+1}).
    The best size differs depending on the
    selected \code{method}.}
  \item{\dots}{arguments to be passed to \code{method}. \code{SavitzkyGolay} has
    an additional \code{polynomialOrder} argument (default: \code{3}) to control
    the order of the filter. Unused for \code{MovingAverage}}
}
\details{
  \code{halfWindowSize}: Depends on the selected \code{method}.
  For the \code{SavitzkyGolay} the \code{halfWindowSize} should be smaller than
  \emph{FWHM} of the peaks (full width at half maximum; please find details in
  Bromba and Ziegler 1981).
  In general the \code{halfWindowSize} for the \code{MovingAverage} has to be
  much smaller than for \code{SavitzkyGolay} to conserve the peak shape.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\references{
A. Savitzky and M. J. Golay. 1964.
Smoothing and differentiation of data by simplified least squares procedures.
Analytical chemistry, 36(8), 1627-1639.

M. U. Bromba and H. Ziegler. 1981.
Application hints for Savitzky-Golay digital smoothing filters.
Analytical Chemistry, 53(11), 1583-1586.
}
\seealso{
\code{\linkS4class{MassSpectrum}}

Website: \url{http://strimmerlab.org/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## smooth spectra
s <- smoothIntensity(fiedler2009subset, method="MovingAverage",
                     halfWindowSize=2)
## or
s <- smoothIntensity(fiedler2009subset, method="SavitzkyGolay",
                     halfWindowSize=10)
}
\keyword{methods}

