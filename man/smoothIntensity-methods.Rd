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
  \item{method}{used smoothing method;
    for \code{method="SavitzkyGolay"} see
    \code{\link[MALDIquant]{.savitzkyGolay}},
    for \code{method="MovingAverage"} see
    \code{\link[MALDIquant]{.movingAverage}}.}
  \item{halfWindowSize}{half window size. The best size differs depending on the
    selected \code{method}; see \code{\link[MALDIquant]{.savitzkyGolay}} and
    \code{\link[MALDIquant]{.movingAverage}} for details.}
  \item{\dots}{arguments to be passed to
    \code{\link[MALDIquant]{.savitzkyGolay}} or
    \code{\link[MALDIquant]{.movingAverage}}.}
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\link[MALDIquant]{.savitzkyGolay}},
\code{\link[MALDIquant]{.movingAverage}},
\code{\linkS4class{MassSpectrum}}
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

