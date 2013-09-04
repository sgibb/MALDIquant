\name{calibrateIntensity-methods}
\docType{methods}
\alias{calibrateIntensity}
\alias{calibrateIntensity,MassSpectrum-method}
\alias{calibrateIntensity,list-method}
\title{Calibrates intensities of a MassSpectrum object.}
\description{
This function calibrates (normalize) intensities of
\code{\linkS4class{MassSpectrum}} objects.
}

\usage{
\S4method{calibrateIntensity}{MassSpectrum}(object,
  method=c("TIC", "PQN", "median"), \dots)
}
\arguments{
  \item{object}{\code{\linkS4class{MassSpectrum}} object or a \code{list} of
    \code{\linkS4class{MassSpectrum}} objects.}
  \item{method}{the calibration method to be used. This should be one of
    \code{"TIC"}, \code{"PQN"} or \code{"median"}.
    See \sQuote{Details} section.}
  \item{\dots}{arguments to be passed to other functions (unneeded until now).}
}
\details{
  A number of different calibration methods are provided:
  \describe{
    \item{\code{"TIC"}:}{The TIC (\emph{T}otal \emph{I}on \emph{C}urrent) of a
      \code{\linkS4class{MassSpectrum}} object is set to one.}
    \item{\code{"PQN"}:}{The PQN (\emph{P}robabilistic \emph{Q}uotient
      \emph{N}ormalization) is described in \emph{Dieterle et al 2006}.
      \code{calibrateIntensity} uses the following algorithm:
      \enumerate{
        \item{Calibrate all spectra using the \code{"TIC"} calibration.}
        \item{Calculate a median reference spectrum.}
        \item{Calculate the quotients of all intensities of the spectra with
          those of the reference spectrum.}
        \item{Calculate the median of these quotients for each spectrum.}
        \item{Divide all intensities of each spectrum by its median of quotients.}
      }
    }
    \item{\code{"Median"}:}{The median of intensities of a
      \code{\linkS4class{MassSpectrum}} object is set to one.}
  }
}
\value{
Returns a modified \code{\linkS4class{MassSpectrum}} object with calibrated
intensities.
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}

F. Dieterle, A. Ross, G. Schlotterbeck, and Hans Senn. 2006.
Probabilistic quotient normalization as robust method to account for
dilution of complex biological mixtures. Application in 1H NMR metabonomics.
Analytical Chemistry 78(13): 4281-4290.
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{MassSpectrum}}
}
\examples{
## load package
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## baseline correction
b <- removeBaseline(fiedler2009subset)

## calibrate intensity values
calibrateIntensity(b, method="TIC")
}
\keyword{methods}
