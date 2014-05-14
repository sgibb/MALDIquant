\name{estimateNoise-methods}
\docType{methods}
\alias{estimateNoise}
\alias{estimateNoise,MassSpectrum-method}
\title{Estimates the noise of a MassSpectrum object.}
\description{
This method estimates the noise of mass spectrometry data
(represented by a \code{\linkS4class{MassSpectrum}} object).\cr
}
\usage{
\S4method{estimateNoise}{MassSpectrum}(object,
  method=c("MAD", "SuperSmoother"),
  \dots)
}
\arguments{
  \item{object}{\code{\linkS4class{MassSpectrum}} object}
  \item{method}{used noise estimation method, one of
    \code{"MAD"} or \code{"SuperSmoother"}.
  }
  \item{\dots}{arguments to be passed to \code{method}.}
}
\details{
  \describe{
    \item{\code{"MAD"}:}{
      This function estimates the noise of mass spectrometry data
      by calculating the median absolute deviation, see also
      \code{\link[stats]{mad}}.
    }
    \item{\code{"SuperSmoother"}:}{
      This function estimates the noise of mass spectrometry data using
      Friedman's Super Smoother. Please refer
      \code{\link[stats]{supsmu}} for details and additional arguments.
    }
  }
}
\value{
Returns a two column matrix (first column: mass, second column: intensity)
of the estimated noise.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}},
\code{\link[stats]{mad}},
\code{\link[stats]{supsmu}}

Website: \url{http://strimmerlab.org/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## choose only the first mass spectrum
s <- fiedler2009subset[[1]]

## transform intensities
s <- transformIntensity(s, method="sqrt")

## remove baseline
s <- removeBaseline(s)

## plot spectrum
plot(s)

## estimate noise
nm <- estimateNoise(s, method="MAD")
nss <- estimateNoise(s, method="SuperSmoother")

## draw noise on the plot
lines(nm, col=2)
lines(nss, col=4)

## draw legend
legend(x="topright", lwd=1, legend=c("MAD", "SuperSmoother"),
       col=c(2, 4))
}
\keyword{methods}

