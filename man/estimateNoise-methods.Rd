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
  \item{method}{a noise estimation function; see
    \code{\link[MALDIquant]{.estimateNoiseMad}},
    \code{\link[MALDIquant]{.estimateNoiseSuperSmoother}}
  }
  \item{\dots}{arguments to be passed to \code{method}.}
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
\code{\link[MALDIquant]{.estimateNoiseMad}},
\code{\link[MALDIquant]{.estimateNoiseSuperSmoother}},
\code{\link[MALDIquant]{removeBaseline,MassSpectrum-method}}
}
\examples{
## load library
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## choose only the first mass spectrum
s <- fiedler2009subset[[1]]

## transform intensities
s <- transformIntensity(s, sqrt)

## remove baseline
s <- removeBaseline(s)

## plot spectrum
plot(s)

## estimate noise
n <- estimateNoise(s)

## draw noise on the plot
lines(n, col="red")
}
\keyword{methods}

