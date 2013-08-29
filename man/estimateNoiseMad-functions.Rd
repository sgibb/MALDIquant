\name{.estimateNoiseMad}
\alias{.estimateNoiseMad}
\title{Estimates the noise by MAD.}
\description{
This function estimates the noise of mass spectrometry data
by calculating the median absolute deviation.
It is hidden by \code{NAMESPACE} because it is a private function and
\code{\link[MALDIquant]{estimateNoise,MassSpectrum-method}} should be used
instead.
}
\usage{
.estimateNoiseMad(x, y)
}
\arguments{
  \item{x}{vector of x values}
  \item{y}{vector of y values}
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
\code{\link[MALDIquant]{estimateNoise,MassSpectrum-method}}
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
n <- estimateNoise(s, method="MAD")

## draw noise on the plot
lines(n, col="red")
}
\keyword{internal}

