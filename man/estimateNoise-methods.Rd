\name{estimateNoise-methods}
\docType{methods}
\alias{estimateNoise}
\alias{estimateNoise,MassSpectrum-method}
\title{Estimates the noise of a MassSpectrum object.}
\description{
This method estimates the noise of mass spectrometry data
(represented by a \code{\linkS4class{MassSpectrum}} object) by calculating the
median absolute deviation.
}
\usage{
\S4method{estimateNoise}{MassSpectrum}(object)
}
\arguments{
    \item{object}{\code{\linkS4class{MassSpectrum}} object}
}
\value{
    Returns a single numeric value.
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}}
}
\examples{
## load library
library("MALDIquant");

## load example data
data("fiedler2009subset", package="MALDIquant");

## choose only the first mass spectrum
s <- fiedler2009subset[[1]];

## transform intensities
s <- transformIntensity(s, sqrt);

## remove baseline
s <- removeBaseline(s);

## plot spectrum
plot(s);

## estimate noise 
n <- estimateNoise(s);

## draw noise on the plot
abline(h=n, col="red");
}
\keyword{methods}

