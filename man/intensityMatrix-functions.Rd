\name{intensityMatrix}
\alias{intensityMatrix}
\title{
Converts a list of MassPeaks objects into a matrix.
}
\description{
This function converts a \code{\link{list}} of
\code{\linkS4class{MassPeaks}} objects into a \code{\link{matrix}}.
}
\usage{
intensityMatrix(peaks, spectra)
}
\arguments{
  \item{peaks}{
    \code{list}, list of \code{\linkS4class{MassPeaks}} objects.
  }
  \item{spectra}{
    \code{list}, list of \code{\linkS4class{MassSpectrum}} objects. If a peak
    is missing the corresponding intensity value of the spectrum is used. If
    \code{spectra} is missing \code{NA} is used instead.
  }
}
\details{
\code{peaks} have to be binned by \code{\link[MALDIquant]{binPeaks}} before
calling \code{\link[MALDIquant]{intensityMatrix}}.
}
\value{
Returns a \code{\link{matrix}} containing intensities of all
\code{\linkS4class{MassPeaks}} objects of \code{peaks} and interpolated
intensity values for missing peaks if \code{spectra} was given or \code{NA}
otherwise. \cr
The \code{\link{matrix}} has \code{length(peaks)} rows
(one row for each sample) and \code{length(unique(mass))} columns.
There is an additional attribute \code{"mass"} that stores the mass values.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\link[MALDIquant]{binPeaks}},
\code{\linkS4class{MassPeaks}},
\code{\linkS4class{MassSpectrum}}

Website: \url{https://strimmerlab.github.io/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## create example MassPeaks objects
p <- list(createMassPeaks(mass=1:4,
                          intensity=11:14,
                          metaData=list(name="test mass peaks 1")),
          createMassPeaks(mass=2:5,
                          intensity=22:25,
                          metaData=list(name="test mass peaks 2")))

## converts MassPeaks objects into a matrix
intensityMatrix(p)


## realistic example

## load example data
data("fiedler2009subset", package="MALDIquant")

## transform intensities
s <- transformIntensity(fiedler2009subset, method="sqrt")

## smoothing spectrum
s <- smoothIntensity(s, method="MovingAverage")

## remove baseline
s <- removeBaseline(s)

## call peak detection
p <- detectPeaks(s)

## bin peaks
p <- binPeaks(p)

## convert MassPeaks objects into a matrix with missing intensity
## values
intensityMatrix(p)

## convert MassPeaks and MassSpectrum objects into a matrix without
## missing intensity values
intensityMatrix(p, s)
}
\keyword{methods}

