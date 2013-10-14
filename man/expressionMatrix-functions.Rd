\name{expressionMatrix}
\alias{expressionMatrix}
\title{
Converts a list of MassPeaks objects into a matrix.
}
\description{
This function converts a \code{\link{list}} of
\code{\linkS4class{MassPeaks}} objects into a binary \code{\link{matrix}}
(presence/absence) or
(if additionally a \code{\link{list}} of \code{\linkS4class{MassSpectrum}} is
given) into a expression \code{\link{matrix}} without missing values.
}
\usage{
expressionMatrix(peaks, spectra)
}
\arguments{
  \item{peaks}{
    \code{list}, list of \code{\linkS4class{MassPeaks}} objects.
  }
  \item{spectra}{
    \code{list}, list of \code{\linkS4class{MassSpectrum}} objects. If a peak
    is missing the corresponding intensity value of the spectrum is used. If
    \code{spectra} is missing a binary \code{\link{matrix}} (presence/absence)
    is generated.
  }
}
\value{
Returns a \code{\link{matrix}} containing intensities of all
\code{\linkS4class{MassPeaks}} objects of \code{peaks} and interpolated
intensity values for missing peaks (if \code{spectra} was given; in comparison:
\code{\link[MALDIquant]{intensityMatrix}} returns \code{NA} for missing peaks).
If the \code{spectra} argument is missing a presence/absence
\code{\link{matrix}} is generated. \cr
The \code{\link{matrix}} has \code{length(peaks)} rows
(one row for each sample) and \code{length(unique(mass))} columns.
}
\details{
The column names of the returned \code{\link{matrix}} store the mass values.
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\link[MALDIquant]{intensityMatrix}}
}
\examples{
## load package
library("MALDIquant")

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

## convert MassPeaks objects into a presence/absence matrix
expressionMatrix(p)

## convert MassPeaks and MassSpectrum objects into an expression matrix without
## missing intensity values
expressionMatrix(p, s)
}
\keyword{methods}

