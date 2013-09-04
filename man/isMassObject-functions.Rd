\name{isMassSpectrum}
\alias{isMassSpectrum}
\alias{isMassPeaks}
\title{
Tests for MassSpectrum or MassPeaks object.
}
\description{
These functions test for a \code{\linkS4class{MassSpectrum}} or
\code{\linkS4class{MassPeaks}} object.
}
\usage{
isMassSpectrum(x)

isMassPeaks(x)
}
\arguments{
  \item{x}{
    object to be tested.
  }
}
\value{
Returns \code{\link{TRUE}} or \code{\link{FALSE}} depending on whether its
argument is an \code{\linkS4class{MassSpectrum}} or
\code{\linkS4class{MassPeaks}} object.
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{MassPeaks}},
\code{\linkS4class{MassSpectrum}},
\code{\linkS4class{AbstractMassObject}}
}
\examples{
## load package
library("MALDIquant")

## create a MassPeaks object
peaks <- createMassPeaks(mass=1:100, intensity=1:100,
                         metaData=list(name="example 1"))

## test
isMassPeaks(peaks)      # returns TRUE
isMassSpectrum(peaks)   # returns FALSE
isMassPeaks(double())   # returns FALSE
}
\keyword{methods}

