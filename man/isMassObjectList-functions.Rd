\name{isMassSpectrumList}
\alias{isMassSpectrumList}
\alias{isMassPeaksList}
\title{
Tests a list of MassSpectrum or MassPeaks objects.
}
\description{
These functions test a \code{\link{list}} whether containing
\code{\linkS4class{MassSpectrum}} or \code{\linkS4class{MassSpectrum}}
objects.
}
\usage{
isMassSpectrumList(x)

isMassPeaksList(x)
}
\arguments{
  \item{x}{
    object to be tested.
  }
}
\value{
Returns \code{\link{TRUE}} or \code{\link{FALSE}} depending on whether its
argument is a \code{\link{list}} of \code{\linkS4class{MassSpectrum}}
or \code{\linkS4class{MassPeaks}} objects.
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

## create list
p <- list()

## test list
isMassPeaksList(p) # returns FALSE

## create two MassPeaks objects and add them to the list
p <- createMassPeaks(mass=1:100, intensity=1:100,
                     metaData=list(name="example 1"))
p <- createMassPeaks(mass=1:100, intensity=1:100,
                     metaData=list(name="example 2"))

## test list
isMassPeaksList(p)    # returns TRUE
isMassSpectrumList(p) # returns FALSE
}
\keyword{methods}

