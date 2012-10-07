\name{isMassObject}
\alias{isMassObject}
\alias{isMassSpectrum}
\alias{isMassPeaks}
\title{
Tests for AbstractMassObject objects.
}
\description{
These functions test for an \code{\linkS4class{AbstractMassObject}} object.
}
\usage{
isMassObject(x)

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
argument is an \code{\linkS4class{AbstractMassObject}} object.
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
## load library
library("MALDIquant");

## create a MassPeaks object
peaks <- createMassPeaks(mass=1:100, intensity=1:100,
                         metaData=list(name="example 1"));

## test 
isMassObject(peaks);    # returns TRUE 
isMassPeaks(peaks);     # returns TRUE 
isMassSpectrum(peaks);  # returns FALSE
isMassPeaks(double());  # returns FALSE
}
\keyword{methods}

