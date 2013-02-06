\name{intensityMatrix}
\alias{intensityMatrix}
\title{
Converts a list of AbstractMassObject objects into a matrix.
}
\description{
This function converts a \code{\link{list}} of
\code{\linkS4class{AbstractMassObject}} objects into a \code{\link{matrix}}.
}
\usage{
intensityMatrix(l)
}
\arguments{
  \item{l}{
    \code{list}, list of \code{\linkS4class{AbstractMassObject}} objects.
  }
}
\value{
Returns a \code{\link{matrix}} containing intensities of all
\code{\linkS4class{AbstractMassObject}} objects of \code{l}.
The \code{\link{matrix}} has \code{length(l)} rows
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
\code{\linkS4class{AbstractMassObject}}
}
\examples{
## load library
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
}
\keyword{methods}

