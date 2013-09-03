\name{trim-methods}
\docType{methods}
\alias{trim}
\alias{trim,AbstractMassObject,numeric-method}
\alias{trim,list,numeric-method}
\alias{trim,list,missing-method}
\title{Trim an AbstractMassObject object.}
\description{
These method trims an \code{\linkS4class{AbstractMassObject}} object.
This is useful if some mass ranges should be excluded from further analysis.
}
\usage{
\S4method{trim}{AbstractMassObject,numeric}(object, range)
\S4method{trim}{list,numeric}(object, range)
\S4method{trim}{list,missing}(object)
}
\arguments{
  \item{object}{\code{\linkS4class{AbstractMassObject}} object or a
    \code{list} of \code{\linkS4class{AbstractMassObject}} objects.}
  \item{range}{\code{numeric}, limits of trimming (left/minimal mass,
    right/maximal mass). If missing it is automatically determined (largest
    overlapping mass range) for a \code{list} of
    \code{\linkS4class{AbstractMassObject}}.}
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{AbstractMassObject}},
\code{\linkS4class{MassPeaks}},
\code{\linkS4class{MassSpectrum}}
}
\examples{
## load package
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## select only one spectrum
s <- fiedler2009subset[[1]]

## remove all mass lower 3000
trim(s, range=c(3000, Inf))

## remove all mass higher 8000
trim(s, range=c(0, 8000))

## remove all mass lower 3000 and higher 8000
trim(s, range=c(3000, 8000))

## choose largest overlapping mass range for all spectra
trim(fiedler2009subset)
}
\keyword{methods}

