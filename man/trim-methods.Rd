\name{trim-methods}
\docType{methods}
\alias{ltrim}
\alias{rtrim}
\alias{trim}
\alias{ltrim,AbstractMassObject,numeric-method}
\alias{rtrim,AbstractMassObject,numeric-method}
\alias{trim,AbstractMassObject,numeric,numeric-method}
\alias{trim,AbstractMassObject,missing,numeric-method}
\alias{trim,AbstractMassObject,numeric,missing-method}
\alias{ltrim,list,numeric-method}
\alias{rtrim,list,numeric-method}
\alias{trim,list,numeric,numeric-method}
\alias{trim,list,missing,numeric-method}
\alias{trim,list,numeric,missing-method}
\title{Trim an AbstractMassObject object.}
\description{
These methods trim an \code{\linkS4class{AbstractMassObject}} object on left
side (\code{ltrim}), right side (\code{rtrim}), or both sides (\code{trim}). \cr
This is useful if some mass ranges should be excluded from further analysis.
}
\usage{
\S4method{ltrim}{AbstractMassObject,numeric}(object, minMass)
\S4method{ltrim}{list,numeric}(object, minMass)
\S4method{rtrim}{AbstractMassObject,numeric}(object, maxMass)
\S4method{rtrim}{list,numeric}(object, maxMass)
\S4method{trim}{AbstractMassObject,numeric,numeric}(object, minMass, maxMass)
\S4method{trim}{list,numeric,numeric}(object, minMass, maxMass)
}
\arguments{
  \item{object}{\code{\linkS4class{AbstractMassObject}} object or a
    \code{list} of \code{\linkS4class{AbstractMassObject}} objects.}
  \item{minMass}{\code{numeric}, minimal mass which should included.}
  \item{maxMass}{\code{numeric}, maximal mass which should included.}
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
## load library
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## select only one spectrum
s <- fiedler2009subset[[1]]

## remove all mass lower 3000
ltrim(s, 3000)

## remove all mass higher 8000
rtrim(s, 8000)

## remove all mass lower 3000 and higher 8000
trim(s, 3000, 8000)
}
\keyword{methods}

