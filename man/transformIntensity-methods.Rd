\name{transformIntensity-methods}
\docType{methods}
\alias{transformIntensity}
\alias{transformIntensity,AbstractMassObject-method}
\alias{transformIntensity,list-method}
\title{Transforms intensities of an AbstractMassObject object.}
\description{
This method performs a transformation (e.g. sqrt-transformation) on the
intensities of an \code{\linkS4class{AbstractMassObject}} object.
}
\usage{
\S4method{transformIntensity}{AbstractMassObject}(object,
  method=c("sqrt", "log", "log2", "log10"))
\S4method{transformIntensity}{list}(object, \dots)
}
\arguments{
  \item{object}{\code{\linkS4class{AbstractMassObject}} object or a
    \code{list} of \code{\linkS4class{AbstractMassObject}} objects.}
  \item{method}{used transformation method.}
  \item{\dots}{arguments to be passed to underlying functions. If
    \code{object} is a \code{list} \code{mc.cores} is also supported.}
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\linkS4class{AbstractMassObject}},
\code{\linkS4class{MassSpectrum}}

Website: \url{http://strimmerlab.org/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## choose only the first mass spectrum
s <- fiedler2009subset[[1]]

## transform spectrum
t <- transformIntensity(s, method="sqrt")

## plot spectrum
par(mfrow=c(2, 1))
plot(s, main="raw spectrum")
plot(t, main="transformed spectrum")
par(mfrow=c(1, 1))
}
\keyword{methods}

