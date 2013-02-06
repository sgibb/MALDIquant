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
   fun, na.rm = TRUE,
   \dots)
}
\arguments{
  \item{object}{\code{\linkS4class{AbstractMassObject}} object or a
    \code{list} of \code{\linkS4class{AbstractMassObject}} objects.}
  \item{fun}{used transformation function (\code{\link{sqrt}},
    \code{\link{log10}}, \code{\link{log}} or something else
    would be possible)}
  \item{na.rm}{logical, indicating whether \sQuote{NA} values should be
    removed}
  \item{\dots}{arguments to be passed to \code{fun}}
}
\details{
\code{\link[MALDIquant]{transformIntensity}} is a shortcut for transforming
intensities manually. \cr
\preformatted{

  ## doing things by hand
  newMassSpectrum <- singleSpectrum1
  intensity(newMassSpectrum) <- sqrt(intensity(singleSpectrum1))

  ## the code above could replaced by the following line
  newMassSpectrum <- transformIntensity(singleSpectrum1, fun=sqrt)
}
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{AbstractMassObject}},
\code{\linkS4class{MassSpectrum}}
}
\examples{
## load library
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## choose only the first mass spectrum
s <- fiedler2009subset[[1]]

## transform spectrum
t <- transformIntensity(s, fun=sqrt)

## plot spectrum
par(mfrow=c(2, 1))
plot(s, main="raw spectrum")
plot(t, main="transformed spectrum")
par(mfrow=c(1, 1))
}
\keyword{methods}

