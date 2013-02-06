\name{imputeMass-methods}
\docType{methods}
\alias{imputeMass}
\alias{imputeMass,MassSpectrum-method}
\title{Imputes missing mass values to a MassSpectrum object.}
\description{
  Most preprocessing steps in mass spectrum analysis are based on indices
  (e.g. \code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}}). This
  method assures equal sized or consistent increasing mass steps between each
  index.\cr
  This is an internal function and hidden by NAMESPACE because it doesn't work
  on Bruker Daltonics' CompassXport 32bit mzXML output.
}
\usage{
\S4method{imputeMass}{MassSpectrum}(object, verbose=FALSE)
}
\arguments{
  \item{object}{\code{\linkS4class{MassSpectrum}} object}
  \item{verbose}{logical. Print verbose messages?}
}
\details{
  Some conversion tools (e.g. Bruker Daltonics' CompassXport) remove
  mass values with intensity zero. This may cause unexpected results in
  index-based preprocessing methods. To overcome this problem
  \code{\link[MALDIquant]{imputeMass,MassSpectrum-method}} imputes
  the missing mass values.
  The imputation algorithm relies on determining whether a equal sized or
  a linear increasing step size is needed and calculates missing mass
  values (with corresponding intensities equal zero).
  Note that the imputed mass values may not be exactly identical to the
  removed ones. However, it is not important to get identical mass
  values (because their intensity is zero),
  only matters that the distance between two observed mass values
  (with corresponding intensities greater as zero) is correct which
  is guaranteed by \code{\link[MALDIquant]{imputeMass,MassSpectrum-method}}.
  Mass values which are trimmed before/after the lowest/highest observed
  mass are not recovered.
}
\value{
  Returns a modified \code{\linkS4class{MassSpectrum}} object with imputed
  mass values if needed. Otherwise the object is returned unchanged.
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{MassSpectrum}}
}
\examples{
## load library
library("MALDIquant")

## create some artificial spectra
mass <- 1:10
intensity <- c(1, 2, 10, 1, 0, 0, 10, 1, 0, 1)

original <- createMassSpectrum(mass=mass, intensity=intensity)
filtered <- createMassSpectrum(mass=mass[intensity!=0],
                               intensity=intensity[intensity!=0])

## compare length == FALSE
length(original) == length(filtered)

## filtered is the shorter spectrum
length(original) > length(filtered)

## run peak detection
pO <- detectPeaks(original, halfWindowSize=2)
pF <- detectPeaks(filtered, halfWindowSize=2)

## a peak in pF is missing caused by an incorrect index distance
length(pO) > length(pF)

## fix it
imputed <- MALDIquant:::imputeMass(filtered, verbose=TRUE)

## now original and imputed have the same length
length(original) == length(imputed)

## and everything is equal
all.equal(intensity(original), intensity(original))
all.equal(mass(original), mass(original))

## test peak detection
pI <- detectPeaks(imputed, halfWindowSize=2)

## all peaks detected
length(pO) == length(pI)

## all peaks are the same
all.equal(intensity(pO), intensity(pI))
all.equal(mass(pO), mass(pO))


## if mass imputation is not needed the spectrum would not changed
a <- createMassSpectrum(mass=1:10, intensity=rep(1, 10))
b <- MALDIquant:::imputeMass(a, verbose=TRUE)
length(a) == length(b)
all.equal(intensity(a), intensity(b))
all.equal(mass(a), mass(b))

}
\keyword{methods}
\keyword{internal}

