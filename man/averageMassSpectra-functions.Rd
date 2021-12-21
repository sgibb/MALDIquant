\name{averageMassSpectra}
\alias{averageMassSpectra}
\title{
Averages \code{\linkS4class{MassSpectrum}} objects.
}
\description{
This function averages \code{\linkS4class{MassSpectrum}} objects.
}
\usage{
averageMassSpectra(l, labels, method=c("mean", "median", "sum"), \dots)
}
\arguments{
  \item{l}{
    \code{list}, list of \code{\linkS4class{MassSpectrum}} objects.
  }
  \item{labels}{
    \code{list}, list of \code{\link{factor}}s (one for each
    \code{\linkS4class{MassSpectrum}} object) to do groupwise averaging.
  }
  \item{method}{used aggregation function.}
  \item{\dots}{arguments to be passed to underlying functions (currently only
    \code{mc.cores} is supported).}
}
\details{
The mass of the averaged \code{\linkS4class{MassSpectrum}} object
will be the mass of the first non-empty
\code{\linkS4class{MassSpectrum}} object (of each group).
}
\value{
Returns a single (no \code{labels} given) or a \code{\link{list}}
(\code{labels} given) of averaged \code{\linkS4class{MassSpectrum}} objects.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{mergeMassPeaks}}

Website: \url{https://strimmerlab.github.io/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## create four MassSpectrum objects and add them to a list
s <- list(createMassSpectrum(mass=1:5, intensity=1:5),
          createMassSpectrum(mass=1:5, intensity=1:5),
          createMassSpectrum(mass=1:5, intensity=6:10),
          createMassSpectrum(mass=1:5, intensity=6:10))

## average all four MassSpectrum objects into a single new one
## by sum their intensities
## (no labels, returns only one new MassSpectrum object)
summedSpectra <- averageMassSpectra(s, method="sum")

## only average MassSpectrum objects in a group
## (e.g. useful for technical replicates)
## (two different labels, returns a list of two new MassPeaks objects)
groups <- factor(c("a", "a", "b", "b"), levels=c("a", "b"))
averagedSpectra <- averageMassSpectra(s, labels=groups, method="mean")
}
\keyword{methods}

