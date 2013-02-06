\name{mergeMassSpectra}
\alias{mergeMassSpectra}
\alias{mergeMassPeaks}
\title{
Merges \code{\linkS4class{MassPeaks}} or \code{\linkS4class{MassSpectrum}}
objects.
}
\description{
These functions merge \code{\linkS4class{MassPeaks}} or
\code{\linkS4class{MassSpectrum}} objects by a user defined function.
}
\usage{
mergeMassPeaks(l, labels, fun=mean, \dots)

mergeMassSpectra(l, labels, fun=mean, \dots)
}
\arguments{
  \item{l}{
    \code{list}, list of \code{\linkS4class{MassPeaks}} or
    \code{\linkS4class{MassSpectrum}} objects.
  }
  \item{labels}{
    \code{list}, list of \code{\link{factor}}s (one for each
    \code{\linkS4class{MassPeaks}} object) to do groupwise merging.
  }
  \item{fun}{used merge function (\code{\link{mean}},
    \code{\link{median}}, \code{\link{sum}} or something else
    would be possible).
  }
  \item{\dots}{arguments to be passed to \code{fun}}
}
\value{
Returns a single (no \code{labels} given) or a \code{\link{list}}
(\code{labels} given) of merged \code{\linkS4class{MassPeaks}} or
\code{\linkS4class{MassSpectrum}} objects.
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{MassPeaks}},
\code{\linkS4class{MassSpectrum}}
}
\examples{
## load library
library("MALDIquant")

## create four MassPeaks objects and add them to the list
p <- list(createMassPeaks(mass=1:2, intensity=1:2),
          createMassPeaks(mass=1:3, intensity=1:3),
          createMassPeaks(mass=1:4, intensity=1:4),
          createMassPeaks(mass=1:5, intensity=1:5))

## merge all four MassPeaks objects into a single new one
## by sum their intensities
## (no labels, returns only one new MassPeaks object)
mergedPeaks <- mergeMassPeaks(p, fun=sum)

## compare result
mass(mergedPeaks)      # 1:5
intensity(mergedPeaks) # c(4, 8, 9, 8, 5)

## only merge MassPeaks objects in a group
## (e.g. useful for technical replicates)
## (two different labels, returns a list of two new MassPeaks objects)
groups <- factor(c("a", "a", "b", "b"), levels=c("a", "b"))
mergedPeaks <- mergeMassPeaks(p, labels=groups, fun=sum)

## compare result
## group "a":
mass(mergedPeaks[[1]])      # 1:3
intensity(mergedPeaks[[1]]) # c(2, 4, 3)

## group "b":
mass(mergedPeaks[[2]])      # 1:5
intensity(mergedPeaks[[2]]) # c(2, 4, 6, 8, 5)
}
\keyword{methods}

