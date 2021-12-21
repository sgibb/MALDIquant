\name{mergeMassPeaks}
\alias{mergeMassPeaks}
\title{
Merges \code{\linkS4class{MassPeaks}} objects.
}
\description{
This function merges \code{\linkS4class{MassPeaks}} objects.
}
\usage{
mergeMassPeaks(l, labels, method=c("mean", "median", "sum"), ignore.na=TRUE,
               \dots)
}
\arguments{
  \item{l}{
    \code{list}, list of \code{\linkS4class{MassPeaks}} objects.
  }
  \item{labels}{
    \code{list}, list of \code{\link{factor}}s (one for each
    \code{\linkS4class{MassPeaks}} object) to do groupwise merging.
  }
  \item{method}{used merge method.}
  \item{ignore.na}{Should \code{NA} (positions where a peak is missing)
    ignored (\code{ignore.na=TRUE}) or treated as zero
    (\code{ignore.na=FALSE})?}
  \item{\dots}{arguments to be passed to underlying functions (currently only
    \code{mc.cores} is supported).}
}
\value{
Returns a single (no \code{labels} given) or a \code{\link{list}}
(\code{labels} given) of merged \code{\linkS4class{MassPeaks}} objects.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\linkS4class{MassPeaks}},
\code{\link[MALDIquant]{averageMassSpectra}}

Website: \url{https://strimmerlab.github.io/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## create four MassPeaks objects and add them to the list
p <- list(createMassPeaks(mass=1:2, intensity=1:2),
          createMassPeaks(mass=1:3, intensity=1:3),
          createMassPeaks(mass=1:4, intensity=1:4),
          createMassPeaks(mass=1:5, intensity=1:5))

## merge all four MassPeaks objects into a single new one
## by sum their intensities
## (no labels, returns only one new MassPeaks object)
mergedPeaks <- mergeMassPeaks(p, method="sum")

## only merge MassPeaks objects in a group
## (two different labels, returns a list of two new MassPeaks objects)
groups <- factor(c("a", "a", "b", "b"), levels=c("a", "b"))
mergedPeaks <- mergeMassPeaks(p, labels=groups, method="mean")

## the same, but treat NA as zero
mergedPeaks <- mergeMassPeaks(p, labels=groups, method="mean", ignore.na=FALSE)
}
\keyword{methods}

