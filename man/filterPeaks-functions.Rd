\name{filterPeaks}
\alias{filterPeaks}
\title{
Removes less frequently peaks.
}
\description{
This function removes infrequently peaks from a list of
\code{\linkS4class{MassPeaks}} objects.
}
\usage{
filterPeaks(l, minFrequency, minNumber, labels)
}
\arguments{
  \item{l}{
    \code{list}, list of \code{\linkS4class{MassPeaks}} objects.
  }
  \item{minFrequency}{
    \code{double}, remove all peaks which occur in less than
    \code{minFrequency*length(l)} \code{\linkS4class{MassPeaks}}
    objects. It is a relative threshold.
  }
  \item{minNumber}{
    \code{double}, remove all peaks which occur in less than
    \code{minNumber} \code{\linkS4class{MassPeaks}}
    objects. It is an absolute threshold.
  }
  \item{labels}{
    \code{list}, list of \code{\link{factor}}s (one for each
    \code{\linkS4class{MassPeaks}} object) to do groupwise filtering.
  }
}
\details{
If \code{labels} argument is given all peaks which occur in less than
\code{minFrequency*n} \code{\linkS4class{MassPeaks}} are removed (\code{n} is
the number of \code{\linkS4class{MassPeaks}} objects in each level).

If \code{minFrequency} and \code{minNumber} arguments are given the highest
of \code{minFrequency*n} (\code{n} is the number of
\code{\linkS4class{MassPeaks}}) and \code{minNumber} is chosen.
}
\value{
Returns a \code{\link{list}} of filtered
\code{\linkS4class{MassPeaks}} objects.
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\link[MALDIquant]{intensityMatrix}},
\code{\linkS4class{MassPeaks}}
}
\examples{
## load package
library("MALDIquant")

## create four MassPeaks objects and add them to the list
p <- list(createMassPeaks(mass=1:2, intensity=1:2),
          createMassPeaks(mass=1:3, intensity=1:3),
          createMassPeaks(mass=1:4, intensity=1:4),
          createMassPeaks(mass=1:5, intensity=1:5))

## only keep peaks which occur in all MassPeaks objects
filteredPeaks <- filterPeaks(p, minFrequency=1)

## compare result
intensities <- intensityMatrix(filteredPeaks)

## peaks at mass 3,4,5 are removed
all(dim(intensities) == c(4, 2)) # TRUE
all(intensities[,1] == 1)        # TRUE
all(intensities[,2] == 2)        # TRUE

## only keep peaks which occur in all MassPeaks objects in a group
## (e.g. useful for technical replicates)
groups <- factor(c("a", "a", "b", "b"), levels=c("a", "b"))
filteredPeaks <- filterPeaks(p, minFrequency=1, labels=groups)

## peaks at mass 3 were removed in group "a"
filteredPeaks[groups == "a"]

## peaks at mass 5 were removed in group "b"
filteredPeaks[groups == "b"]

## only keep peaks which occur at least twice in a group
groups <- factor(c("a", "a", "b", "b", "b"), levels=c("a", "b"))
filteredPeaks <- filterPeaks(c(p, p[[3]]), minNumber=2, labels=groups)

## peaks at mass 3 were removed in group "a"
filteredPeaks[groups == "a"]

## peaks at mass 5 were removed in group "b"
filteredPeaks[groups == "b"]

}
\keyword{methods}
