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
filterPeaks(l, minFrequency, minNumber, labels, mode="group")
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
  \item{mode}{
    \code{character}, whitelist mode, could be \code{"group"} (default),
    \code{"all"} or \code{"none"}. See Details section for more information.
  }
}
\details{
If \code{labels} argument is given all peaks which occur in less than
\code{minFrequency*n} \code{\linkS4class{MassPeaks}} are removed (\code{n} is
the number of \code{\linkS4class{MassPeaks}} objects in each level).

If \code{minFrequency} and \code{minNumber} arguments are given the highest
of \code{minFrequency*n} (\code{n} is the number of
\code{\linkS4class{MassPeaks}}) and \code{minNumber} is chosen.

\code{mode} controls the whitelist behaviour. For each level (group) of
\code{labels} a separate whitelist is generated. If \code{mode="group"} the
whitelist is only applied to the current level (group). In contrast
\code{mode="all"} results in a whitelist that is applied to all levels
(groups). \cr
\code{mode="none"} does not create any whitelist and all peaks in this level
(group) will be removed if no other level has \code{mode="all"}. For
clarification see the examples, too.
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


## demonstrate the use of different whitelist modes
groups <- factor(c("a", "a", "b", "b"), levels=c("a", "b"))

## default behaviour
filteredPeaks <- filterPeaks(p, minNumber=2, labels=groups,
                             mode=c("group", "group"))
intensityMatrix(filteredPeaks)
#     1 2  3  4
#[1,] 1 2 NA NA
#[2,] 1 2 NA NA
#[3,] 1 2  3  4
#[4,] 1 2  3  4

## use "all" for group "b"
## (whitelist b is applied to group a, too;
##  please note that mass == 3 is not removed in the second MassPeaks)
filteredPeaks <- filterPeaks(p, minNumber=2, labels=groups,
                             mode=c("group", "all"))
intensityMatrix(filteredPeaks)
#     1 2  3  4
#[1,] 1 2 NA NA
#[2,] 1 2  3 NA
#[3,] 1 2  3  4
#[4,] 1 2  3  4

## use "none" for group "b"
## (no whitelist is generated for group "b" and all peaks in "b" are removed)
filteredPeaks <- filterPeaks(p, minNumber=2, labels=groups,
                             mode=c("group", "none"))
intensityMatrix(filteredPeaks)
#      1  2
#[1,]  1  2
#[2,]  1  2
#[3,] NA NA
#[4,] NA NA

## use "all" and "none"
## (no whitelist is generated for group "b" but peaks in "b" are not
##  completely because the "a" whitelist have been applied on "b")
filteredPeaks <- filterPeaks(p, minNumber=2, labels=groups,
                             mode=c("all", "none"))
intensityMatrix(filteredPeaks)
#     1 2
#[1,] 1 2
#[2,] 1 2
#[3,] 1 2
#[4,] 1 2
}
\keyword{methods}
