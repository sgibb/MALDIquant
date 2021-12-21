\name{filterPeaks}
\alias{filterPeaks}
\title{
Removes less frequent peaks.
}
\description{
This function removes infrequently occuring peaks in a list of
\code{\linkS4class{MassPeaks}} objects.
}
\usage{
filterPeaks(l, minFrequency, minNumber, labels, mergeWhitelists=FALSE)
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
    \code{factor}, (one for each \code{\linkS4class{MassPeaks}} object)
    to do groupwise filtering. The \emph{levels} of the factor
    \code{label} define the groups. If not specified a single group is assumed.
  }
  \item{mergeWhitelists}{
    \code{logical}, if \code{FALSE} the filtering criteria are applied
    groupwise. If \code{TRUE} peaks that survive the filtering in one group
    (level of \code{labels}) these peaks are also kept in other groups even if
    their frequencies are below \code{minFrequency}.
  }
}
\details{
For \code{mergeWhitelists=FALSE} the filtering uses a separate peak whitelist
for each group specified by \code{labels}, and is done independently in each
group. For \code{mergeWhitelists=TRUE} the peak whitelists are combined,
which means that peaks that occur frequently in at least one group are also
kept in all other groups.

If both \code{minFrequency} and \code{minNumber} arguments are specified the
more stringent threshold is used.
}
\value{
Returns a \code{\link{list}} of filtered
\code{\linkS4class{MassPeaks}} objects.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\link[MALDIquant]{intensityMatrix}},
\code{\linkS4class{MassPeaks}}

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

## apply different minFrequency arguments to each group
groups <- factor(c("a", "a", "b", "b", "b"), levels=c("a", "b"))
filteredPeaks <- filterPeaks(c(p, p[[3]]), minFrequency=c(1, 2/3), labels=groups)
intensityMatrix(filteredPeaks)
#     1 2  3  4
#[1,] 1 2 NA NA
#[2,] 1 2 NA NA
#[3,] 1 2  3  4
#[4,] 1 2  3  4
#[4,] 1 2  3  4


## demonstrate the use of mergeWhitelists
groups <- factor(c("a", "a", "b", "b"), levels=c("a", "b"))

## default behaviour
filteredPeaks <- filterPeaks(p, minNumber=2, labels=groups)
intensityMatrix(filteredPeaks)
#     1 2  3  4
#[1,] 1 2 NA NA
#[2,] 1 2 NA NA
#[3,] 1 2  3  4
#[4,] 1 2  3  4

## use mergeWhitelists=TRUE to keep peaks of group "a" that match all filtering
## criteria in group "b"
## (please note that mass == 3 is not removed in the second MassPeaks object)
filteredPeaks <- filterPeaks(p, minNumber=2, labels=groups,
                             mergeWhitelists=TRUE)
intensityMatrix(filteredPeaks)
#     1 2  3  4
#[1,] 1 2 NA NA
#[2,] 1 2  3 NA
#[3,] 1 2  3  4
#[4,] 1 2  3  4
}
\keyword{methods}
