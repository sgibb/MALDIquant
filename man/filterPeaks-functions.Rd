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
filterPeaks(l, minFrequency, labels)
}
\arguments{
    \item{l}{
        \code{list}, list of \code{\linkS4class{MassPeaks}} objects.
    }
    \item{minFrequency}{
        \code{double}, remove all peaks which occur in less than
        \code{minFrequency*length(l)} \code{\linkS4class{MassPeaks}}
        objects.
    }
    \item{labels}{
        \code{list}, list of \code{\link{factor}}s (one for each
        \code{\linkS4class{MassPeaks}} object) to do groupwise filtering.
    }
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
## load library
library("MALDIquant");

## create four MassPeaks objects and add them to the list
p <- list(createMassPeaks(mass=1:2, intensity=1:2),
          createMassPeaks(mass=1:3, intensity=1:3),
          createMassPeaks(mass=1:4, intensity=1:4),
          createMassPeaks(mass=1:5, intensity=1:5))

## only keep peaks which occur in all MassPeaks objects
filteredPeaks <- filterPeaks(p, minFrequency=1);

## compare result
intensities <- intensityMatrix(filteredPeaks);

## peaks at mass 3,4,5 are removed
all(dim(intensities) == c(4, 2)); # TRUE
all(intensities[,1] == 1);        # TRUE
all(intensities[,2] == 2);        # TRUE

## only keep peaks which occur in all MassPeaks in a group
## (e.g. useful for technical replicates)
groups <- factor(c("a", "a", "b", "b"), levels=c("a", "b"));
filteredPeaks <- filterPeaks(p, minFrequency=1, labels=groups);

## peaks at mass 3 removed in group "a"
filteredPeaks[groups == "a"];

## peaks at mass 5 removed in group "b"
filteredPeaks[groups == "b"];

}
\keyword{methods}
