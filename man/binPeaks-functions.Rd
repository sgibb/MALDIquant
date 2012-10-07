\name{binPeaks}
\alias{binPeaks}
\title{
Align Peaks into discrete bins.
}
\description{
This function looks for similar peaks (mass) across
\code{\linkS4class{MassPeaks}} objects and equalizes their mass.
}
\usage{
binPeaks(l, tolerance=0.002)
}
\arguments{
    \item{l}{
        \code{list}, list of \code{\linkS4class{MassPeaks}} objects.
    }
    \item{tolerance}{
        \code{double}, maximal deviation of a peak position (mass) to be
        considered as identical.
    } 
}
\details{
The algorithm is based on the following workflow:
\enumerate{
    \item{Put all mass in a sorted vector.}
    \item{Calculate differences between each neighbor.}
    \item{Divide the mass vector at the largest gap (largest difference) and form
        a left and a right bin.}
    \item{Rerun step 3 for the left and/or the right bin if they don't fulfill
        the two criteria:
        \itemize{
            \item{The bin doesn't contain two or more peaks of the same sample.}
            \item{All peaks in a bin are near to the mean 
                (\code{abs(mass-meanMass)/meanMass < tolerance}).}
        }
    }
}
}
\value{
Returns a \code{\link{list}} of mass adjusted
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

## create two MassPeaks objects
p <- list(createMassPeaks(mass=seq(100, 500, 100),
                          intensity=1:5),
          createMassPeaks(mass=c(seq(100.2, 300.2, 100), 395),
                          intensity=1:4));

## only keep peaks which occur in all MassPeaks objects
binnedPeaks <- binPeaks(p, tolerance=0.002);

## compare result
iM1 <- intensityMatrix(p);
iM2 <- intensityMatrix(binnedPeaks);

all(dim(iM1) == c(2, 9)); # TRUE
all(dim(iM2) == c(2, 6)); # TRUE

show(iM2);

## increase tolerance
binnedPeaks <- binPeaks(p, tolerance=0.1);

iM3 <- intensityMatrix(binnedPeaks);

all(dim(iM3) == c(2, 5)); # TRUE

show(iM3);
}
\keyword{methods}

