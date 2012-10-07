\name{findEmptyMassObjects}
\alias{findEmptyMassObjects}
\alias{removeEmptyMassObjects}
\title{
Finds or removes empty AbstractMassObject objects in a list.
}
\description{
These functions looks for empty \code{\linkS4class{AbstractMassObject}} objects
in a \code{\link{list}}.
}
\usage{
findEmptyMassObjects(l)

removeEmptyMassObjects(l)
}
\arguments{
    \item{l}{
        \code{list}, list of \code{\linkS4class{AbstractMassObject}} where empty
        objects should be found or removed.
    }
}
\value{
\code{findEmptyMassObjects}: Returns a \code{\link{vector}} of indices referring to empty 
\code{\linkS4class{AbstractMassObject}} objects.

\code{removeEmptyMassObjects}: Returns a \code{\link{list}} of 
\code{\linkS4class{AbstractMassObject}} objects but without empty ones.
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\link[MALDIquant]{isEmpty,AbstractMassObject-method}},
\code{\linkS4class{AbstractMassObject}}
}
\examples{
## load library
library("MALDIquant");

## create list
peakList <- list();

## create two MassPeaks objects and add them to the list
peakList[[1]] <- createMassPeaks(mass=1:100, intensity=1:100,
                                 metaData=list(name="example 1"));
peakList[[2]] <- createMassPeaks(mass=1:100, intensity=1:100,
                                 metaData=list(name="example 2"));

## find empty objects (there should not be any one)
findEmptyMassObjects(peakList);

## add an empty MassPeaks object to the list
peakList[[3]] <- createMassPeaks(mass=double(), intensity=double(),
                                 metaData=list(name="empty MassPeaks object"));

## look for empty objects (isEmptyIdx == 3)
(isEmptyIdx <- findEmptyMassObjects(peakList));

## to remove all empty MassObjects from a list
length(peakList); ## 3
peakList <- removeEmptyMassObjects(peakList);
length(peakList); ## 2; WARNING: all indices could changed
}
\keyword{methods}

