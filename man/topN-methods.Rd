\name{topN-methods}
\docType{methods}
\alias{topN}
\alias{topN,MassPeaks-method}
\alias{topN,list-method}
\title{Find most intense peaks in a MassPeaks object.}
\description{
This method selects the most intense peaks of a
\code{\linkS4class{MassPeaks}} object.
}
\usage{
\S4method{topN}{MassPeaks}(object, n, \dots)
\S4method{topN}{list}(object, n, \dots)
}
\arguments{
  \item{object}{\code{\linkS4class{MassPeaks}} object or a
    \code{list} of \code{\linkS4class{MassPeaks}} objects.}
  \item{n}{\code{integer}, length 1, the \code{n} highest peaks would be
    selected. Please see the note section, too.}
  \item{\dots}{arguments to be passed to underlying functions (currently only
    \code{mc.cores} is supported).}
}
\note{
It is no warranty that the resulting \code{\linkS4class{MassPeaks}} has the same
length as \code{n}. If the object contains two peaks with the same intensity
that are higher than all the other peaks \code{topN(x, n=1)} will return an
object that contains both peaks.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\linkS4class{MassPeaks}},
\code{\link[MALDIquant]{labelPeaks,MassPeaks-method}},
\code{rank}

Website: \url{http://strimmerlab.org/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## create a MassPeaks object from scratch
p <- createMassPeaks(mass=1:20, intensity=c(1:11, 10:2),
                     metaData=list(name="example"))

## plot peaks
plot(p)

## label 5 highest peaks
labelPeaks(topN(p, n=5), col="red")

## n=3, length resulting MassPeaks object == 3
intensity(topN(p, n=3))
# [1] 10 11 10

## n=4, length resulting MassPeaks object == 5
intensity(topN(p, n=4))
# [1]  9 10 11 10  9
}
\keyword{methods}

