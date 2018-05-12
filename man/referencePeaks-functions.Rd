\name{referencePeaks}
\alias{referencePeaks}
\title{
Creates a reference \code{\linkS4class{MassPeaks}} object.
}
\description{
This function creates a reference \code{\linkS4class{MassPeaks}} object (also
called \emph{Anchor Peaks}) from a list of \code{\linkS4class{MassPeaks}}
objects.\cr
Generally it is a combination of \code{\link[MALDIquant]{binPeaks}} and
\code{\link[MALDIquant]{filterPeaks}}
}
\usage{
referencePeaks(l, method=c("strict", "relaxed"), minFrequency=0.9,
               tolerance=0.002)
}
\arguments{
  \item{l}{
    \code{list}, list of \code{\linkS4class{MassPeaks}} objects.
  }
  \item{method}{
    bin creation rule (see \code{\link[MALDIquant]{binPeaks}}).
  }
  \item{minFrequency}{
    \code{double}, remove all peaks which occur in less than
    \code{minFrequency*length(l)} \code{\linkS4class{MassPeaks}}
    objects.
  }
  \item{tolerance}{
    \code{double}, maximal relative deviation of a peak position (mass) to be
    considered as identical. Must be multiplied by \code{10^-6} for ppm,
    e.g. use \code{tolerance=5e-6} for 5 ppm.
  }
}
\value{
Returns a new \code{\linkS4class{MassPeaks}} objects. \cr
The \code{\link[MALDIquant]{intensity}} slot of the returned
\code{\linkS4class{MassPeaks}} represents the frequency of this mass position
in all samples.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\link[MALDIquant]{binPeaks}},
\code{\link[MALDIquant]{filterPeaks}},
\code{\linkS4class{MassPeaks}}

Website: \url{http://strimmerlab.org/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## create four MassPeaks objects and add them to the list
p<- list(createMassPeaks(mass=1:2, intensity=1:2),
         createMassPeaks(mass=1:3, intensity=1:3),
         createMassPeaks(mass=1:4, intensity=1:4),
         createMassPeaks(mass=1:5, intensity=1:5))

## only use peaks which occur in all MassPeaks objects as reference peaks
refPeaks <- referencePeaks(p, minFrequency=1)

mass(refPeaks)      # 1:2
intensity(refPeaks) # c(1, 1)
}
\keyword{methods}

