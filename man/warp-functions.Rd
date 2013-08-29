\name{warpMassSpectra}
\alias{warpMassSpectra}
\alias{warpMassPeaks}
\title{
Run warping functions on AbstractMassObject objects.
}
\description{
These functions run warping functions on
\code{\linkS4class{AbstractMassObject}} objects
(warping is also known as \emph{phase correction}).
}
\usage{
warpMassPeaks(l, w)

warpMassSpectra(l, w)
}
\arguments{
  \item{l}{
    \code{list}, list of \code{\linkS4class{MassPeaks}} or
    \code{\linkS4class{MassSpectrum}} objects.
  }
  \item{w}{
    a \code{list} of warping functions determined by
    \code{\link[MALDIquant]{determineWarpingFunctions}}. Has to be of the
    same length as \code{l}.
  }
}
\details{
The warping function \code{w} is called in the following way:
\deqn{newMass = oldMass + w(oldMass)}
}
\value{
Returns a \code{list} of warped \code{\linkS4class{MassPeaks}} or
\code{\linkS4class{MassSpectrum}} objects.
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\link[MALDIquant]{determineWarpingFunctions}},
\code{\linkS4class{MassPeaks}},
\code{\linkS4class{MassSpectrum}}
}
\examples{
## load package
library("MALDIquant")

## create a MassPeaks object
p <- createMassPeaks(mass=1:5, intensity=1:5)

## stupid warping function for demonstration
## (please use determineWarpingFunctions in real life applications)
simpleWarp <- function(x) { return(1) }

## run warping function
w <- warpMassPeaks(list(p), list(simpleWarp))[[1]]

## compare results
all(mass(w) == mass(p)+1) # TRUE
}
\keyword{methods}

