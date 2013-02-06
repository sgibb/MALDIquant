\name{determineWarpingFunctions}
\alias{determineWarpingFunctions}
\title{
Determine warping functions of MassPeaks objects.
}
\description{
This function determines a warping function for a list of
\code{\linkS4class{AbstractMassObject}} objects
(warping is also known as \emph{phase correction/spectra alignment}).
}
\usage{
determineWarpingFunctions(l, reference, tolerance=0.002,
                          warpingFunction=.warpingFunctionLowess,
                          plot=FALSE, plotInteractive=FALSE, \dots)
}
\arguments{
  \item{l}{
    \code{list}, list of \code{\linkS4class{MassPeaks}} objects.
  }
  \item{reference}{
    \code{\linkS4class{MassPeaks}}, reference object to which the samples
    (\code{l}) should be aligned. If missing
    \code{\link[MALDIquant]{referencePeaks}} is used.
  }
  \item{tolerance}{
    \code{double}, maximal deviation of a peak position (mass) to be
    considered as identical.
  }
  \item{warpingFunction}{
    \code{function}, general warping function which is used to align
    sample and reference \code{\linkS4class{MassPeaks}} objects.
  }
  \item{plot}{
    \code{logical}, if \code{TRUE} a warping plot is drawn for each sample.
  }
  \item{plotInteractive}{
    \code{logical}, if \code{FALSE} a non-interactive device (e.g.
    \code{pdf}) is used for warping plots.
  }
  \item{\dots}{
    arguments to be passed to \code{warpingFunction}
  }
}
\details{
  \code{warpingFunction}: \code{determineWarpingFunctions} estimates a warping
  function to overcome the difference between mass in \code{reference} and in
  the current sample. To calculate the differences each reference peak would
  match with the highest sample peak in the nearer neighborhood
  (defined by \code{mass of reference peak*tolerance}). \cr
  MALDIquant uses a \code{\link{lowess}}-based warping
  function as default one. It could easily replaced by an own warping
  function. At least three arguments are needed:
  \itemize{
    \item{\code{x}: \code{double}, the original (not warped) mass of the
      sample \code{\linkS4class{MassPeaks}} object.
    }
    \item{\code{d}: \code{double}, the difference of the sample and the
      reference mass.
    }
    \item{\code{\dots}: further arguments (e.g. \code{iter} for
      \code{\link{lowess}})
    }
  }
  A correct warping function has to return a single individual warping function
  which depends on a single argument \code{x}. \cr
  \preformatted{
## e.g. warping function to fit a 2nd order polynomial
quadraticWarp <- function(x, d, ...) {
  l <- lm(y ~ x1+x2, data=list(x1=x, x2=x*x, y=d), ...)
  co <- coef(l)
  return(function(x) { return (co[1]+x*co[2]+x*x*co[3]) })
}
  }
  \code{plotInteractive}: If \code{plot} is \code{TRUE} a lot of output is
  created (each sample in \code{l} gets its own plot).
  That's why an non-interactive devices is recommended:
  \preformatted{
## create a device
pdf()
## calculate warping functions
w <- determineWarpingFunctions(p, plot=TRUE)
## close device
dev.off()
  }
}
\value{
Returns a \code{list} of individual warping functions.
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\link[MALDIquant]{referencePeaks}},
\code{\link[MALDIquant]{warpMassPeaks}},
\code{\link[MALDIquant]{warpMassSpectra}},
\code{\linkS4class{MassPeaks}}
}
\examples{
## load library
library("MALDIquant")

## create a reference MassPeaks object
r <- createMassPeaks(mass=1:5, intensity=1:5)

## create test samples
p <- list(createMassPeaks(mass=((1:5)*1.01), intensity=1:5),
          createMassPeaks(mass=((1:5)*0.99), intensity=1:5))

## create a simple (in other cases useless ) warping function
simpleWarp <- function(x, d, ...) { return(function(x){return(d)}) }

## create an interactive device with 2 rows
par(mfrow=c(2, 1))
## calculate warping function (using simpleWarp as basic warping function)
## and show warping plot
w <- determineWarpingFunctions(p, tolerance=0.02, warpingFunction=simpleWarp,
                               plot=TRUE, plotInteractive=TRUE)
par(mfrow=c(1, 1))

## w contains the individual warping functions
warpedPeaks <- warpMassPeaks(p, w)

## compare results
all(mass(r) == mass(warpedPeaks[[1]])) # TRUE
all(mass(r) == mass(warpedPeaks[[2]])) # TRUE



## realistic example

## load example data
data("fiedler2009subset", package="MALDIquant")

## running typical workflow

## transform intensities
t<- transformIntensity(fiedler2009subset, fun=sqrt)

## smoothing function
movingAvg <- function(y) {
    return(filter(y, rep(1, 5)/5, sides=2))
}

## smooth spectra
s <- transformIntensity(t, fun=movingAvg)

## baseline correction
b <- removeBaseline(s)

## detect peaks
peaks <- detectPeaks(b)

## create an interactive device with 2 rows
par(mfrow=c(2, 1))
## calculate warping functions (using lowess based basic function [default])
w <- determineWarpingFunctions(peaks, plot=TRUE, plotInteractive=TRUE)
par(mfrow=c(1, 1))
}
\keyword{methods}

