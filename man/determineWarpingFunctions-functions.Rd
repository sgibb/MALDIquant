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
                          method=c("lowess", "linear", "quadratic", "cubic"),
                          allowNoMatches=FALSE,
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
    \code{double}, maximal relative deviation of a peak position (mass) to be
    considered as identical. Must be multiplied by \code{10^-6} for ppm,
    e.g. use \code{tolerance=5e-6} for 5 ppm.
  }
  \item{method}{
    used basic warping function.
  }
  \item{allowNoMatches}{
    \code{logical}, don't throw an error if an \code{\linkS4class{MassPeaks}}
    object could not match to the \code{reference}.
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
  \code{allowNoMatches}: If \code{allowNoMatches} is \code{TRUE} a warning instead
  of an error is thrown if an \code{\linkS4class{MassPeaks}}
  object could not match to the \code{reference}. The returned list of warping
  functions will contain \code{NA} for this object (same index in the list).
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
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\link[MALDIquant]{referencePeaks}},
\code{\link[MALDIquant]{warpMassPeaks}},
\code{\link[MALDIquant]{warpMassSpectra}},
\code{\linkS4class{MassPeaks}}

\code{demo("warping")}

Website: \url{http://strimmerlab.org/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## create a reference MassPeaks object
r <- createMassPeaks(mass=1:5, intensity=1:5)

## create test samples
p <- list(createMassPeaks(mass=((1:5)*1.01), intensity=1:5),
          createMassPeaks(mass=((1:5)*0.99), intensity=1:5))

## create an interactive device with 2 rows
par(mfrow=c(2, 1))
## calculate warping function
## (using a linear function as basic warping function)
## and show warping plot
w <- determineWarpingFunctions(p, tolerance=0.02, method="linear",
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

## use only four spectra of the subset
spectra <- fiedler2009subset[1:4]

## transform intensities
spectra <- transformIntensity(spectra, method="sqrt")

## smooth spectra
spectra <- smoothIntensity(spectra, method="MovingAverage")

## baseline correction
spectra <- removeBaseline(spectra)

## detect peaks
peaks <- detectPeaks(spectra)

## create an interactive device with 2 rows
par(mfrow=c(4, 1))
## calculate warping functions (using LOWESS based basic function [default])
w <- determineWarpingFunctions(peaks, plot=TRUE, plotInteractive=TRUE)
par(mfrow=c(1, 1))



## realistic example with user defined reference/calibration peaks

## use the workflow above for fiedler2009subset

## create reference peaks
refPeaks <- createMassPeaks(mass=c(1207, 1264, 1351, 1466, 1616, 2769, 2932,
                                   3191, 3262, 4091, 4209, 5904, 7762, 9285),
                            intensity=rep(1, 14))

## create an interactive device with 2 rows
par(mfrow=c(4, 1))
## calculate warping functions (using a quadratic function as basic function)
w <- determineWarpingFunctions(peaks, reference=refPeaks, method="quadratic",
                               plot=TRUE, plotInteractive=TRUE)
par(mfrow=c(1, 1))
}
\keyword{methods}
