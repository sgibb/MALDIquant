\name{alignSpectra}
\alias{alignSpectra}
\title{
Align MassSpectrum objects.
}
\description{
This function aligns a list of \code{\linkS4class{MassSpectrum}} objects
(spectra alignment is also known as \emph{warping/phase correction}). \cr
}
\usage{
alignSpectra(spectra, halfWindowSize=20, noiseMethod="MAD", SNR=2,
  reference, tolerance=0.002, warpingMethod="lowess",
  allowNoMatches=FALSE, emptyNoMatches=FALSE, \dots)
}
\arguments{
  \item{spectra}{
    \code{list}, list of \code{\linkS4class{MassSpectrum}} objects.
  }
  \item{halfWindowSize}{\code{numeric}, half window size; see
    \code{\link[MALDIquant]{detectPeaks}}.
  }
  \item{noiseMethod}{a noise estimation method; see
    \code{\link[MALDIquant]{detectPeaks}}.
  }
  \item{SNR}{single numeric value. \code{SNR} is an abbreviation for
    \emph{s}ignal-to-\emph{n}oise-\emph{r}atio; see
    \code{\link[MALDIquant]{detectPeaks}}.
  }
  \item{reference}{
    \code{\linkS4class{MassPeaks}}, reference object to which the samples
    (\code{l}) should be aligned. If missing
    \code{\link[MALDIquant]{referencePeaks}} is used; see
    \code{\link[MALDIquant]{determineWarpingFunctions}}.
  }
  \item{tolerance}{
    \code{double}, maximal relative deviation of a peak position (mass) to be
    considered as identical. Must be multiplied by \code{10^-6} for ppm,
    e.g. use \code{tolerance=5e-6} for 5 ppm; see
    \code{\link[MALDIquant]{determineWarpingFunctions}}.
  }
  \item{warpingMethod}{
    used basic warping function; see
    \code{\link[MALDIquant]{determineWarpingFunctions}}.
  }
  \item{allowNoMatches}{
    \code{logical}, don't throw an error if an \code{\linkS4class{MassPeaks}}
    object could not match to the \code{reference}; see
    \code{\link[MALDIquant]{determineWarpingFunctions}}.
  }
  \item{emptyNoMatches}{
    \code{logical}, if \code{TRUE} (default: \code{FALSE})
    the intensity values of
    \code{\linkS4class{MassSpectrum}} or
    \code{\linkS4class{MassPeaks}} objects with missing (\code{NA})
    warping functions are set to zero; see
    \code{\link[MALDIquant]{warpMassSpectra}}.
  }
  \item{\dots}{arguments to be passed to
    \code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}}.}
}
\details{
  \code{alignSpectra} is a wrapper function around
  \code{\link[MALDIquant]{detectPeaks}},
  \code{\link[MALDIquant]{determineWarpingFunctions}} and
  \code{\link[MALDIquant]{warpMassSpectra}}. Please call these functions
  manually if you need finer control (e.g. plotting of warping functions).
}
\value{
Returns a \code{list} of aligned \code{\linkS4class{MassSpectrum}} objects.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\link[MALDIquant]{detectPeaks}},
\code{\link[MALDIquant]{determineWarpingFunctions}},
\code{\link[MALDIquant]{referencePeaks}},
\code{\link[MALDIquant]{warpMassSpectra}},
\code{\linkS4class{MassSpectrum}}

\code{demo("warping")}

Website: \url{https://strimmerlab.github.io/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## running typical workflow

## transform intensities
spectra <- transformIntensity(fiedler2009subset, method="sqrt")

## smooth spectra
spectra <- smoothIntensity(spectra, method="MovingAverage")

## baseline correction
spectra <- removeBaseline(spectra)

## align spectra
spectra <- alignSpectra(spectra)
}
\keyword{methods}

