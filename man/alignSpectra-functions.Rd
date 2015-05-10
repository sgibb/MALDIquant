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
  reference, tolerance=0.002, warpingMethod="lowess", \dots)
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
    \code{double}, maximal deviation of a peak position (mass) to be
    considered as identical; see
    \code{\link[MALDIquant]{determineWarpingFunctions}}.
  }
  \item{warpingMethod}{
    used basic warping function; see
    \code{\link[MALDIquant]{determineWarpingFunctions}}.
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

Website: \url{http://strimmerlab.org/software/maldiquant/}
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

