\name{AbstractMassSpectrum-class}
\Rdversion{1.1}
\docType{class}
\alias{AbstractMassSpectrum}
\alias{MassSpectrum-class}


\title{Class "AbstractMassSpectrum"}
\description{
\code{\linkS4class{AbstractMassSpectrum}} is an abstract (means pure virtual)
class representing a single spectrum of a MALDI-TOF
mass spectrometry measurement. It is the parent class of \code{\linkS4class{MassSpectrum}} and
\code{\linkS4class{MassSpectrumOnDisk}}. It shouldn't be created or handled by the user because it is for internal use only.. \cr
}

\section{Extends}{
Class \code{\linkS4class{AbstractMassObject}}, directly.
}

\section{Derived classes}{
\code{\linkS4class{MassSpectrum}},
\code{\linkS4class{MassSpectrumOnDisk}}
}
\section{Methods}{
\describe{
  \item{calibrateIntensity}{\code{signature(x = "MassSpectrum")}:
    Calibrates the intensity of a
    \code{\linkS4class{MassSpectrum}} object.
    See \code{\link[MALDIquant]{calibrateIntensity,MassSpectrum-method}} for
    details.}
  \item{detectPeaks}{\code{signature(x = "MassSpectrum")}:
    Look for local maxima and estimate noise to extract peaks out of a
    \code{\linkS4class{MassSpectrum}} object.
    See \code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}} for
    details.}
  \item{estimateBaseline}{\code{signature(x = "MassSpectrum")}:
    Estimates the baseline of a
    \code{\linkS4class{MassSpectrum}} object.
    See \code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}} for
    details.}
  \item{estimateNoise}{\code{signature(x = "MassSpectrum")}:
    Estimates the noise of a
    \code{\linkS4class{MassSpectrum}} object.
    See \code{\link[MALDIquant]{estimateNoise,MassSpectrum-method}} for
    details.}
  \item{isRegular}{\code{signature(object = "MassSpectrum")}:
    Returns \code{FALSE} if the frequency of mass values with irregular
    intervals is greater than \code{threshold} (because \code{object}
    was measured in \emph{centroid} mode or some \code{intensity}
    values were filtered).}
  \item{removeBaseline}{\code{signature(x = "MassSpectrum")}:
    Estimates and removes the baseline of a
    \code{\linkS4class{MassSpectrum}} object.
    See \code{\link[MALDIquant]{removeBaseline,MassSpectrum-method}} for
    details.}
  \item{smoothIntensity}{\code{signature(object = "MassSpectrum")}:
    Smoothes the intensities of an \code{MassSpectrum} object.
    See \code{\link[MALDIquant]{smoothIntensity,MassSpectrum-method}} for
    details.}
  \item{totalIonCurrent}{\code{signature(object = "MassSpectrum")}:
    Accessor function for Total Ion Current (TIC, area under the curve).}
}
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\link[MALDIquant]{createMassSpectrum}},
\code{\link[MALDIquant]{calibrateIntensity,MassSpectrum-method}},
\code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}},
\code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}},
\code{\link[MALDIquant]{estimateNoise,MassSpectrum-method}},
\code{\link[MALDIquant]{removeBaseline,MassSpectrum-method}},
\code{\link[MALDIquant]{smoothIntensity,MassSpectrum-method}},
\code{\linkS4class{AbstractMassObject}}

Website: \url{http://strimmerlab.org/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## create a MassSpectrum object by default constructor
s <- createMassSpectrum(mass=1:100, intensity=rnorm(100)^2,
                        metaData=list(name="example"))

## show some details
s

## plot spectrum
plot(s)

## get TIC
totalIonCurrent(s)

## modify intensity and metaData
intensity(s)[1:50] <- 0
metaData(s) <- list(name="modified example")

## plot again
plot(s)
}
\keyword{classes}

