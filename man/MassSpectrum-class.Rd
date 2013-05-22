\name{MassSpectrum-class}
\Rdversion{1.1}
\docType{class}
\alias{MassSpectrum}
\alias{MassSpectrum-class}
\alias{isRegular}
\alias{isRegular,MassSpectrum-method}

\title{Class "MassSpectrum"}
\description{
\code{\linkS4class{MassSpectrum}} represents a single spectrum of a MALDI-TOF
mass spectrometry measurement. It provides an easy framework for doing
some preprocessing steps like peak detection, baseline correction and
much more. \cr
}
\section{Objects from the Class}{
\code{\link[MALDIquant]{createMassSpectrum}}: Creates a
\code{\linkS4class{MassSpectrum}} object.
}
\section{Extends}{
Class \code{\linkS4class{AbstractMassObject}}, directly.
}
\section{Methods}{
\describe{
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
%  \item{imputeMass}{\code{signature(x = "MassSpectrum")}:
%    Impute missing mass values to a
%    \code{\linkS4class{MassSpectrum}} object.
%    See \code{\link[MALDIquant]{imputeMass,MassSpectrum-method}} for
%    details.}
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
}
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\link[MALDIquant]{createMassSpectrum}},
\code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}},
\code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}},
\code{\link[MALDIquant]{estimateNoise,MassSpectrum-method}},
\code{\link[MALDIquant]{imputeMass,MassSpectrum-method}},
\code{\link[MALDIquant]{removeBaseline,MassSpectrum-method}},
\code{\linkS4class{AbstractMassObject}}
}
\examples{
## load library
library("MALDIquant")

## create a MassSpectrum object by default constructor
s <- createMassSpectrum(mass=1:100, intensity=rnorm(100)^2,
                        metaData=list(name="example"))

## show some details
s

## plot spectrum
plot(s)

## modify intensity and metaData
intensity(s)[1:50] <- 0
metaData(s) <- list(name="modified example")

## plot again
plot(s)
}
\keyword{classes}

