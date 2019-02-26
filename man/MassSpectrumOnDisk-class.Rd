\name{MassSpectrumOnDisk-class}
\Rdversion{1.1}
\docType{class}
\alias{MassSpectrumOnDisk}
\alias{MassSpectrumOnDisk-class}
\alias{isRegular}
\alias{isRegular,MassSpectrumOnDisk-method}
\alias{totalIonCurrent}
\alias{totalIonCurrent,MassSpectrumOnDisk-method}

\title{Class "MassSpectrumOnDisk"}
\description{
\code{\linkS4class{MassSpectrumOnDisk}} represents a single spectrum of a MALDI-TOF
mass spectrometry measurement. It provides an easy framework for doing
some preprocessing steps like peak detection, baseline correction and
much more. This class is identical to the \code{\linkS4class{MassSpectrum}} object except that it uses 
the \code{matter} package internally to interface directly with mass spectra on disk 
without loading them into memory. This is handy when dealing with mass spectrometry 
imaging (MSI) data which usually has high memory-footprint. The \code{mass} and \code{intensity} slots 
are \code{matter::matter_vec} objects with only the \code{metaData} slot loaded into memory. 
When used with \code{\href{https://ms-imaging.org/wp/imzml/}{imzML}} MSI files, the spectra can be 
directly attached to the binary \code{ibd} file via \code{MALDIquantForeign::importImzMl; attachOnly = TRUE} without the need to load them into memory. \cr
}
\section{Objects from the Class}{
\code{\link[MALDIquant]{createMassSpectrumOnDisk}}: Creates a
\code{\linkS4class{MassSpectrumOnDisk}} object.
}
\section{Extends}{
Class \code{\linkS4class{AbstractMassObject}}, directly.
}
\section{Methods}{
\describe{
  \item{calibrateIntensity}{\code{signature(x = "MassSpectrumOnDisk")}:
    Calibrates the intensity of a
    \code{\linkS4class{MassSpectrumOnDisk}} object.
    See \code{\link[MALDIquant]{calibrateIntensity,MassSpectrumOnDisk-method}} for
    details.}
  \item{detectPeaks}{\code{signature(x = "MassSpectrumOnDisk")}:
    Look for local maxima and estimate noise to extract peaks out of a
    \code{\linkS4class{MassSpectrumOnDisk}} object.
    See \code{\link[MALDIquant]{detectPeaks,MassSpectrumOnDisk-method}} for
    details.}
  \item{estimateBaseline}{\code{signature(x = "MassSpectrumOnDisk")}:
    Estimates the baseline of a
    \code{\linkS4class{MassSpectrumOnDisk}} object.
    See \code{\link[MALDIquant]{estimateBaseline,MassSpectrumOnDisk-method}} for
    details.}
  \item{estimateNoise}{\code{signature(x = "MassSpectrumOnDisk")}:
    Estimates the noise of a
    \code{\linkS4class{MassSpectrumOnDisk}} object.
    See \code{\link[MALDIquant]{estimateNoise,MassSpectrumOnDisk-method}} for
    details.}
  \item{isRegular}{\code{signature(object = "MassSpectrumOnDisk")}:
    Returns \code{FALSE} if the frequency of mass values with irregular
    intervals is greater than \code{threshold} (because \code{object}
    was measured in \emph{centroid} mode or some \code{intensity}
    values were filtered).}
  \item{removeBaseline}{\code{signature(x = "MassSpectrumOnDisk")}:
    Estimates and removes the baseline of a
    \code{\linkS4class{MassSpectrumOnDisk}} object.
    See \code{\link[MALDIquant]{removeBaseline,MassSpectrumOnDisk-method}} for
    details.}
  \item{smoothIntensity}{\code{signature(object = "MassSpectrumOnDisk")}:
    Smoothes the intensities of an \code{MassSpectrumOnDisk} object.
    See \code{\link[MALDIquant]{smoothIntensity,MassSpectrumOnDisk-method}} for
    details.}
  \item{totalIonCurrent}{\code{signature(object = "MassSpectrumOnDisk")}:
    Accessor function for Total Ion Current (TIC, area under the curve).}
}
}
\author{
Denis Abu Sammour \email{d.abu-sammour@hs-mannheim.de}
}

\references{
\code{"matter"}:\cr
Kylie A. Bemis (2018). matter: A framework for rapid prototyping with binary data on disk. R
  package version 1.8.0. \url{https://github.com/kuwisdelu/matter}.
}

\seealso{
\code{\linkS4class{MassPeaks}},
\code{\linkS4class{MassSpectrum}},
\code{\linkS4class{AbstractMassObject}}

Website: \url{http://strimmerlab.org/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## create a MassSpectrumOnDisk object by default constructor
## This creates two matter::matter_vec objects in the temp
## directory representing mass and intensity values, respectfully. 
s <- createMassSpectrumOnDisk(mass=1:100, intensity=rnorm(100)^2,
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

