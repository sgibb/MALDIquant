\name{AbstractMassObject-class}
\Rdversion{1.1}
\docType{class}
\alias{AbstractMassObject-class}
\alias{[,AbstractMassObject,logical,missing-method}
\alias{[,AbstractMassObject,numeric,missing-method}
\alias{[,MassPeaks,logical,missing-method}
\alias{[,MassPeaks,numeric,missing-method}
\alias{as.matrix,AbstractMassObject-method}
\alias{intensity}
\alias{intensity<-}
\alias{intensity,AbstractMassObject-method}
\alias{intensity<-,AbstractMassObject,numeric-method}
\alias{isEmpty}
\alias{isEmpty,AbstractMassObject-method}
\alias{length,AbstractMassObject-method}
\alias{lines,AbstractMassObject-method}
\alias{mass}
\alias{mass<-}
\alias{mass,AbstractMassObject-method}
\alias{mass<-,AbstractMassObject,numeric-method}
\alias{mz}
\alias{mz<-}
\alias{mz,AbstractMassObject-method}
\alias{mz<-,AbstractMassObject,numeric-method}
\alias{metaData}
\alias{metaData<-}
\alias{metaData,AbstractMassObject-method}
\alias{metaData<-,AbstractMassObject-method}
\alias{coordinates}
\alias{coordinates<-}
\alias{coordinates,AbstractMassObject-method}
\alias{coordinates<-,AbstractMassObject,numeric-method}
\alias{coordinates<-,AbstractMassObject,matrix-method}
\alias{coordinates,list-method}
\alias{coordinates<-,list,matrix-method}
\alias{points,AbstractMassObject-method}

\title{Class "AbstractMassObject"}
\description{
\code{\linkS4class{AbstractMassObject}} is an abstract (means pure virtual)
class. It is the parent class of \code{\linkS4class{MassSpectrum}} and
\code{\linkS4class{MassPeaks}}.
It shouldn't create or handle by the user because it is for internal use only.
}
\section{Derived classes}{
\code{\linkS4class{MassPeaks}},
\code{\linkS4class{MassSpectrum}}
}
\section{Slots}{
\describe{
  \item{\code{mass}:}{\code{numeric}, mass or mass-to-charge ratio}
  \item{\code{intensity}:}{\code{numeric}, intensities for measured
    mass-to-charge ratios}
  \item{\code{metaData}:}{\code{list}, some metadata to describe the
    spectrum}
}
}
\section{Methods}{
\describe{
  \item{[}{\code{signature(x = "AbstractMassObject", i = "numeric")}:
    Extracts a range of an \code{\linkS4class{AbstractMassObject}} object
    and returns a new one.}
  \item{as.matrix}{\code{signature(x = "AbstractMassObject")}:
    Converts an \code{\linkS4class{AbstractMassObject}} object to a
    matrix with 2 columns (\code{mass}, \code{intensity}).}
  \item{coordinates}{\code{signature(object = "AbstractMassObject")}:
    Accessor function for coordinates stored in object generated from imaging
    mass spectrometry data.}
  \item{coordinates<-}{\code{signature(object = "AbstractMassObject",
      value = "numeric|matrix")}
    Replacement function for coordinates used in imaging mass spectrometry
    datasets.}
  \item{intensity}{\code{signature(object = "AbstractMassObject")}:
    Accessor function for slot \code{intensity}.}
  \item{intensity<-}{\code{signature(object = "AbstractMassObject",
      value = "numeric")}
    Replacement function for slot \code{intensity}.}
  \item{isEmpty}{\code{signature(object = "AbstractMassObject")}:
    Returns \code{TRUE} if length of \code{intensity} is 0 or all
    \code{intensity} values are 0.}
  \item{length}{\code{signature(x = "AbstractMassObject")}:
    Returns length of slot \code{intensity}.}
  \item{lines}{\code{signature(x = "AbstractMassObject")}:
    Extented function for adding \code{AbstractMassObject} object as a
    line to a specific plot.
    See \code{\link{lines}} for details.}
  \item{mass}{\code{signature(object = "AbstractMassObject")}:
    Accessor function for slot \code{mass}.}
  \item{mass<-}{\code{signature(object = "AbstractMassObject",
      value = "numeric")}
    Replacement function for slot \code{mass}.}
  \item{mz}{\code{signature(object = "AbstractMassObject")}:
    Accessor function for slot \code{mass}.}
  \item{mz<-}{\code{signature(object = "AbstractMassObject",
      value = "numeric")}
    Replacement function for slot \code{mass}.}
  \item{metaData}{\code{signature(object = "AbstractMassObject")}:
    Accessor function for slot \code{metaData}.}
  \item{metaData<-}{\code{signature(object = "AbstractMassObject")}:
    Replacement function for slot \code{metaData}.}
  \item{plot}{\code{signature(x = "AbstractMassObject", y = "missing")}:
    Extented function for plotting an \code{AbstractMassObject} object.
    See \code{\link{plot,AbstractMassObject,missing-method}} for
    details.}
  \item{points}{\code{signature(x = "AbstractMassObject")}:
    Extented function for adding
    \code{\linkS4class{AbstractMassObject}} object as points
    to a specific plot.
    See \code{\link{points}} for details.}
  \item{trim}{\code{signature(object = "AbstractMassObject",
      range = "numeric")}: Trim an \code{AbstractMassObject}
    object. See \code{\link[MALDIquant]{trim,AbstractMassObject,numeric-method}}
    for details.}
  \item{transformIntensity}{\code{signature(object = "AbstractMassObject")}:
    Transforms the intensities of an \code{AbstractMassObject} object.
    See \code{\link[MALDIquant]{transformIntensity,AbstractMassObject-method}}
    for details.}
}
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\linkS4class{MassPeaks}},
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{plot,AbstractMassObject,missing-method}},
\code{\link[MALDIquant]{transformIntensity,AbstractMassObject-method}},
\code{\link[MALDIquant]{trim,AbstractMassObject,numeric-method}}

Website: \url{https://strimmerlab.github.io/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## create example spectrum
s <- createMassSpectrum(mass=1:10, intensity=11:20,
                        metaData=list(name="Example Spectrum"))

## get intensity
intensity(s)

## get mass
mass(s)

## get metaData
metaData(s)

## replace metaData
metaData(s) <- list(name="Spectrum")

## trim spectrum
trim(s, c(2, 9))

## select a range
s[3:6]
}
\keyword{classes}

