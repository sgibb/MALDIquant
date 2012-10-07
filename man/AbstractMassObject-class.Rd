\name{AbstractMassObject-class}
\Rdversion{1.1}
\docType{class}
\alias{AbstractMassObject-class}
\alias{[,AbstractMassObject,logical,missing-method}
\alias{[,AbstractMassObject,numeric,missing-method}
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
\alias{metaData}
\alias{metaData<-}
\alias{metaData,AbstractMassObject-method}
\alias{metaData<-,AbstractMassObject-method}
\alias{points,AbstractMassObject-method}
\alias{totalIonCurrent}
\alias{totalIonCurrent<-}
\alias{totalIonCurrent,AbstractMassObject-method}
\alias{totalIonCurrent<-,AbstractMassObject,numeric-method}

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
    \item{\code{.cache}:}{\code{environment}, internal use only}
    \item{\code{mass}:}{\code{vector}, mass or mass-to-charge ratio}
    \item{\code{intensity}:}{\code{vector}, intensities for measured
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
    \item{intensity}{\code{signature(object = "AbstractMassObject")}:
        Accessor function for slot \code{intensity}.}
    \item{intensity<-}{\code{signature(object = "AbstractMassObject",
            value = "numeric")}
        Replacement function for slot \code{intensity}.}
    \item{isEmpty}{\code{signature(object = "AbstractMassObject")}:
        Returns \code{TRUE} if length of \code{intensity} is 0.}
    \item{length}{\code{signature(x = "AbstractMassObject")}:
        Returns length of slot \code{intensity}.}
    \item{lines}{\code{signature(x = "AbstractMassObject")}:
        Extented function for adding \code{AbstractMassObject} object as a 
        line to a specific plot.
        See \code{\link{lines}} for details.}
    \item{ltrim}{\code{signature(object = "AbstractMassObject", minMass =
        "numeric")}: Trim the left side of an \code{AbstractMassObject} object.
        See \code{\link{ltrim,AbstractMassObject,numeric-method}} for details.}
    \item{mass}{\code{signature(object = "AbstractMassObject")}:
        Accessor function for slot \code{mass}.}
    \item{mass<-}{\code{signature(object = "AbstractMassObject",
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
    \item{rtrim}{\code{signature(object = "AbstractMassObject", maxMass =
        "numeric")}: Trim the right side of an \code{AbstractMassObject} object.
        See \code{\link{rtrim,AbstractMassObject,numeric-method}} for details.}
    \item{totalIonCurrent}{\code{signature(object = "AbstractMassObject")}:
        Accessor function for sum of all intensities (slot \code{intensity}).}
    \item{totalIonCurrent<-}{\code{signature(object = "AbstractMassObject",
            value = "numeric")}
        Set sum of slot \code{intensity} to \code{value}.}
    \item{trim}{\code{signature(object = "AbstractMassObject", minMass =
        "numeric", maxMass = "numeric")}: Trim an \code{AbstractMassObject}
        object. See \code{\link{trim,AbstractMassObject,numeric,numeric-method}}
        for details.}
    \item{transformIntensity}{\code{signature(object = "AbstractMassObject")}:
        Transforms the intensities of an \code{AbstractMassObject} object. 
        See \code{\link{transformIntensity,AbstractMassObject-method}} for 
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
\code{\linkS4class{MassPeaks}},
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{plot,AbstractMassObject,missing-method}},
\code{\link[MALDIquant]{transformIntensity,AbstractMassObject-method}},
\code{\link[MALDIquant]{trim,AbstractMassObject,numeric,numeric-method}}
}
\examples{
## load library
library("MALDIquant");

## create example spectrum
s <- createMassSpectrum(mass=1:10, intensity=11:20, 
                        metaData=list(name="Example Spectrum"));

## get intensity
intensity(s);

## get mass
mass(s);

## get metaData
metaData(s);

## replace metaData
metaData(s) <- list(name="Spectrum");

## get TIC 
totalIonCurrent(s);

## trim spectrum
trim(s, 2, 9);

## select a range
s[3:6];
}
\keyword{classes}

