\name{MALDIquant-deprecated}
\alias{MALDIquant-deprecated}
%------ PLEASE: put \alias{.} here for EACH !
% since MALDIquant 1.7.4
\alias{totalIonCurrent,MassPeaks-method}
% since MALDIquant 1.7.6
\alias{calibrate}
\alias{calibrate,matrix-method}
\alias{standardizeTotalIonCurrent}
\alias{standardizeTotalIonCurrent,list-method}
\alias{totalIonCurrent<-}
\alias{totalIonCurrent<-,AbstractMassObject,numeric-method}
% since MALDIquant 1.7.10
\alias{ltrim}
\alias{rtrim}
\alias{ltrim,AbstractMassObject,numeric-method}
\alias{rtrim,AbstractMassObject,numeric-method}
\alias{ltrim,list,numeric-method}
\alias{rtrim,list,numeric-method}
% since MALDIquant 1.7.12
\alias{mergeMassSpectra}
\alias{movingAverage}
\alias{savitzkyGolay}
% since MALDIquant 1.7.13
\alias{isMassObject}
\alias{isMassObjectList}
%------ NOTE: ../R/deprecated.R must be synchronized with this!
\title{Deprecated Functions/Methods in Package \pkg{MALDIquant}}
\description{
  These functions/methods are provided for compatibility with older versions
  \code{\link{MALDIquant}} only, and may be defunct as soon as the next release.
}
\details{
\describe{
  \item{totalIonCurrent}{\code{signature(object = "MassPeaks")}:
    Accessor function for sum of all intensities (slot \code{intensity}).}
  \item{totalIonCurrent<-}{\code{signature(object = "AbstractMassObject",
      value = "numeric")}
    Set sum of slot \code{intensity} to \code{value}.}
  \item{calibrate}{\code{signature(x = "matrix")}:
    This function calibrates intensities of an intensity matrix created by
    \code{\link[MALDIquant]{intensityMatrix}}.}
  \item{standardizeTotalIonCurrent}{\code{signature(object = "list")}:
    This method sets the TIC (\emph{T}otal \emph{I}on \emph{C}urrent) of a list
    of \code{\linkS4class{AbstractMassObject}} objects to one.}
  \item{ltrim}{
    \code{signature(object = "AbstractMassObject", minMass = "numeric")}:
    This method trims an \code{\linkS4class{AbstractMassObject}} on the left.}
  \item{rtrim}{
    \code{signature(object = "AbstractMassObject", maxMass = "numeric")}:
    This method trims an \code{\linkS4class{AbstractMassObject}} on the right.}
  \item{mergeMassSpectra}{use \code{\link[MALDIquant]{averageMassSpectra}}
    instead.}
  \item{movingAverage}{use \code{\link[MALDIquant]{smoothIntensity}}
    instead.}
  \item{savitzkyGolay}{use \code{\link[MALDIquant]{smoothIntensity}}
    instead.}
  \item{isMassObject}{deprecated.}
  \item{isMassObjectList}{deprecated.}
}
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{internal}
\keyword{misc}
