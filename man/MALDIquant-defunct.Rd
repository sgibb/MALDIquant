\name{MALDIquant-defunct}
\alias{MALDIquant-defunct}
\title{Removed Functions/Methods in Package \pkg{MALDIquant}}
\description{
  These functions/methods are removed.
}
\details{
\describe{
  \item{totalIonCurrent}{\code{signature(object = "MassPeaks")}: removed.}
  \item{totalIonCurrent<-}{\code{signature(object = "AbstractMassObject",
      value = "numeric")}:
    use \code{\link[MALDIquant]{calibrateIntensity}} instead.}
  \item{calibrate}{\code{signature(x = "matrix")}:
    use \code{\link[MALDIquant]{calibrateIntensity}} instead.}
  \item{standardizeTotalIonCurrent}{\code{signature(object = "list")}:
    use \code{\link[MALDIquant]{calibrateIntensity}} instead.}
  \item{ltrim}{
    \code{signature(object = "AbstractMassObject", minMass = "numeric")}:
    use \code{\link[MALDIquant]{trim}} instead.}
  \item{rtrim}{
    use \code{\link[MALDIquant]{trim}} instead.}
  \item{mergeMassSpectra}{use \code{\link[MALDIquant]{averageMassSpectra}}
    instead.}
  \item{movingAverage}{use \code{\link[MALDIquant]{smoothIntensity}}
    instead.}
  \item{savitzkyGolay}{use \code{\link[MALDIquant]{smoothIntensity}}
    instead.}
  \item{isMassObject}{removed.}
  \item{isMassObjectList}{removed.}
  \item{intensityMatrix}{
    does not support \code{\linkS4class{MassSpectrum}} objects anymore.}
}
}
\keyword{internal}
\keyword{misc}
