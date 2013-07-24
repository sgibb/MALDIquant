\name{MALDIquant-deprecated}
\alias{MALDIquant-deprecated}
%------ PLEASE: put \alias{.} here for EACH !
\alias{totalIonCurrent,MassPeaks-method}
%------ NOTE: ../R/deprecated.R must be synchronized with this!
\title{Deprecated Functions/Methods in Package \pkg{MALDIquant}}
\description{
  These functions/methods are provided for compatibility with older versions
  \code{\link{MALDIquant}} only, and may be defunct as soon as the next release.
}
\usage{
\S4method{totalIonCurrent}{MassPeaks}(object)
}
\arguments{
   \item{object}{\code{\linkS4class{MassPeaks}} object}
}
\details{
\describe{
  \item{totalIonCurrent}{\code{signature(object = "MassPeaks")}:
    Accessor function for sum of all intensities (slot \code{intensity}).}
}
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{internal}
\keyword{misc}
