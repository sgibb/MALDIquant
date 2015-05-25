\name{MALDIquant-deprecated}
\alias{MALDIquant-deprecated}
%------ PLEASE: put \alias{.} here for EACH !
%------ NOTE: ../R/deprecated.R must be synchronized with this!
\alias{plotImsSlice}
\title{Deprecated Functions/Methods in Package \pkg{MALDIquant}}
\description{
  These functions/methods are provided for compatibility with older versions
  \code{\link{MALDIquant}} only, and may be defunct as soon as the next release.
}
\details{
\describe{
  % since MALDIquant x.y.7
  %\item{function}{explanation}
  % since MALDIquant 1.11.12
  \item{\code{plotImsSlice}}{
    This function allows to plot a slice of an Imaging Mass Spectrometry dataset
    represented by a \code{list} of \code{\linkS4class{AbstractMassObject}}
    objects. \cr
    Use \code{\link[MALDIquant]{plotMsiSlice}} instead.
  }
}
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{internal}
\keyword{misc}
