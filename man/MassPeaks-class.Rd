\name{MassPeaks-class}
\Rdversion{1.1}
\docType{class}
\alias{MassPeaks}
\alias{MassPeaks-class}
\alias{snr}
\alias{snr,MassPeaks-method}

\title{Class "MassPeaks"}
\description{
\code{\linkS4class{MassPeaks}} represents extracted peaks of a single
spectrum of a MALDI-TOF mass spectrometry measurement.
}
\section{Objects from the Class}{
\code{\link[MALDIquant]{createMassPeaks}}: Creates a
\code{\linkS4class{MassPeaks}} object.
}
\section{Extends}{
Class \code{\linkS4class{AbstractMassObject}}, directly.
}
\section{Slots}{
\describe{
  \item{\code{snr}:}{\code{vector}, \emph{s}ignal-to-\emph{n}oise
    \emph{r}atio}
}
}
\section{Methods}{
\describe{
  \item{labelPeaks}{\code{signature(x = "MassPeaks")}:
    Draws peak labels to plot.
    See \code{\link{labelPeaks,MassPeaks-method}} for
    details.}
  \item{monoisotopicPeaks}{\code{signature(x = "MassPeaks")}:
    Finds monoisotopic peaks in peak lists.
    See \code{\link{monoisotopicPeaks,MassPeaks-method}} for
    details.}
}
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\link[MALDIquant]{createMassPeaks}},
\code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}},
\code{\link[MALDIquant]{labelPeaks,MassPeaks-method}},
\code{\linkS4class{AbstractMassObject}}

Website: \url{https://www.strimmerlab.org/software/maldiquant/}
}
%\examples{
%## load package
%library("MALDIquant")
%}
\keyword{classes}

