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
}
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\link[MALDIquant]{createMassPeaks}},
\code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}},
\code{\link[MALDIquant]{labelPeaks,MassPeaks-method}},
\code{\linkS4class{AbstractMassObject}}
}
%\examples{
%## load package
%library("MALDIquant")
%}
\keyword{classes}

