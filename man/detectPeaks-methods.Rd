\name{detectPeaks-methods}
\docType{methods}
\alias{detectPeaks}
\alias{detectPeaks,MassSpectrum-method}
\alias{detectPeaks,list-method}
\title{Detects peaks in a MassSpectrum object.}
\description{
This method looks for peaks in mass spectrometry data
(represented by a \code{\linkS4class{MassSpectrum}} object).\cr
A peak is a local maximum above a user defined noise threshold.
}
\usage{
\S4method{detectPeaks}{MassSpectrum}(object,
  halfWindowSize=20, method=c("MAD", "SuperSmoother"), SNR=2,
  \dots)
\S4method{detectPeaks}{list}(object, \dots)
}
\arguments{
  \item{object}{\code{\linkS4class{MassSpectrum}} object or a \code{list} of
    \code{\linkS4class{MassSpectrum}} objects.}
  \item{halfWindowSize}{\code{numeric}, half window size. \cr
    The resulting window reaches from \code{mass[currentIndex-halfWindowSize]}
    to \code{mass[currentIndex+halfWindowSize]}. A local maximum have to be
    the highest one in the given window to be recognized as peak.}
  \item{method}{a noise estimation function; see
    \code{\link[MALDIquant]{estimateNoise,MassSpectrum-method}}.
  }
  \item{SNR}{single numeric value. \code{SNR} is an abbreviation for
    \emph{s}ignal-to-\emph{n}oise-\emph{r}atio. A local maximum has to
    be higher than \code{SNR*noise} to be recognize as peak.}
  \item{\dots}{arguments to be passed to
    \code{\link[MALDIquant]{estimateNoise,MassSpectrum-method}}. If
    \code{object} is a \code{list} \code{mc.cores} is also supported.}
}
\value{
Returns a \code{\linkS4class{MassPeaks}} object.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\linkS4class{MassPeaks}},
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{estimateNoise,MassSpectrum-method}}

\code{demo("peaks")}

Website: \url{https://www.strimmerlab.org/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## choose only the first mass spectrum
s <- fiedler2009subset[[1]]

## transform intensities
s <- transformIntensity(s, method="sqrt")

## smoothing spectrum
s <- smoothIntensity(s, method="MovingAverage")

## remove baseline
s <- removeBaseline(s)

## plot spectrum
plot(s)

## call peak detection
p <- detectPeaks(s)

## draw peaks on the plot
points(p)

## label 10 highest peaks
top10 <- intensity(p) \%in\% sort(intensity(p), decreasing=TRUE)[1:10]
labelPeaks(p, index=top10)
}
\keyword{methods}

