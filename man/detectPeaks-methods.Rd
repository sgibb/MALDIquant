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
    halfWindowSize=20, fun, SNR=2,
    \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MassSpectrum}} object or a \code{list} of
        \code{\linkS4class{MassSpectrum}} objects.}
    \item{halfWindowSize}{\code{numeric}, half window size. \cr
        The resulting window reaches from \code{mass[currentIndex-halfWindowSize]}
        to \code{mass[currentIndex+halfWindowSize]}. A local maximum have to be
        the highest one in the given window to be recognized as peak.}
    \item{fun}{\code{function}, noise estimation function. If no \code{fun}
        argument is given the noise would be estimated automatically by calling
        \code{\link[MALDIquant]{estimateNoise,MassSpectrum-method}}.}
    \item{SNR}{single numeric value. \code{SNR} is an abbreviation for
        \emph{s}ignal-to-\emph{n}oise-\emph{r}atio. A local maximum has to 
        be higher than \code{SNR*noise} to be recognize as peak.}
    \item{\dots}{arguments to be passed to \code{fun} or to
        \code{\link[MALDIquant]{estimateNoise,MassSpectrum-method}}.}
}
\details{
    \code{fun}: A user-defined function to estimate the noise of a
    \code{\linkS4class{MassSpectrum}} object.\cr
    At least three arguments are needed:
    \itemize{
        \item{\code{x}: \code{double}, the mass of the
            \code{\linkS4class{MassSpectrum}} object.
        }
        \item{\code{y}: \code{double}, the intensity of the
            \code{\linkS4class{MassSpectrum}} object.
        }
        \item{\code{\dots}: further arguments
        }
    }
    A correct noise estimation function has to return a two column matrix
    with mass (x-values) as first and intensities (y-values) as second column or
    a single numeric value.
    \cr
    \preformatted{
## e.g. estimate noise using lowess (?lowess)
estimateNoiseLowess <- function(x, y, ...) {
    n <- stats::lowess(x=x, y=y, ...);

    return(cbind(mass=x, intensity=n$y));
}

## use it
p <- detectPeaks(spectra, fun=estimateNoiseLowess) 
    }
}
\value{
    Returns a \code{\linkS4class{MassPeaks}} object.
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
\code{\link[MALDIquant]{estimateNoise,MassSpectrum-method}}
}
\examples{
## load library
library("MALDIquant");

## load example data
data("fiedler2009subset", package="MALDIquant");

## choose only the first mass spectrum
s <- fiedler2009subset[[1]];

## transform intensities
s <- transformIntensity(s, sqrt);

## remove baseline
s <- removeBaseline(s);

## plot spectrum
plot(s);

## call peak detection
p <- detectPeaks(s);

## draw peaks on the plot
points(p);

## label 10 highest peaks
top10 <- intensity(p) \%in\% sort(intensity(p), decreasing=TRUE)[1:10];
labelPeaks(p, index=top10);
}
\keyword{methods}

