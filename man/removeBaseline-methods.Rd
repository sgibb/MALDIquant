\name{removeBaseline-methods}
\docType{methods}
\alias{removeBaseline}
\alias{removeBaseline,MassSpectrum-method}
\alias{removeBaseline,list-method}
\title{Removes the baseline of a MassSpectrum object.}
\description{
This method removes the baseline of mass spectrometry data
(represented by a \code{\linkS4class{MassSpectrum}} object).\cr
The intensity of the mass spectrometry data would be reduced by 
\code{baseline}.
}
\usage{
\S4method{removeBaseline}{MassSpectrum}(object,
    fun,
    \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MassSpectrum}} object or a \code{list} of
        \code{\linkS4class{MassSpectrum}} objects.}
    \item{fun}{\code{function}, baseline estimation function. If no
        \code{fun} argument is given the baseline would be estimate 
        automatically by calling
        \code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}}.}
    \item{\dots}{arguments to be passed to \code{fun} or to
        \code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}}.}
}
\details{
    \code{fun}: A user-defined function to estimate the baseline of a
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
    A correct baseline estimation function has to return a two column matrix
    with mass (x-values) as first and intensities (y-values) as second column.
    \cr
    \preformatted{
## e.g. moving median baseline estimation
estimateBaselineMovingMedian <- function(x, y, halfWindowSize=100) {
    if (halfWindowSize<1) {
        stop(sQuote("halfWindowSize"), "=", halfWindowSize, " is too small!");
    }
    
    m <- runmed(y, k=(2*halfWindowSize+1));

    return(cbind(x, m));
}
    }

}
\value{
    Returns a modified \code{\linkS4class{MassSpectrum}} object with reduced
    intensities.
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}}
}
\examples{
## load library
library("MALDIquant");

## load example data
data("fiedler2009subset", package="MALDIquant");

## choose only the first mass spectrum
s <- fiedler2009subset[[1]];

## plot spectrum
plot(s);

## subtract baseline 
b <- removeBaseline(s);

## draw modified spectrum on the plot
lines(b, col="blue");
}
\keyword{methods}

