\name{estimateBaseline-methods}
\docType{methods}
\alias{estimateBaseline}
\alias{estimateBaseline,MassSpectrum-method}
\title{Estimates the baseline of a MassSpectrum object.}
\description{
This method estimates the baseline of mass spectrometry data
(represented by a \code{\linkS4class{MassSpectrum}} object).\cr
}
\usage{
\S4method{estimateBaseline}{MassSpectrum}(object, 
    method=c("SNIP", "ConvexHull", "Median"),
    \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MassSpectrum}} object}
    \item{method}{a baseline estimation function; see
        \code{\link[MALDIquant]{.estimateBaselineSnip}},
        \code{\link[MALDIquant]{.estimateBaselineConvexHull}},
        \code{\link[MALDIquant]{.estimateBaselineMedian}}
    }
    \item{\dots}{arguments to be passed to \code{method}}
}
\value{
Returns a two column matrix (first column: mass, second column: intensity)
of the estimated baseline.
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{.estimateBaselineSnip}},
\code{\link[MALDIquant]{.estimateBaselineConvexHull}},
\code{\link[MALDIquant]{.estimateBaselineMedian}},
\code{\link[MALDIquant]{removeBaseline,MassSpectrum-method}}
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

## estimate baseline
b <- estimateBaseline(s);

## draw baseline on the plot
lines(b, col="red");
}
\keyword{methods}

