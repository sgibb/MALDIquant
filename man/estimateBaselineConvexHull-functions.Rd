\name{.estimateBaselineConvexHull}
\alias{.estimateBaselineConvexHull}
\title{Estimates the baseline by a Convex Hull.}
\description{
This function estimates the baseline of mass spectrometry data.\cr
The baseline estimation is based on creating a Convex Hull below the
spectrum.\cr
It is hidden by \code{NAMESPACE} because it is a private function and 
\code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}} should be used
instead.
}
\usage{
.estimateBaselineConvexHull(x, y)
}
\arguments{
    \item{x}{vector of x values}
    \item{y}{vector of y values}
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
\code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}},
\code{\link[MALDIquant]{removeBaseline,MassSpectrum-method}},
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
b <- estimateBaseline(s, method="ConvexHull");

## draw baseline on the plot
lines(b, col="red");
}
\keyword{internal}

