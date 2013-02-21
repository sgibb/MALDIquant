\name{.estimateBaselineTopHat}
\alias{.estimateBaselineTopHat}
\title{Estimates the baseline by TopHat filter.}
\description{
This function estimates the baseline of mass spectrometry data.\cr
The baseline estimation is based on the TopHat filter. \cr
It is hidden by \code{NAMESPACE} because it is a private function and
\code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}} should be used
instead.
}
\usage{
.estimateBaselineTopHat(x, y, halfWindowSize=100)
}
\arguments{
  \item{x}{vector of x values (only needed to return a two-column matrix)}
  \item{y}{vector of y values}
  \item{halfWindowSize}{half size of the moving window for the TopHat filter.
    (The resulting window reaches from \code{mass[cur_index-halfWindowSize]}
    to \code{mass[cur_index+halfWindowSize]}.}
}
\value{
  Returns a two column matrix (first column: mass, second column: intensity)
  of the estimated baseline.
}
\references{
M. van Herk. 1992.
A Fast Algorithm for Local Minimum and Maximum Filters on Rectangular and
Octagonal Kernels.
Pattern Recognition Letters 13.7: 517-521.

J. Y. Gil and M. Werman. 1996.
Computing 2-Dimensional Min, Median and Max Filters.
IEEE Transactions: 504-507.
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}},
\code{\link[MALDIquant]{removeBaseline,MassSpectrum-method}}
}
\examples{
## load library
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## choose only the first mass spectrum
s <- fiedler2009subset[[1]]

## plot spectrum
plot(s)

## estimate baseline (try different parameters)
b1 <- estimateBaseline(s, method="TopHat", halfWindowSize=75)
b2 <- estimateBaseline(s, method="TopHat", halfWindowSize=150)

## draw baselines on the plot
lines(b1, col=2)
lines(b2, col=3)

## draw legend
legend(x="topright", lwd=1, legend=paste("halfWindowSize=", c(75, 150), sep=""),
       col=c(2, 3))
}
\keyword{internal}

