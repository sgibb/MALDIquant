\name{.estimateBaselineSnip}
\alias{.estimateBaselineSnip}
\title{Estimates the baseline by SNIP algorithm.}
\description{
This function estimates the baseline of mass spectrometry data.\cr
The baseline estimation is based on the Statistics-sensitive Non-linear
Iterative Peak-clipping algorithm (SNIP) described in
Ryan et al 1988.\cr
It is hidden by \code{NAMESPACE} because it is a private function and
\code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}} should be used
instead.
}
\usage{
.estimateBaselineSnip(x, y, iterations=100)
}
\arguments{
  \item{x}{vector of x values (only needed to return a two-column matrix)}
  \item{y}{vector of y values}
  \item{iterations}{half window size\cr
    The resulting window reaches from \code{mass[cur_index-iterations]}
    to \code{mass[cur_index+iterations]}.}
}
\value{
  Returns a two column matrix (first column: mass, second column: intensity)
  of the estimated baseline.
}
\details{
The algorithm based on the following equation:
\deqn{y_i(k) = \min \{ y_i, \frac{(y_{i-k}+y_{i+k})}{2} \}}{y_i(k) = min \{ y_i, (y_{i-k}+y_{i+k})/2 \}}
Where \emph{k} is increase by one until \code{iterations} is reached.
}
\references{
C.G. Ryan, E. Clayton, W.L. Griffin, S.H. Sie, and D.R. Cousens. 1988.
Snip, a statistics-sensitive background treatment for the quantitative analysis
of pixe spectra in geoscience applications.
Nuclear Instruments and Methods in Physics Research Section B:
Beam Interactions with Materials and Atoms, 34(3): 396-402.
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
b1 <- estimateBaseline(s, method="SNIP", iterations=75)
b2 <- estimateBaseline(s, method="SNIP", iterations=150)

## draw baselines on the plot
lines(b1, col=2)
lines(b2, col=3)

## draw legend
legend(x="topright", lwd=1, legend=paste("iterations=", c(75, 150), sep=""),
       col=c(2, 3))
}
\keyword{internal}

