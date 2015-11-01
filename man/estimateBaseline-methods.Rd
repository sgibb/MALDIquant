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
  method=c("SNIP", "TopHat", "ConvexHull", "median"),
  \dots)
}
\arguments{
  \item{object}{\code{\linkS4class{MassSpectrum}} object}
  \item{method}{used baseline estimation method, one of
    \code{"SNIP"}, \code{"TopHat"}, \code{"ConvexHull"} or \code{"median"}.
  }
  \item{\dots}{arguments to be passed to \code{method}}
}
\details{
  \describe{
    \item{\code{"SNIP"}:}{
      This baseline estimation is based on the Statistics-sensitive Non-linear
      Iterative Peak-clipping algorithm (SNIP) described in Ryan et al 1988.\cr

      The algorithm based on the following equation:
      \deqn{y_i(k) = \min \{ y_i, \frac{(y_{i-k}+y_{i+k})}{2} \}}{y_i(k) = min \{ y_i, (y_{i-k}+y_{i+k})/2 \}}

      It has two additional arguments namely \code{iterations} and
      \code{decreasing}. \code{iterations} controls the window size (\emph{k};
      similar to \code{halfWindowSize} in \code{"TopHat"}, \code{"Median"}) of
      the algorithm. The resulting window reaches from
      \code{mass[cur_index-iterations]} to \code{mass[cur_index+iterations]}.

      \code{decreasing}: In Morhac 2009 a decreasing clipping window is
      suggested to get a smoother baseline.  For \code{decreasing = TRUE}
      (\code{decreasing = FALSE}) \emph{k}=\code{iterations} is decreased
      (increased) by one until zero (\code{iterations}) is reached. The default
      setting is \code{decreasing = TRUE}.
    }
    \item{\code{"TopHat"}:}{
      This algorithm applies a moving minimum (erosion filter) and subsequently
      a moving maximum (dilation filter) filter on the intensity values. The
      implementation is based on van Herk 1996.
      It has an additional \code{halfWindowSize} argument determining the half
      size of the moving window for the TopHat filter. The resulting window
      reaches from \code{mass[cur_index-halfWindowSize]} to
      \code{mass[cur_index+halfWindowSize]}.
    }
    \item{\code{"ConvexHull"}:}{
      The baseline estimation is based on a convex hull constructed below the
      spectrum.
    }
    \item{\code{"median"}:}{
      This baseline estimation uses a moving median. It is based on
      \code{\link[stats]{runmed}}. The additional argument \code{halfWindowSize}
      corresponds to the \code{k} argument in \code{\link[stats]{runmed}}
      (\code{k = 2 * halfWindowSize + 1}) and controls the half size of the
      moving window. The resulting window reaches from
      \code{mass[cur_index-halfWindowSize]} to
      \code{mass[cur_index+halfWindowSize]}.
    }
  }
}
\value{
Returns a two column matrix (first column: mass, second column: intensity)
of the estimated baseline.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\references{
\code{"SNIP"}:\cr
C.G. Ryan, E. Clayton, W.L. Griffin, S.H. Sie, and D.R. Cousens. 1988.
Snip, a statistics-sensitive background treatment for the quantitative analysis
of pixe spectra in geoscience applications.
Nuclear Instruments and Methods in Physics Research Section B:
Beam Interactions with Materials and Atoms, 34(3): 396-402.

M. Morhac. 2009.
An algorithm for determination of peak regions and baseline elimination in
spectroscopic data.
Nuclear Instruments and Methods in Physics Research Section A:
Accelerators, Spectrometers, Detectors and Associated Equipment, 600(2),
478-487.

\code{"TopHat"}:\cr
M. van Herk. 1992.
A Fast Algorithm for Local Minimum and Maximum Filters on Rectangular and
Octagonal Kernels.
Pattern Recognition Letters 13.7: 517-521.

J. Y. Gil and M. Werman. 1996.
Computing 2-Dimensional Min, Median and Max Filters.
IEEE Transactions: 504-507.

\code{"ConvexHull"}:\cr
Andrew, A. M. 1979.
Another efficient algorithm for convex hulls in two dimensions.
Information Processing Letters, 9(5), 216-219.
}
\seealso{
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{removeBaseline,MassSpectrum-method}}

\code{demo("baseline")}

Website: \url{http://strimmerlab.org/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## choose only the first mass spectrum
s <- fiedler2009subset[[1]]


## SNIP
plot(s)

## estimate baseline
b <- estimateBaseline(s, method="SNIP", iterations=100)

## draw baseline on the plot
lines(b, col="red")


## TopHat
plot(s)

## estimate baseline (try different parameters)
b1 <- estimateBaseline(s, method="TopHat", halfWindowSize=75)
b2 <- estimateBaseline(s, method="TopHat", halfWindowSize=150)

## draw baselines on the plot
lines(b1, col=2)
lines(b2, col=3)

## draw legend
legend(x="topright", lwd=1, legend=paste0("halfWindowSize=", c(75, 150)),
       col=c(2, 3))


## ConvexHull
plot(s)

## estimate baseline
b <- estimateBaseline(s, method="ConvexHull")

## draw baseline on the plot
lines(b, col="red")


## Median
plot(s)

## estimate baseline
b <- estimateBaseline(s, method="median")

## draw baseline on the plot
lines(b, col="red")
}
\keyword{methods}

