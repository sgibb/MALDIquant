\name{removeBaseline-methods}
\docType{methods}
\alias{removeBaseline}
\alias{removeBaseline,MassSpectrum-method}
\alias{removeBaseline,MassSpectrumOnDisk-method}
\alias{removeBaseline,list-method}
\title{Removes the baseline of a MassSpectrum object.}
\description{
This method removes the baseline of mass spectrometry data
(represented by a \code{\linkS4class{MassSpectrum}/\linkS4class{MassSpectrumOnDisk}} object).
The intensity of the mass spectrometry \cr data would be reduced by
\code{baseline}.
}
\usage{
\S4method{removeBaseline}{MassSpectrum}(object,
  method=c("SNIP", "TopHat", "ConvexHull", "median"),
  \dots)
\S4method{removeBaseline}{MassSpectrumOnDisk}(object,
  method=c("SNIP", "TopHat", "ConvexHull", "median"),
  \dots)
\S4method{removeBaseline}{list}(object, \dots)
}
\arguments{
  \item{object}{\code{\linkS4class{MassSpectrum}/\linkS4class{MassSpectrumOnDisk}} object or a \code{list} thereof.}

  \item{method}{used baseline estimation method, one of
    \code{"SNIP"}, \code{"TopHat"}, \code{"ConvexHull"} or \code{"median"}. See
    \code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}} for details.
  }
  \item{\dots}{arguments to be passed to
    \code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}}. If
    \code{object} is a \code{list} \code{mc.cores} is also supported.}
}
\value{
  Returns a modified \code{\linkS4class{MassSpectrum}/\linkS4class{MassSpectrumOnDisk}} object with reduced
  intensities.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\linkS4class{MassSpectrum}},
\code{\link[MALDIquant]{estimateBaseline,MassSpectrum-method}}

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

## plot spectrum
plot(s)

## subtract baseline
b <- removeBaseline(s, method="SNIP")

## draw modified spectrum on the plot
lines(b, col="blue")
}
\keyword{methods}

