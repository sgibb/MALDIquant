\name{msiSlices}
\title{Turn a list of AbstractMassObjects into a mass spectrometry imaging
  slice.}
\alias{msiSlices}
\description{
This function turns a mass spectrometry imaging dataset represented by a
\code{list} of \code{\linkS4class{AbstractMassObject}} objects into an
\code{\link[MALDIquant]{intensityMatrix}} for each slice (stored in an
\code{array}).
}
\usage{
msiSlices(x, center, tolerance, method=c("sum", "mean", "median"), adjust=TRUE)

}
\arguments{
  \item{x}{a \code{list} of \code{\linkS4class{MassSpectrum}}/
    \code{\linkS4class{MassPeaks}} objects.}
  \item{center}{\code{double}, the \code{center} mas value of each slice.}
  \item{tolerance}{\code{double}, specifies the thickness of the slices
    (\code{center + c(-tolerance, tolerance)}).}
  \item{method}{used aggregation function.}
  \item{adjust}{\code{logical}, if \code{TRUE} the lowest coordinates of the
    mass spectrometry imaging dataset are set to \code{c(x=1, y=1)} to avoid
    \code{NA} values at the borders.}
}
\details{
Each \code{\linkS4class{MassSpectrum}}/\code{\linkS4class{MassPeaks}} object in
\code{x} must contain a \code{list} named \code{imaging} with an element
\code{pos} that stores the \code{x} and \code{y} value of the spectrum, e.g.:
\preformatted{
> metaData(spectra[[1]])$imaging$pos
x y
1 5
}
}
\value{
Returns an \code{\link{array}} of three dimensions. The first and second
dimensions contains the x and y coordinates of the image. The third dimension
represents the index of the \code{center} of each slice. There are two
additional attributes, namely \code{"center"} and \code{"tolerance"} which store
the original \code{center} and \code{tolerance} information.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\linkS4class{AbstractMassObject}},
\code{\linkS4class{MassSpectrum}},
\code{\linkS4class{MassPeaks}},
\code{\link[MALDIquant]{coordinates,AbstractMassObject-method}},
\code{\link[MALDIquant]{plotMsiSlice,list-method}}

Please find real examples on:

Website: \url{https://strimmerlab.github.io/software/maldiquant/}

Vignette: \url{https://github.com/sgibb/MALDIquantExamples/raw/master/inst/doc/nyakas2013.pdf}

Shiny: \url{https://github.com/sgibb/ims-shiny/}
}
\examples{
## load package
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## please note: this is NOT a MSI data set
## we just add some coordinates for demonstration
coordinates(fiedler2009subset) <- cbind(x=rep(1:4, 2), y=rep(1:2, each=4))

slices <- msiSlices(fiedler2009subset, center=c(5864.49, 8936.97),
                    tolerance=0.25)

slices
}
