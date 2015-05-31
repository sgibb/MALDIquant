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
\code{\link[MALDIquant]{plotMsiSlice,list-method}}

Website: \url{http://strimmerlab.org/software/maldiquant/}

Example:
\url{https://github.com/sgibb/MALDIquantExamples/blob/master/vignettes/ims.Rnw}

Shiny example: \url{https://github.com/sgibb/ims-shiny/}
}
\examples{
\dontrun{
msiSlices(spectra, center=3362.3, tolerance=0.25)
}
}
