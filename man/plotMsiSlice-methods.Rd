\name{plotMsiSlice-methods}
\docType{methods}
\title{Plots a Mass Spectrometry Imaging dataset.}
\alias{plotMsiSlice}
\alias{plotMsiSlice,list-method}
\alias{plotMsiSlice,array-method}
\alias{plotMsiSlice,matrix-method}
\description{
This function allows to plot a slice of a mass spectrometry imaging dataset
represented by a \code{list} of \code{\linkS4class{AbstractMassObject}} objects
or an \code{array} or a \code{matrix}.
}
\usage{
\S4method{plotMsiSlice}{list}(x, center, tolerance,
  colRamp=colorRamp(c("black", "blue", "green", "yellow", "red")),
  interpolate=FALSE, legend=TRUE, alignLabels=FALSE, combine=FALSE,
  \dots)
\S4method{plotMsiSlice}{array}(x,
  colRamp=colorRamp(c("black", "blue", "green", "yellow", "red")),
  interpolate=FALSE, legend=TRUE, alignLabels=FALSE, combine=FALSE,
  plotInteractive=FALSE, \dots)
\S4method{plotMsiSlice}{matrix}(x,
  colRamp=colorRamp(c("black", "blue", "green", "yellow", "red")),
  interpolate=FALSE, scale=TRUE, legend=scale, \dots)
}
\arguments{
  \item{x}{The mass spectrometry imaging dataset. It could be a \code{list} of
    \code{\linkS4class{MassSpectrum}}/\code{\linkS4class{MassPeaks}} objects or
    an \code{array} (e.g. generated by \code{\link[MALDIquant]{msiSlices}}) or
    a \code{matrix}.}
  \item{center}{\code{double}, if \code{x} is a \code{list} of
    \code{\linkS4class{MassSpectrum}}/\code{\linkS4class{MassPeaks}} objects
    this argument represent the \emph{center} mass value of the slices,
    see \code{\link[MALDIquant]{msiSlices}} for details.}
  \item{tolerance}{\code{double}, if \code{center} is given \code{tolerance}
    specifies the thickness of the slices (\code{center + c(-tolerance,
    tolerance)}), see \code{\link[MALDIquant]{msiSlices}} for details.}
  \item{colRamp}{colours as \code{colorRamp} \code{function}, see
    \code{\link[grDevices]{colorRamp}} for details. If \code{combine=TRUE}
    multiple colour functions must be applied as \code{list} with an length that
    equals the number of given centers.}
  \item{interpolate}{\code{logical}, use linear interpolation when drawing the
    image, see \code{\link[graphics]{rasterImage}} for details.}
  \item{scale}{\code{logical}, if \code{TRUE} all values are divided by the
    maximal value of the slice to get values between 0 and 1.}
  \item{legend}{\code{logical}, if \code{TRUE} a reference color gradient is
    plotted on the right hand side of the plot. The upper color represents the
    highest value in the slice and the lower color the lowest value
    respectively. The \code{legend} is disabled if \code{scale=FALSE}.}
  \item{alignLabels}{\code{logical}, if \code{combine=TRUE} and
    \code{alignLabels=TRUE} the center positions below the legend are aligned
    on the right margin otherwise the aligned to their corresponding gradient.}
  \item{combine}{\code{logical}, if \code{TRUE} multiple centers are plotted in
    one image. Therefore it would be necessary to apply a list of \code{colRamp}
    functions (one function for each center). The intensity values for each
    center of each pixel are compared against each other and the highest scaled
    intensity determines the center (and the corresponding \code{colRamp}).}
  \item{plotInteractive}{
    \code{logical}, if the slice \code{array} contains multiple centers,
    \code{combine=FALSE} and an interactive plotting device is used a warning is
    thrown and only the first center would be plotted. Use
    \code{plotInteractive=TRUE} to overwrite this behaviour and to plot multiple
    centers on an interactive device.}
  \item{\dots}{arguments to be passed to \code{\link[graphics]{plot}}, e.g.
    \code{main}.}
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
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\linkS4class{AbstractMassObject}},
\code{\linkS4class{MassSpectrum}},
\code{\linkS4class{MassPeaks}},
\code{\link[MALDIquant]{coordinates,AbstractMassObject-method}},
\code{\link[MALDIquant]{msiSlices}},
\code{\link[MALDIquant]{plot,MassSpectrum,missing-method}}

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

plotMsiSlice(fiedler2009subset, center=8936.97, tolerance=0.25)

plotMsiSlice(fiedler2009subset, center=c(5864.49, 8936.97), tolerance=0.25,
             combine=TRUE,
             colRamp=list(colorRamp(c("#000000", "#FF00FF")),
                          colorRamp(c("#000000", "#00FF00"))))
}
\keyword{Graphics|hplot}
