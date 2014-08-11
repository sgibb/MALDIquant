\name{plotImsSlice}
\title{Plots an Imaging Mass Spectrometry dataset.}
\alias{plotImsSlice}
\description{
This function allows to plot a slice of an Imaging Mass Spectrometry dataset
represented by a \code{list} of \code{\linkS4class{AbstractMassObject}} objects.
}
\usage{
plotImsSlice(x, range=c(0, Inf),
             sub=paste0("m/z: ", range[1L], "-", range[2L], ""),
             removeEmptyRows=TRUE, removeEmptyCols=TRUE,
             colRamp=colorRamp(c("black", "blue", "green", "yellow", "red")),
             interpolate=FALSE, \dots)
}
\arguments{
  \item{x}{\code{list}, list of
    \code{\linkS4class{MassSpectrum}}/\code{\linkS4class{MassPeaks}} objects.}
  \item{range}{\code{double}, length 2, range/thickness of the slice.}
  \item{sub}{\code{character}, sub title for the plot, see
    \code{\link[graphics]{title}}.}
  \item{removeEmptyRows}{\code{logical}, Should empty rows be removed?}
  \item{removeEmptyCols}{\code{logical}, Should empty columns be removed?}
  \item{colRamp}{colours as \code{colorRamp} \code{function}, see
    \code{\link[grDevices]{colorRamp}} for details.}
  \item{interpolate}{logical, use linear interpolation when drawing the image,
    see \code{\link[graphics]{rasterImage}} for details.}
  \item{\dots}{arguments to be passed to \code{\link[graphics]{plot}}.}
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
\code{\link[MALDIquant]{plot,MassSpectrum,missing-method}}

Website: \url{http://strimmerlab.org/software/maldiquant/}

Example:
\url{https://github.com/sgibb/MALDIquantExamples/blob/master/vignettes/ims.Rnw}

Shiny example: \url{http://spark.rstudio.com/sgibb/ims/},
\url{https://github.com/sgibb/ims-shiny/}
}
\examples{
\dontrun{
plotImsSlice(spectra, range = c(3361.8, 3362.8))
}
}
\keyword{Graphics|hplot}

