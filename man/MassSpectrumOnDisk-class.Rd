\name{MassSpectrumOnDisk-class}
\Rdversion{1.1}
\docType{class}
\alias{MassSpectrumOnDisk}
\alias{MassSpectrumOnDisk-class}
\alias{isMassSpectrumOnDisk}
\alias{[,MassSpectrumOnDisk,logical,missing-method}
\alias{[,MassSpectrumOnDisk,numeric,missing-method}

\title{Class "MassSpectrumOnDisk"}
\description{
\code{\linkS4class{MassSpectrumOnDisk}} represents a single spectrum of a
mass spectrometry measurement. It is similar to the
\code{\linkS4class{MassSpectrum}} but its mass and intensity values are stored
on disk. Subsetting or averaging is currently not supported and will result in
a new \code{\linkS4class{MassSpectrum}} object (all values are stored in
memory).\cr
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\link[MALDIquant]{MassSpectrum}},
\code{\linkS4class{AbstractMassObject}}

Website: \url{http://strimmerlab.org/software/maldiquant/}
}
\keyword{classes}
