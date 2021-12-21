\name{MALDIquant-package}
\alias{MALDIquant-package}
\alias{MALDIquant}
\docType{package}
\title{
Quantitative Analysis of Mass Spectrometry Data
}
\description{
MALDIquant provides a complete analysis pipeline for
matrix-assisted laser desorption/ionization-time-of-flight (MALDI-TOF)
and other two-dimensional mass spectrometry data.

In addition to commonly used plotting and processing methods it
includes distinctive features, namely baseline
subtraction methods such as morphological filters (TopHat) or the
statistics-sensitive non-linear iterative peak-clipping algorithm
(SNIP), peak alignment using warping functions, handling of replicated
measurements as well as allowing spectra with different resolutions.

For a first overview see
\code{vignette("MALDIquant-intro", package="MALDIquant")} and/or run
\code{demo("MALDIquant")}.
}
\details{
\tabular{ll}{
Package: \tab MALDIquant\cr
License: \tab GPL (>= 3)\cr
URL: \tab https://strimmerlab.github.io/software/maldiquant/\cr
}

Main classes:

\itemize{
  \item \code{\linkS4class{MassPeaks}}: Represents a peak list of a single
  spectrum.
  \item \code{\linkS4class{MassSpectrum}}: Represents a single spectrum.
}

The accompanying website (see below) provides example R scripts to illustrate
the functionality of this package, too.
}
\author{
Sebastian Gibb

Maintainer: Sebastian Gibb \email{mail@sebastiangibb.de}
}
\references{
S. Gibb and K. Strimmer. 2012.
MALDIquant: a versatile R package for the analysis of mass spectrometry data.
Bioinformatics 28: 2270-2271.
\url{https://academic.oup.com/bioinformatics/article-abstract/28/17/2270/246552}

Website: \url{https://strimmerlab.github.io/software/maldiquant/}
}
\seealso{
  \itemize{
    \item Introduction:
      \code{vignette("MALDIquant-intro", package="MALDIquant")}.
    \item Run demo files: \code{demo("MALDIquant")}.
    \item List all available manual pages: \code{library(help="MALDIquant")}.
    \item MALDIquant website: \url{https://strimmerlab.github.io/software/maldiquant/}.
    \item more MALDIquant examples and complete analyses:
    \url{https://github.com/sgibb/MALDIquantExamples/}.
  }
}
