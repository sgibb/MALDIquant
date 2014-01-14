\name{MALDIquant-package}
\alias{MALDIquant-package}
\alias{MALDIquant}
\docType{package}
\title{
Quantitative Analysis of Mass Spectrometry Data
}
\description{
MALDIquant provides a complete analysis pipeline for MALDI-TOF and other
2-dimensional mass spectrometry data. Distinctive features include baseline
subtraction methods such as TopHat or SNIP, peak alignment using warping
functions, handling of replicated measurements as well as allowing spectra with
different resolutions.

For a first overview run \code{demo("MALDIquant")}.
}
\details{
\tabular{ll}{
Package: \tab MALDIquant\cr
Version: \tab 1.9\cr
Date: \tab 2014-01-14\cr
License: \tab GPL (>= 3)\cr
URL: \tab http://strimmerlab.org/software/maldiquant/\cr
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
\url{http://bioinformatics.oxfordjournals.org/content/28/17/2270.abstract}

See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\seealso{
  \itemize{
    \item Run demo files: \code{demo("MALDIquant")}.
    \item List all available manual pages: \code{library(help="MALDIquant")}.
  }
}
