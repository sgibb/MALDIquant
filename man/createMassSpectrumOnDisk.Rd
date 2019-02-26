\name{createMassSpectrumOnDisk}
\alias{createMassSpectrumOnDisk}
\title{
Creates a MassSpectrumOnDisk object.
}
\description{
This function creates a \code{createMassSpectrumOnDisk} object. It is identical to \code{createMassSpectrum} except that it uses the \code{matter} package internally to interface directly with mass spectra on disk without loading them into memory. The mass and intensity slots are \code{matter::matter_vec} objects with only the metaData slot loaded into memory.
}
\usage{
createMassSpectrumOnDisk(mass, intensity, metaData=list())
}
\arguments{
  \item{mass}{
    \code{vector}, mass or mass-to-charge ratio
  }
  \item{intensity}{
    \code{vector}, intensities for measured mass-to-charge ratios
  }
  \item{metaData}{
    \code{list}, some metadata to describe the spectrum
  }
}
\value{
Returns a \code{createMassSpectrumOnDisk} object.
}
\author{
Denis Abu Sammour \email{d.abu-sammour@hs-mannheim.de}
}

\references{
\code{"matter"}:\cr
Kylie A. Bemis (2018). matter: A framework for rapid prototyping with binary data on disk. R
  package version 1.8.0. \url{https://github.com/kuwisdelu/matter}.
  }

\seealso{
\code{\linkS4class{createMassSpectrum}}

}
\examples{
## load package
library("MALDIquant")

## create a MassSpectrumOnDisk object by default constructor
s <- createMassSpectrumOnDisk(mass=1:100, intensity=rnorm(100)^2,
                        metaData=list(name="example spectrum"))

## show some details
s
}

\keyword{methods}

