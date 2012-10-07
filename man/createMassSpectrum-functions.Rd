\name{createMassSpectrum}
\alias{createMassSpectrum}
\title{
Creates a MassSpectrum object.
}
\description{
This function creates a \code{\linkS4class{MassSpectrum}} object.
}
\usage{
createMassSpectrum(mass, intensity, metaData=list())
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
Returns a \code{\linkS4class{MassSpectrum}} object.
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\examples{
## load library
library("MALDIquant");

## create a MassSpectrum object by default constructor
s <- createMassSpectrum(mass=1:100, intensity=rnorm(100)^2,
                        metaData=list(name="example spectrum"));

## show some details
s;
}
\keyword{methods}

