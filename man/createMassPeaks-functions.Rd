\name{createMassPeaks}
\alias{createMassPeaks}
\title{
Creates a MassPeaks object.
}
\description{
This function creates a \code{\linkS4class{MassPeaks}} object. Normally it
shouldn't called by the user. Try
\code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}} instead.
}
\usage{
createMassPeaks(mass, intensity, snr=as.double(rep.int(NA, length(intensity))),
                metaData=list())
}
\arguments{
  \item{mass}{
    \code{vector}, mass or mass-to-charge ratio.
  }
  \item{intensity}{
    \code{vector}, intensities for measured mass-to-charge ratios.
  }
  \item{snr}{
    \code{vector}, signal-to-noise ratios for intensity values.
  }
  \item{metaData}{
    \code{list}, some metadata to describe the peaks.
  }
}
\value{
Returns a \code{\linkS4class{MassPeaks}} object.
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\seealso{
\code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}},
\code{\linkS4class{MassPeaks}}

Website: \url{http://strimmerlab.org/software/maldiquant/}
}
\examples{
## load package
library("MALDIquant")

## create a MassPeaks object by default constructor
s <- createMassPeaks(mass=1:100, intensity=rnorm(100)^2,
                     metaData=list(name="example peaks"))

## show some details
s
}
\keyword{methods}

