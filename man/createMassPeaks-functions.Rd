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
createMassPeaks(mass, intensity, snr=rep(NA, length(intensity)), metaData=list())
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
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}},
\code{\linkS4class{MassPeaks}}
}
\examples{
## load library
library("MALDIquant")

## create a MassPeaks object by default constructor
s <- createMassPeaks(mass=1:100, intensity=rnorm(100)^2,
                     metaData=list(name="example peaks"))

## show some details
s
}
\keyword{methods}

