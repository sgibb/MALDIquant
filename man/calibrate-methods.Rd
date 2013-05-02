\name{calibrate-methods}
\docType{methods}
\alias{calibrate}
\alias{calibrate,matrix-method}
\title{Calibrates intensity matrix.}
\description{
This function calibrates intensities of an intensity matrix created by
\code{\link[MALDIquant]{intensityMatrix}}.
}
\usage{
calibrate(x, \dots)
}
\arguments{
  \item{x}{
    \code{matrix}, an intensity matrix created by
    \code{\link[MALDIquant]{intensityMatrix}}. (Rows have to contain samples
    and columns features (peaks).)}
  \item{\dots}{arguments to be passed to other functions (unneeded until now).}
}
\details{
First a \emph{median spectrum/feature list} is calculated to be used as
reference. Afterwards each sample is divided by the median of the ratios of
sample intensities to reference intensities.
}
\value{
Returns a calibrated \code{matrix} (scale/calibration factors as attribute).
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{AbstractMassObject}}
}
\examples{
## load library
library("MALDIquant")

## create example matrix
m <- matrix(c(1, 2, 3,
              3, 6, 9),
            nrow=2, byrow=TRUE, dimnames=list(paste0("samples", 1:2),
                                              paste("peaks", LETTERS[1:3])))

## show matrix
m

## show calibrated matrix
calibrate(m)
}
\keyword{methods}
