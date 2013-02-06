\name{standardizeTotalIonCurrent-methods}
\docType{methods}
\alias{standardizeTotalIonCurrent}
\alias{standardizeTotalIonCurrent,list-method}
\title{Standardizes the Total Ion Current of a list of AbstractMassObject objects.}
\description{
This method sets the TIC (\emph{T}otal \emph{I}on \emph{C}urrent) of a list of
\code{\linkS4class{AbstractMassObject}} objects to one.\cr
That results in a decreased intensity of the mass spectrometry data.
}
\usage{
\S4method{standardizeTotalIonCurrent}{list}(object,
  value=1)
}
\arguments{
  \item{object}{a \code{list} of \code{\linkS4class{AbstractMassObject}}
    objects}
  \item{value}{numeric, resulting TIC}
}
\value{
  Returns a \code{list} of modified \code{\linkS4class{AbstractMassObject}}
  objects.
}
\details{
\code{\link[MALDIquant]{standardizeTotalIonCurrent}} is a shortcut for:
\preformatted{
  sTIC <- lapply(spectraList, "totalIonCurrent<-", 1)
}
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{AbstractMassObject}},
\code{\linkS4class{MassSpectrum}},
\code{\linkS4class{MassPeaks}}
}
\examples{
## load library
library("MALDIquant")

## load example data
data("fiedler2009subset", package="MALDIquant")

## show TIC in spectrum 1
totalIonCurrent(fiedler2009subset[[1]]) # 90312326

## set TIC to 1 in all spectra
sTIC <- standardizeTotalIonCurrent(fiedler2009subset)

## show TIC in spectrum 1
totalIonCurrent(sTIC[[1]])              # 1
}
\keyword{methods}

