\name{plot-methods}
\docType{methods}
\alias{plot,AbstractMassObject,missing-method}
\alias{plot,MassSpectrum,missing-method}
\title{Plots an AbstractMassObject object.}
\description{
This is an overloaded method to allow plotting of an
\code{\linkS4class{AbstractMassObject}} object.
}
\usage{
\S4method{plot}{AbstractMassObject,missing}(x, col="black",
   xlab="mass", ylab="intensity",
   type=ifelse(isMassPeaks(x), "h", "l"),
   xlim=c(ifelse(length(x@mass), min(x@mass, na.rm=TRUE), 0),
      ifelse(length(x@mass), max(x@mass, na.rm=TRUE), 1)),
   ylim=c(0, ifelse(length(x@intensity), max(x@intensity, na.rm=TRUE), 1)),
   main=x@metaData$name, sub=x@metaData$file,
   cex.sub=0.75, col.sub="#808080",
   abline.col="#808080", \dots)
}
\arguments{
  \item{x}{\code{\linkS4class{MassSpectrum}} object}
  \item{col}{line colour, see \code{\link[graphics]{par}}}
  \item{xlab}{title for the x-axis, see \code{\link[graphics]{title}}}
  \item{ylab}{title for the y-axis, see \code{\link[graphics]{title}}}
  \item{type}{type of plot: see \code{\link[graphics]{plot}}}
  \item{xlim}{the x limits (x1, x2) of the plot, see
    \code{\link[graphics]{plot.default}}}
  \item{ylim}{the y limits (y1, y2) of the plot, see
    \code{\link[graphics]{plot.default}}}
  \item{main}{title for the plot, see \code{\link[graphics]{title}}}
  \item{sub}{sub title for the plot, see \code{\link[graphics]{title}}}
  \item{cex.sub}{sub title font size, see \code{\link[graphics]{par}}}
  \item{col.sub}{sub title color, see \code{\link[graphics]{par}}}
  \item{abline.col}{colour for horizontal line at y=0}
  \item{\dots}{arguments to be passed to \code{\link[graphics]{plot}}}
}
\references{
See website: \url{http://strimmerlab.org/software/maldiquant/}
}
\author{
Sebastian Gibb
}
\seealso{
\code{\linkS4class{AbstractMassObject}}
}
\examples{
## load package
library("MALDIquant")

## create a MassSpectrum object by default constructor
s <- createMassSpectrum(mass=1:100, intensity=rnorm(100)^2,
                        metaData=list(name="example"))

## show some details
s

## plot spectrum
plot(s)
}
\keyword{methods}
\keyword{Graphics|hplot}

