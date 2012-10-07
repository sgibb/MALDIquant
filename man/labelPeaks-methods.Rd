\name{labelPeaks-methods}
\docType{methods}
\alias{labelPeaks}
\alias{labelPeaks,MassPeaks-method}
\title{Draws peak labels to plot.}
\description{
\code{\link[MALDIquant]{labelPeaks}} draws the corresponding mass values on top of the
peaks stored in a \code{\linkS4class{MassPeaks}} object to a plot.
}
\usage{
\S4method{labelPeaks}{MassPeaks}(object, 
    index,
    mass, tolerance=0.002,
    digits=3, underline=TRUE, 
    verticalOffset=abs(diff(par("usr")[3:4]))*0.01,
    absoluteVerticalPos,
    adj=c(0.5, 0), cex=0.7,
    avoidOverlap=FALSE,
    arrowLength=0, arrowLwd=0.5, arrowCol=1,
    \dots)
}
\arguments{
    \item{object}{\code{\linkS4class{MassPeaks}} object.}
    \item{index}{\code{vector}, indices of peaks to label.}
    \item{mass}{\code{vector}, mass of peaks to label.}
    \item{tolerance}{
        \code{double}, maximal deviation of a peak position (mass) to be
        considered.}
    \item{digits}{\code{integer}, number of decimal places.}
    \item{underline}{logical, underline peak values?}
    \item{verticalOffset}{\code{numeric}, move label vertically (relative to
        peak height).}
    \item{absoluteVerticalPos}{\code{numeric}, absolute \code{y} value for the
        label. If missing \code{verticalOffset} is used.}
    \item{adj}{\code{numeric}, adjust text to the left, center, right and top,
        center, bottom; see \code{\link[graphics]{text}}.}
    \item{cex}{\code{numeric}, font size, see \code{\link[graphics]{par}}.}
    \item{avoidOverlap}{\code{logical}, try to find label coordinates to avoid
        overlap.}
    \item{arrowLength, arrowLwd, arrowCol}{arrow parameters, possible vectors.
        \code{NA} values in \code{arrowCol} cause the arrow to be omitted, see
        \code{\link[graphics]{arrows}}.}
    \item{...}{arguments to be passed to \code{\link[graphics]{text}}.}
}
\author{Sebastian Gibb}
\seealso{
    \code{\linkS4class{MassPeaks}},
    \code{\link[MALDIquant]{plot,AbstractMassObject,missing-method}}
}
\examples{
## load library
library("MALDIquant");

## create a MassPeaks object from scratch
p <- createMassPeaks(mass=1:20, intensity=sample(x=100:10000, size=20), 
                     metaData=list(name="example"));

## plot peaks
plot(p);

## label the first 5 peaks
labelPeaks(p, index=1:5);

## label all peaks in mass range 15 to 20 
labelPeaks(p, mass=15:20, underline=FALSE);

## label highest peaks (top 5)
top5 <- intensity(p) \%in\% sort(intensity(p), decreasing=TRUE)[1:5];
labelPeaks(p, index=top5, col="red");


## real example
data("fiedler2009subset")

## a simplified preprocessing
r <- removeBaseline(fiedler2009subset[[1]])
p <- detectPeaks(r)
plot(p)

## label highest peaks (top 10) and avoid label overlap
top10 <- intensity(p) \%in\% sort(intensity(p), decreasing=TRUE)[1:10];
labelPeaks(p, index=top10, avoidOverlap=TRUE, digits=1)
}
\keyword{methods}
\keyword{Graphics|hplot}

