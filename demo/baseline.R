## This is a MALDIquant example file. It is released into public domain with the
## right to use it for any purpose but without any warranty.


## baseline demo


## load necessary packages
## requires MALDIquant >= 1.9
library("MALDIquant")

## load example spectra
data("fiedler2009subset", package="MALDIquant")

## choose only spectrum 1
s <- fiedler2009subset[[1]]

## test different baseline estimation methods
bSnip <- estimateBaseline(s, method="SNIP")
plot(s, main="SNIP Baseline [default]")
lines(bSnip, lwd=2, col=2)

bTopHat <- estimateBaseline(s, method="TopHat")
plot(s, main="TopHat Baseline")
lines(bTopHat, lwd=2, col=2)

bConvexHull <- estimateBaseline(s, method="ConvexHull")
plot(s, main="ConvexHull Baseline")
lines(bConvexHull, lwd=2, col=2)

bMedian <- estimateBaseline(s, method="median")
plot(s, main="Median Baseline")
lines(bMedian, lwd=2, col=2)
