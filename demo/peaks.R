## This is a MALDIquant example file. It is released into public domain with the
## right to use it for any purpose but without any warranty.


## peaks demo


## load necessary libraries
library("MALDIquant")

## load example spectra
data("fiedler2009subset", package="MALDIquant")

## choose only spectrum 1
s <- fiedler2009subset[[1]]

## remove baseline
s <- removeBaseline(s)

## detect peaks
p <- detectPeaks(s, SNR=10)

## plot spectrum
plot(s, main="example spectrum 1")

## draw vertical lines
lines(p, col=4)

## mark peaks
points(p, col=2, pch=4)

## label some peaks
labelPeaks(p, mass=c(1020, 1206, 1350, 1466, 1616, 2660, 2932, 3191, 3262,
                     3883, 4209, 5336, 5904, 7766, 9290))

