## This is a MALDIquant example file. It is released into public domain with the
## right to use it for any purpose but without any warranty.


## peaks demo


## load necessary packages
## requires MALDIquant >= 1.9
library("MALDIquant")

## load example spectra
data("fiedler2009subset", package="MALDIquant")

## choose only spectrum 1
s1 <- fiedler2009subset[[1]]


## preprocessing
## sqrt transform (for variance stabilization)
s2 <- transformIntensity(s1, method="sqrt")


## 21 point Savitzky-Golay-Filter for smoothing spectra
## (maybe you have to adjust the halfWindowSize;
## you could use a simple moving average instead)
## see ?smoothIntensity
s3 <- smoothIntensity(s2, method="SavitzkyGolay", halfWindowSize=10)


## remove baseline
## (maybe you have to adjust iterations to your spectra; high resolution
## spectra need a much lower iteration number (halfWindowSize, for some other
## baseline estimation algorithms)
## see ?removeBaseline, ?estimateBaseline
s4 <- removeBaseline(s3, method="SNIP", iterations=100)


## run peak detection
## (maybe you need to adjust halfWindowSize [decreasing it for high resolution
## spectra] and SNR [a higher value increase the True-Positive-Rate but decrease
## sensitivity])
## see ?detectPeaks, ?estimateNoise
p <- detectPeaks(s4, method="MAD", halfWindowSize=20, SNR=2)


## produce some plots
par(mfrow=c(2,3))

xlim <- range(mass(s1)) # use same xlim on all plots for better comparison

plot(s1, main="1: raw", sub="", xlim=xlim)
plot(s2, main="2: variance stabilization", sub="", xlim=xlim)
plot(s3, main="3: smoothing", sub="", xlim=xlim)
plot(s4, main="4: baseline correction", sub="", xlim=xlim)

plot(s4, main="5: peak detection", sub="", xlim=xlim)
points(p)


## label top 20 peaks
top20 <- intensity(p) %in% sort(intensity(p), decreasing=TRUE)[1:20]
labelPeaks(p, index=top20, underline=TRUE)

plot(p, main="6: peak plot", sub="", xlim=xlim)
labelPeaks(p, index=top20, underline=TRUE)

par(mfrow=c(1,1))
