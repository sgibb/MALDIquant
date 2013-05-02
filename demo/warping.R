## This is a MALDIquant example file. It is released into public domain with the
## right to use it for any purpose but without any warranty.


## warping demo


## load necessary libraries
library("MALDIquant")

## load example spectra
data("fiedler2009subset", package="MALDIquant")

## use only 4 spectra
spectra <- fiedler2009subset[seq(1, 16, by=4)]

## some preprocessing

## sqrt transform (for variance stabilization)
spectra <- transformIntensity(spectra, sqrt)

## simple 5 point moving average for smoothing spectra
spectra <- transformIntensity(spectra, movingAverage, halfWindowSize=2)

## remove baseline
spectra <- removeBaseline(spectra)

## calibrate intensity values by "total ion current"
spectra <- standardizeTotalIonCurrent(spectra)

## run peak detection
peaks <- detectPeaks(spectra)

## warping
par(mfrow=c(2, 2))
warpingFunctions <- determineWarpingFunctions(peaks, tolerance=0.001,
                                              plot=TRUE, plotInteractive=TRUE)

## warp spectra
warpedSpectra <- warpMassSpectra(spectra, warpingFunctions)
## warp peaks
warpedPeaks <- warpMassPeaks(peaks, warpingFunctions)

## compare some regions in a plot
colour <- rainbow(length(spectra))

par(mfrow=c(2, 2))

## helper function to avoid double coding
plotSpectra <- function(unwarped, warped, range) {
  plot(unwarped[[1]], main=paste0("unwarped spectra (mass ",
                                  paste0(range, collapse=":"), " Da)"),
       xlim=range, ylim=c(0, 4e-4), type="n")

  for (i in seq(along=unwarped)) {
    lines(unwarped[[i]], col=colour[i])
  }

  plot(unwarped[[1]], main=paste0("warped spectra (mass ",
                                 paste0(range, collapse=":"), " Da)"),
       xlim=range, ylim=c(0, 4e-4), type="n")

  for (i in seq(along=warped)) {
    lines(warped[[i]], col=colour[i])
  }
}

plotSpectra(spectra, warpedSpectra, c(4180, 4240))
plotSpectra(spectra, warpedSpectra, c(9200, 9400))

