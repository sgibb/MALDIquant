## This is a MALDIquant example file. It is released into public domain with the
## right to use it for any purpose but without any warranty.


## warping demo


## load necessary packages
## requires MALDIquant >= 1.9
library("MALDIquant")

## load example spectra
data("fiedler2009subset", package="MALDIquant")

## use only 4 spectra
spectra <- fiedler2009subset[seq(1, 16, by=4)]

## some preprocessing

## sqrt transform (for variance stabilization)
spectra <- transformIntensity(spectra, method="sqrt")

## 21 point Savitzky-Golay-Filter for smoothing spectra
spectra <- smoothIntensity(spectra, method="SavitzkyGolay", halfWindowSize=10)

## remove baseline
spectra <- removeBaseline(spectra, method="SNIP", iterations=100)

## calibrate intensity values by "Total Ion Current"
spectra <- calibrateIntensity(spectra, method="TIC")

## run peak detection
peaks <- detectPeaks(spectra, method="MAD", halfWindowSize=20, SNR=2)

## warping
par(mfrow=c(2, 2))
warpingFunctions <- determineWarpingFunctions(peaks, tolerance=0.001,
                                              plot=TRUE, plotInteractive=TRUE)

## warp spectra
warpedSpectra <- warpMassSpectra(spectra, warpingFunctions)
## warp peaks
warpedPeaks <- warpMassPeaks(peaks, warpingFunctions)

## compare some regions in a plot
par(mfrow=c(2, 2))

## helper function to avoid double coding
plotSpectra <- function(unwarped, warped, range) {
  plot(unwarped[[1]], main=paste0("unwarped spectra (mass ",
                                  paste0(range, collapse=":"), " Da)"),
       xlim=range, ylim=c(0, 2e-3), type="n")

  color <- rainbow(length(unwarped))

  for (i in seq(along=unwarped)) {
    lines(unwarped[[i]], col=color[i])
  }

  plot(unwarped[[1]], main=paste0("warped spectra (mass ",
                                 paste0(range, collapse=":"), " Da)"),
       xlim=range, ylim=c(0, 2e-3), type="n")

  for (i in seq(along=warped)) {
    lines(warped[[i]], col=color[i])
  }
}

plotSpectra(spectra, warpedSpectra, c(4180, 4240))
plotSpectra(spectra, warpedSpectra, c(9200, 9400))

par(mfrow=c(1, 1))
