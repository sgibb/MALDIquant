## warpMassSpectra
##  warp MassSpectrum objects
##
## params:
##  l: list of MassSpectrum objects
##  w: list of warping functions determined by determineWarpingFunctions
##
## returns:
##  a list of warped MassSpectrum objects
##
warpMassSpectra <- function(l, w) {
  .stopIfNotIsMassSpectrumList(l)
  .stopIfNotIsFunctionList(w)

  .warp(l, w)
}

## warpMassPeaks
##  warp MassPeaks objects
##
## params:
##  l: list of MassPeaks objects
##  w: list of warping functions determined by determineWarpingFunctions
##
## returns:
##  a list of warped MassPeaks objects
##
warpMassPeaks <- function(l, w) {
  .stopIfNotIsMassPeaksList(l)
  .stopIfNotIsFunctionList(w)

  .warp(l, w)
}


## .warp
##  .warp bstractMassObject objects
##
## params:
##  l: list of AbstractMassObject objects
##  w: list of warping functions determined by determineWarpingFunctions
##
## returns:
##  a list of warped AbstractMassObject objects
##
.warp <- function(l, w) {
  .mapply(function(m, wf) {
           m@mass <- m@mass + wf(m@mass)
           m
  }, m=l, wf=w)
}
