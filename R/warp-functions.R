## warpMassSpectra
##  warp MassSpectrum objects
##
## params:
##  l: list of MassSpectrum objects
##  w: list of warping functions determined by determineWarpingFunctions
##  emptyNoMatches: logical, if TRUE mismatches (warping function NA)
##
## returns:
##  a list of warped MassSpectrum objects
##
warpMassSpectra <- function(l, w, emptyNoMatches=FALSE) {
  .stopIfNotIsMassSpectrumList(l)

  .warp(l, w, emptyNoMatches=emptyNoMatches)
}

## warpMassPeaks
##  warp MassPeaks objects
##
## params:
##  l: list of MassPeaks objects
##  w: list of warping functions determined by determineWarpingFunctions
##  emptyNoMatches: logical, if TRUE mismatches (warping function NA)
##
## returns:
##  a list of warped MassPeaks objects
##
warpMassPeaks <- function(l, w, emptyNoMatches=FALSE) {
  .stopIfNotIsMassPeaksList(l)

  .warp(l, w, emptyNoMatches=emptyNoMatches)
}


## .warp
##  .warp bstractMassObject objects
##
## params:
##  l: list of AbstractMassObject objects
##  w: list of warping functions determined by determineWarpingFunctions
##  emptyNoMatches: logical, if TRUE mismatches (warping function NA)
##
## returns:
##  a list of warped AbstractMassObject objects
##
.warp <- function(l, w, emptyNoMatches=FALSE) {
  notNa <- !is.na(w)
  wl <- w[notNa]
  ml <- l[notNa]

  if (length(wl)) {
    .stopIfNotIsFunctionList(wl)
  }

  l[notNa] <- .mapply(function(m, wf) {
           m@mass <- m@mass + wf(m@mass)
           m
  }, m=ml, wf=wl)
  if (emptyNoMatches) {
      l[!notNa] <- lapply(l[!notNa], function(m) { m@intensity[] <- 0; m })
  }
  l
}
