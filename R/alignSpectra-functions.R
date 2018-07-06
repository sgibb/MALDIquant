## alignSpectra
##  wrapper around detectPeaks, determineWarpingFunctions and warpMassSpectra
##
## params:
##  spectra: list, list of MassSpectrum objects
##  halfWindowSize: numeric, half window size.
##  noiseMethod: character, noise estimation method
##  SNR: double, signal-to-noise ratio
##  reference: MassPeaks, a reference MassPeaks object to which all other
##             MassPeaks objects should be aligned
##  tolerance: double, maximal deviation of a peak position to be
##             considered as same peak
##  warpingMethod: choose type of base warping function
##  allowNoMatches: logical, don't throw an error if a single MassPeaks object
##                  could not match to the reference.
##  emptyNoMatches: logical, if TRUE mismatches (warping function NA)
##
## returns:
##  a list of aligned MassSpectrum objects
##
alignSpectra <- function(spectra,
                         ## peak detection
                         halfWindowSize=20, noiseMethod="MAD", SNR=2,
                         ## warping
                         reference, tolerance=0.002, warpingMethod="lowess",
                         allowNoMatches=FALSE, emptyNoMatches=FALSE, ...) {

  ## test arguments
  .stopIfNotIsMassSpectrumList(spectra)

  peaks <- detectPeaks(spectra, halfWindowSize=halfWindowSize,
                       method=noiseMethod, SNR=SNR, ...)
  wf <- determineWarpingFunctions(peaks, reference=reference,
                                  tolerance=tolerance, method=warpingMethod,
                                  allowNoMatches=allowNoMatches)
  warpMassSpectra(spectra, wf, emptyNoMatches=emptyNoMatches)
}
