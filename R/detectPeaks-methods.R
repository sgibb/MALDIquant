## MassSpectrum
setMethod(f="detectPeaks",
          signature=signature(object="MassSpectrum"),
          definition=function(object, halfWindowSize=20L,
                              method=c("MAD", "SuperSmoother"), SNR=2L, ...) {

  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(createMassPeaks(mass=object@mass, intensity=object@intensity,
                           metaData=object@metaData))
  }

  ## estimate noise
  noise <- .estimateNoise(x=object@mass, y=object@intensity, method=method, ...)

  ## find local maxima
  isLocalMaxima <- .findLocalMaximaLogical(object,
                                           halfWindowSize=halfWindowSize)

  ## include only local maxima which are above the noise
  isAboveNoise <- object@intensity > (SNR * noise)

  peakIdx <- which(isAboveNoise & isLocalMaxima)

  createMassPeaks(mass=object@mass[peakIdx],
                  intensity=object@intensity[peakIdx],
                  snr=object@intensity[peakIdx] / noise[peakIdx],
                  metaData=object@metaData)
})

## list
setMethod(f="detectPeaks",
          signature=signature(object="list"),
          definition=function(object, ...) {

  ## test arguments
  .stopIfNotIsMassSpectrumList(object)

  .mapply(detectPeaks, object, ...)
})
