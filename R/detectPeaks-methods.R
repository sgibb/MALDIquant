## MassSpectrum
setMethod(f="detectPeaks",
          signature=signature(object="MassSpectra"),
          definition=function(object, halfWindowSize=20L,
                              method=c("MAD", "SuperSmoother"), SNR=2L, ...) {

  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(createMassPeaks(mass=mass(object), intensity=intensity(object),
                           metaData=object@metaData))
  }

  m <- mass(object)
  i <- intensity(object)

  ## estimate noise
  noise <- .estimateNoise(x=m, y=i, method=method, ...)

  ## find local maxima
  isLocalMaxima <- .findLocalMaximaLogical(object,
                                           halfWindowSize=halfWindowSize)

  ## include only local maxima which are above the noise
  isAboveNoise <- i > (SNR * noise)

  peakIdx <- which(isAboveNoise & isLocalMaxima)

  createMassPeaks(mass=m[peakIdx],
                  intensity=i[peakIdx],
                  snr=i[peakIdx] / noise[peakIdx],
                  metaData=object@metaData)
})

## list
setMethod(f="detectPeaks",
          signature=signature(object="list"),
          definition=function(object, ...) {

  ## test arguments
  .stopIfNotIsMassSpectraList(object)

  .mapply(detectPeaks, object, ...)
})
