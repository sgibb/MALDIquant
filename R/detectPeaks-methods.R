## MassSpectrum
setMethod(f="detectPeaks",
          signature=signature(object="AbstractMassSpectrum"),
          definition=function(object, halfWindowSize=20L,
                              method=c("MAD", "SuperSmoother"), SNR=2L, ...) {

  tmpMass <- mass(object)
  tmpIntensity <- intensity(object)
  
  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(createMassPeaks(mass=tmpMass, intensity=tmpIntensity,
                           metaData=object@metaData))
  }

  ## estimate noise
  noise <- .estimateNoise(x=tmpMass, y=tmpIntensity, method=method, ...)

  ## find local maxima
  isLocalMaxima <- .findLocalMaximaLogical(object,
                                           halfWindowSize=halfWindowSize)

  ## include only local maxima which are above the noise
  isAboveNoise <- tmpIntensity > (SNR * noise)

  peakIdx <- which(isAboveNoise & isLocalMaxima)

  createMassPeaks(mass=tmpMass[peakIdx],
                  intensity=tmpIntensity[peakIdx],
                  snr=tmpIntensity[peakIdx] / noise[peakIdx],
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
