## Copyright 2011-2014 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This file is part of MALDIquant for R and related languages.
##
## MALDIquant is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## MALDIquant is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with MALDIquant. If not, see <http://www.gnu.org/licenses/>

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
