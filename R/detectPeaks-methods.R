## Copyright 2011-2013 Sebastian Gibb
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
                              method=c("MAD", "SuperSmoother"), SNR=2L,
                              fun, ## deprecated
                              ...) {

  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(createMassPeaks(mass=object@mass, intensity=object@intensity,
                           metaData=object@metaData))
  }

  ## try to use user-defined noise estimation function
  if (!missing(fun)) {
    .deprecatedArgument("1.7.12", old="fun", new="method", help="detectPeaks")
    fun <- match.fun(fun)
    noise <- fun(object@mass, object@intensity, ...)

    ## wrong noise argument given?
    isCorrectNoise <- is.matrix(noise) &&
                      (nrow(noise) == length(object) && ncol(noise) == 2)

    if (!isCorrectNoise) {
      stop("The noise argument is not valid.")
    }
  } else {
    ## estimate noise
    noise <- estimateNoise(object, method=method, ...)
  }

  ## find local maxima
  localMaxima <- .findLocalMaximaLogical(object, halfWindowSize=halfWindowSize)

  ## include only local maxima which are above the noise
  aboveNoise <- object@intensity > (SNR * noise[, 2L])

  isPeak <- aboveNoise & localMaxima

  return(createMassPeaks(mass=object@mass[isPeak],
                         intensity=object@intensity[isPeak],
                         snr=object@intensity[isPeak]/noise[isPeak, 2L],
                         metaData=object@metaData))
})

## list
setMethod(f="detectPeaks",
          signature=signature(object="list"),
          definition=function(object, ...) {

  ## test arguments
  .stopIfNotIsMassSpectrumList(object)

  return(lapply(object, detectPeaks, ...))
})

