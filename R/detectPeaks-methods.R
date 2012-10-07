## Copyright 2011-2012 Sebastian Gibb
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
    definition=function(object, 
                        halfWindowSize=20, fun,  SNR=2,
                        ...) {

    ## empty spectrum?
    if (.isEmptyWarning(object)) {
        return(createMassPeaks(mass=object@mass,
                               intensity=object@intensity,
                               metaData=object@metaData));
    }

    ## try to use user-defined noise estimation function
    if (!missing(fun)) {
        fun <- match.fun(fun);
        noise <- fun(object@mass, object@intensity, ...);
    } else {
        noise <- estimateNoise(object);
    }

    localMaxima <- .findLocalMaxima(object=object,
                                    halfWindowSize=halfWindowSize);

    ## wrong noise argument given?
    isCorrectNoise <- (is.matrix(noise) || is.numeric(noise)) &&
                      ((nrow(noise) == length(object) && ncol(noise) == 2) ||
                      (length(noise) == 1));

    if (!isCorrectNoise) {
        stop("The noise argument is not valid.");
    }

    ## include only local maxima which are above the noise
    if (is.matrix(noise)) {
        noiseIndex <- noise[, 1] %in% localMaxima[, 1];
        peakIndex <- localMaxima[, 2] > (SNR * noise[noiseIndex, 2]);
    } else {
        peakIndex <- localMaxima[, 2] > (SNR * noise);
    }
    
    return(createMassPeaks(mass=localMaxima[peakIndex, 1],
                                intensity=localMaxima[peakIndex, 2],
                                metaData=object@metaData));
});

## list
setMethod(f="detectPeaks",
    signature=signature(object="list"),
    definition=function(object, ...) {

    ## test arguments
    .stopIfNotMassSpectrumList(object);

    return(lapply(object, detectPeaks, ...));
});

