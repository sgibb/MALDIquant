## Copyright 2013 Sebastian Gibb
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

## expressionMatrix
##  converts list of MassPeaks (MassSpectrum) objects to a matrix
##  missing values are interpolated from original spectra
##  (if missing it returns a presence/absence matrix).
##
## params:
##  peaks: list of MassPeaks objects
##  spectra: list of MassSpectrum objects (if missing only presence/absence)
##
## returns:
##  a matrix
##
expressionMatrix <- function(peaks, spectra) {
  .stopIfNotIsMassPeaksList(peaks)

  l <- .intensityMatrix(peaks)
  isNa <- is.na(l$intensityMatrix)

  if (missing(spectra)) {
    return(ifelse(isNa, 0L, 1L))
  } else {
    .stopIfNotIsMassSpectrumList(spectra)
    if (length(peaks) != length(spectra)) {
      stop("Incompatible number of spectra!")
    }
    approxSpectra <- lapply(spectra, approxfun, yleft=0L, yright=0L)
    for (i in seq(along=approxSpectra)) {
      l$intensityMatrix[i, isNa[i, ]] <-
        approxSpectra[[i]](l$uniqueMass[isNa[i, ]])
    }
    return(l$intensityMatrix)
  }
}

