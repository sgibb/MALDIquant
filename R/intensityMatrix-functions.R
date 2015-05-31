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

## intensityMatrix
##  converts a list of MassPeaks into an expression matrix
##
## params:
##  peaks: list of MassPeaks objects
##  spectra: list of MassSpectrum objects
##
## returns:
##  a matrix
##
intensityMatrix <- function(peaks, spectra) {

  ## test arguments
  .stopIfNotIsMassPeaksList(peaks)

  m <- .as.matrix.MassObjectList(peaks)

  ## lookup corresponding intensity values in spectra for missing peaks
  if (!missing(spectra)) {
    .stopIfNotIsMassSpectrumList(spectra)

    if (length(peaks) != length(spectra)) {
      stop("Incompatible number of spectra!")
    }

    isNa <- is.na(m)
    uniqueMass <- as.double(colnames(m))

    approxSpectra <- lapply(spectra, approxfun, yleft=0L, yright=0L)

    for (i in seq_along(approxSpectra)) {
      m[i, isNa[i, ]] <- approxSpectra[[i]](uniqueMass[isNa[i, ]])
    }
  }

  m
}
