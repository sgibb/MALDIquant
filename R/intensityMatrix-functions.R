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

## intensityMatrix
##  converts a list of MassPeaks into an expression matrix
##
## params:
##  peaks: list of MassPeaks objects
##  spectra: list of MassSpectrum objects
##  tolerance: binning tolerance
##
## returns:
##  a matrix
##
intensityMatrix <- function(peaks, spectra, tolerance=0.002) {

  ## deprecated for MassSpectrum objects
  if (isMassSpectrumList(peaks)) {
    .deprecated("1.8.4", "\"intensityMatrix\" is deprecated ",
                "for lists of MassSpectrum objects.")
    return(.intensityMatrixDeprecated(peaks))
  }

  ## test arguments
  .stopIfNotIsMassPeaksList(peaks)

  peaks <- binPeaks(peaks, tolerance=tolerance)

  mass <- sort(x=.unlist(lapply(peaks, function(x)x@mass)), method="quick")
  uniqueMass <- unique(mass)

  ## build matrix
  m <- do.call(rbind, lapply(peaks, function(x) {
    return(x@intensity[match(x=uniqueMass, table=x@mass, nomatch=NA)])}))

  ## set column names
  dimnames(m) <- list(NULL, c(uniqueMass))

  ## lookup corresponding intensity values in spectra for missing peaks
  if (!missing(spectra)) {
    .stopIfNotIsMassSpectrumList(spectra)

    if (length(peaks) != length(spectra)) {
      stop("Incompatible number of spectra!")
    }

    isNa <- is.na(m)

    approxSpectra <- lapply(spectra, approxfun, yleft=0L, yright=0L)

    for (i in seq(along=approxSpectra)) {
      m[i, isNa[i, ]] <- approxSpectra[[i]](uniqueMass[isNa[i, ]])
    }
  }

  return(m)
}
