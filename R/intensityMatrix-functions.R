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
