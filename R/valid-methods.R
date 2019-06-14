## AbstractMassObject
.validAbstractMassObject <- function(object) {
  if (length(object@mass) != length(object@intensity)) {
    return(paste0("Lengths of mass (", length(object@mass),
                  ") and intensity (", length(object@intensity),
                  ") have to be equal."))
  }
  m <- mass(object)
  i <- intensity(object)
  if (is.numeric(m) && length(object) && any(m < 0L)) {
    warning("Negative mass values found.")
  }
  if (is.unsorted(m)) {
    warning("Unsorted mass values found.")
  }
  if (is.numeric(i) && !isEmpty(object) && any(i < 0L)) {
    warning("Negative intensity values found.")
  }
  TRUE
}

setValidity("AbstractMassObject", method=.validAbstractMassObject)

.validMassSpectrum <- function(object) {
}

setValidity("MassSpectrum", method=.validMassSpectrum)

.validMassSpectrumOnDisk <- function(object) {
}

setValidity("MassSpectrumOnDisk", method=.validMassSpectrumOnDisk)

.validMassPeaks <- function(object) {
  if (length(object@intensity) != length(object@snr)) {
    return(paste0("Lengths of intensity (", length(object@intensity),
                  ") and snr (", length(object@snr),
                  ") have to be equal."))
  }
  TRUE
}

setValidity("MassPeaks", method=.validMassPeaks)
