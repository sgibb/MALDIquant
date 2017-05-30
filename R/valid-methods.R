## AbstractMassObject
.validAbstractMassObject <- function(object) {
  if (length(object@mass) != length(object@intensity)) {
    return(paste0("Lengths of mass (", length(object@mass),
                  ") and intensity (", length(object@intensity),
                  ") have to be equal."))
  }
  if (is.numeric(object@mass) &&
      length(object@mass) &&
      any(object@mass < 0L)) {
    warning("Negative mass values found.")
  }
  if (is.numeric(object@intensity) &&
      !isEmpty(object) &&
      any(object@intensity < 0L)) {
    warning("Negative intensity values found.")
  }
  if (is.unsorted(object@mass)) {
    warning("Unsorted mass values found.")
  }
  TRUE
}

setValidity("AbstractMassObject", method=.validAbstractMassObject)

.validMassPeaks <- function(object) {
  if (length(object@intensity) != length(object@snr)) {
    return(paste0("Lengths of intensity (", length(object@intensity),
                  ") and snr (", length(object@snr),
                  ") have to be equal."))
  }
  .validAbstractMassObject(object)
}

setValidity("MassPeaks", method=.validMassPeaks)
