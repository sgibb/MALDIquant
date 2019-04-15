## AbstractMassObject
.validAbstractMassObject <- function(object) {
       
  tmpMass <- mass(object)
  tmpIntensity <- intensity(object)
       
  if (length(tmpMass) != length(tmpIntensity)) {
    return(paste0("Lengths of mass (", length(tmpMass),
                  ") and intensity (", length(tmpIntensity),
                  ") have to be equal."))
  }
  if (is.numeric(tmpMass) &&
      length(tmpMass) &&
      any(tmpMass < 0L)) {
    warning("Negative mass values found.")
  }
  if (is.numeric(tmpIntensity) &&
      !isEmpty(object) &&
      any(tmpIntensity < 0L)) {
    warning("Negative intensity values found.")
  }
  if (is.unsorted(tmpMass)) {
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
