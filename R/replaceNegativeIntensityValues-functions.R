## .replaceNegativeIntensityValues
##  replaces negative intensity valus by zeros
##
## params:
##  object AbstractMass Object
##  warn: throw a warning?
##
## returns:
##  an AbstractMass object
##
.replaceNegativeIntensityValues <- function(object, warn=TRUE) {
       
  tmpIntensity <- intensity(object)
  
  if (any(tmpIntensity < 0L, na.rm=TRUE) && !isEmpty(object)) {
    if (warn) {
      warning("Negative intensity values are replaced by zeros.")
    }
  intensity(object)[which(tmpIntensity < 0L)] <- 0L
  }
  object
}
