## .replaceNegativeIntensityValues
##  replace negative intensity valus by zeros
##
## params:
##  object AbstractMass Object
##  warn: throw a warning?
##
## returns:
##  a AbstractMass object
##
.replaceNegativeIntensityValues <- function(object, warn=TRUE) {
  if (any(object@intensity < 0L, na.rm=TRUE) && !isEmpty(object)) {
    if (warn) {
      warning("Negative intensity values are replaced by zeros.")
    }
    object@intensity[which(object@intensity < 0L)] <- 0L
  }
  object
}
