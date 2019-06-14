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
  i <- intensity(object)
  if (any(i < 0L, na.rm=TRUE) && !isEmpty(object)) {
    if (warn) {
      warning("Negative intensity values are replaced by zeros.")
    }
    i[i < 0L] <- 0L
    object@intensity[] <- i
  }
  object
}
