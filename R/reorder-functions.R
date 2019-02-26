## .reorder
##  reorders mass and intensity values (increasing)
##
## params:
##  object AbstractMass Object
##  warn: throw a warning?
##
## returns:
##  an AbstractMass object
##
.reorder <- function(object, warn=TRUE) {
       tmpMass <- mass(object)
  if (is.unsorted(tmpMass)) {
    if (warn) {
      warning("Mass and intensity values are reordered.")
    }
    i <- sort.int(tmpMass, index.return=TRUE)
    mass(object) <- i$x
    intensity(object) <- intensity(object)[i$ix]
  }
  object
}
