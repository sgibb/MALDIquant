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
  if (is.unsorted(object@mass)) {
    if (warn) {
      warning("Mass and intensity values are reordered.")
    }
    i <- sort.int(object@mass, index.return=TRUE)
    object@mass <- i$x
    object@intensity <- object@intensity[i$ix]
  }
  object
}
