## .overlap
## returns largest overlapping mass range of a list of AbstractMassObject
## objects.
##
## params:
## l: list of AbstractMassObject objects
##
## returns:
## double, minimal and maximal mass
##
.overlap <- function(l) {
  ## test argument
  .stopIfNotIsMassObjectList(l)

  ## mass values are already sorted
  leftMass <- .unlist(lapply(l, function(x)x@mass[1L]))
  rightMass <- .unlist(lapply(l, function(x)x@mass[length(x@mass)]))

  if (length(rightMass)) {
    r <- c(max(leftMass, na.rm=TRUE), min(rightMass, na.rm=TRUE))
    if (r[1L] < r[2L]) {
      return(r)
    }
  }

  ## no overlap
  c(0L, 0L)
}

## .reorderRange
## swap range values if needed
##
## params:
## x: range values
##
## returns:
## corrected range values
##
.reorderRange <- function(x) {
  ## sort range
  if (x[1L] > x[2L]) {
    x <- x[2L:1L]
  }

  x
}
