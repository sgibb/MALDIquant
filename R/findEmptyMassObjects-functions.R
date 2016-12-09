## findEmptyMassObjects
##  find empty AbstractMassObject objects in a MassObjectsList
##
## params:
##  l: list of AbstractMassObject objects
##
## returns:
##  a vector of indices refer to empty AbstractMassObject objects
##
findEmptyMassObjects <- function(l) {

  .stopIfNotIsMassObjectList(l)

  isEmpty <- vapply(l, isEmpty, logical(1L))

  which(isEmpty)
}
