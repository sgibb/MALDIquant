## removeEmptyMassObjects
##  find and remove empty AbstractMassObject objects in a MassObjectsList
##
## params:
##  l: list of AbstractMassObject objects
##
## returns:
##  a list without empty objects
##
removeEmptyMassObjects <- function(l) {

  .stopIfNotIsMassObjectList(l)

  ## find empty MassPeaks objects
  notEmpty <- !(seq_along(l) %in% findEmptyMassObjects(l))

  ## exclude empty elements
  l[notEmpty]
}
