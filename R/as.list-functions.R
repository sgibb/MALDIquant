## .as.occurrence.list
##  internal function to create a list of peaks occurrence
##
## params:
##  l: list of AbstractMassObject objects
##
## returns:
##  a list
.as.occurrence.list <- function(l) {

  .stopIfNotIsMassObjectList(l)

  mass <- .unlist(lapply(l, function(x)x@mass))
  uniqueMass <- sort.int(unique(mass))
  n <- lengths(l)
  r <- rep.int(seq_along(l), n)

  i <- findInterval(mass, uniqueMass)

  return(list(r = as.integer(r), i = as.integer(i), masses = uniqueMass))

}
