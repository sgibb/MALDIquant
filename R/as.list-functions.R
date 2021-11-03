## .as.occurrence.list
##  internal function to create a list of peaks occurrence
##
## params:
##  l: list of AbstractMassObject objects
##
## returns:
##  a list, where sample is the sample id, i is the index of the uniqueMass,
##  and mass is the unique mass vector
.as.occurrence.list.MassObjectList <- function(l) {
    .stopIfNotIsMassObjectList(l)

    mass <- .unlist(lapply(l, function(x)x@mass))
    uniqueMass <- sort.int(unique(mass))
    n <- lengths(l)

    list(
        sample = rep.int(seq_along(l), lengths(l)),
        i = findInterval(mass, uniqueMass),
        mass = uniqueMass
    )
}
