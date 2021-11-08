## .as.matrix.MassObjectList
##  internal function to convert a list of AbstractMassObject objects into a
##  matrix
##
## params:
##  l: list of AbstractMassObject objects
##
## returns:
##  a matrix
.as.matrix.MassObjectList <- function(l) {
    .stopIfNotIsMassObjectList(l)

    intensity <- .unlist(lapply(l, function(x)x@intensity))
    o <- .as.occurrence.list.MassObjectList(l)

    m <- matrix(
        NA_real_, nrow=length(l), ncol=length(o$mass),
        dimnames=list(NULL, o$mass)
    )
    m[cbind(o$sample, o$i)] <- intensity
    attr(m, "mass") <- o$mass
    m
}

## .as.binary.matrix
##  internal function to convert a matrix with NA to a binary one
##
## params:
##  m: matrix
##
## returns:
##  a binary matrix
.as.binary.matrix <- function(m) {
    if (!is.matrix(m))
        stop("'x' has to be a matrix!")
    m[] <- !is.na(m)
    mode(m) <- "integer"
    m
}
