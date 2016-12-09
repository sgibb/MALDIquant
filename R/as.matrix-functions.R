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

  mass <- .unlist(lapply(l, function(x)x@mass))
  intensity <- .unlist(lapply(l, function(x)x@intensity))
  uniqueMass <- sort.int(unique(mass), method="quick")
  n <- lengths(l)
  r <- rep.int(seq_along(l), n)

  i <- findInterval(mass, uniqueMass)

  m <- matrix(NA_real_, nrow=length(l), ncol=length(uniqueMass),
              dimnames=list(NULL, uniqueMass))
  m[cbind(r, i)] <- intensity
  attr(m, "mass") <- uniqueMass
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
  stopifnot(is.matrix(m))
  isNA <- which(is.na(m))
  m[] <- 1L
  m[isNA] <- 0L
  mode(m) <- "integer"
  m
}
