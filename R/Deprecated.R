## deprecated since MALDIquant 1.8.4
## intensityMatrix for MassSpectrum objects
## TODO: remove corresponding code in intensityMatrix
.intensityMatrixDeprecated <- function(l) {
  mass <- sort(x=.unlist(lapply(l, function(x)x@mass)), method="quick")
  uniqueMass <- unique(mass)

  ## build matrix
  m <- do.call(rbind, lapply(l, function(x) {
    return(x@intensity[match(x=uniqueMass, table=x@mass, nomatch=NA)])}))

  ## set column names
  dimnames(m) <- list(NULL, c(uniqueMass))

  return(m)
}
