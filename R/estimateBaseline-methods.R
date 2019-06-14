## MassSpectrum
setMethod(f="estimateBaseline",
          signature=signature(object="MassSpectrum"),
          definition=function(object, method=c("SNIP", "TopHat", "ConvexHull",
                                               "median"),
                              ...) {
  if (.isEmptyWarning(object)) {
    return(NA)
  }

  m <- mass(object)
  cbind(mass=m, intensity=.estimateBaseline(x=m, y=intensity(object),
                                            method=method, ...))
})
