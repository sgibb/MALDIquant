## MassSpectrum
setMethod(f="estimateBaseline",
          signature=signature(object="AbstractMassSpectrum"),
          definition=function(object, method=c("SNIP", "TopHat", "ConvexHull",
                                               "median"),
                              ...) {
  if (.isEmptyWarning(object)) {
    return(NA)
  }

  cbind(mass=mass(object),
        intensity=.estimateBaseline(x=mass(object), y=intensity(object),
                                    method=method, ...))
})
