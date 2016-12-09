## MassSpectrum
setMethod(f="estimateNoise",
          signature=signature(object="MassSpectrum"),
          definition=function(object, method=c("MAD", "SuperSmoother"),
                              ...) {
  if (.isEmptyWarning(object)) {
    return(0L)
  }

  cbind(mass=object@mass,
        intensity=.estimateNoise(x=object@mass, y=object@intensity,
                                 method=method, ...))
})
