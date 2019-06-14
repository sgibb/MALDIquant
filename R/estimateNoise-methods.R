## MassSpectrum
setMethod(f="estimateNoise",
          signature=signature(object="MassSpectrum"),
          definition=function(object, method=c("MAD", "SuperSmoother"),
                              ...) {
  if (.isEmptyWarning(object)) {
    return(0L)
  }

  m <- mass(object)
  cbind(mass=m, intensity=.estimateNoise(x=m, y=intensity(object),
                                         method=method, ...))
})
