## MassSpectrum
setMethod(f="estimateNoise",
          signature=signature(object="AbstractMassSpectrum"),
          definition=function(object, method=c("MAD", "SuperSmoother"),
                              ...) {
  if (.isEmptyWarning(object)) {
    return(0L)
  }

  cbind(mass=mass(object),
        intensity=.estimateNoise(x=mass(object), y=intensity(object),
                                 method=method, ...))
})
