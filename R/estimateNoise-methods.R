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

## MassSpectrumOnDisk
setMethod(f="estimateNoise",
          signature=signature(object="MassSpectrumOnDisk"),
          definition=function(object, method=c("MAD", "SuperSmoother"),
                              ...) {
                 if (.isEmptyWarning(object)) {
                        return(0L)
                 }
                 
                 cbind(mass=mass(object),
                       intensity=.estimateNoise(x=mass(object), y=intensity(object),
                                                method=method, ...))
          })