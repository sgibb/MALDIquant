## MassSpectrum
setMethod(f=".findLocalMaxima",
          signature=signature(object="MassSpectrum"),
          definition=function(object, halfWindowSize=20L) {

  if (.isEmptyWarning(object)) {
    return(matrix(ncol=2L, dimnames=list(list(), list("mass", "intensity"))))
  }

  localMaxima <- .findLocalMaximaLogical(object, halfWindowSize=halfWindowSize)

  cbind(mass=object@mass, intensity=object@intensity)[localMaxima,]
})

setMethod(f=".findLocalMaximaLogical",
          signature=signature(object="MassSpectrum"),
          definition=function(object, halfWindowSize=20L) {

  if (.isEmptyWarning(object)) {
    return(logical())
  }

  .stopIfNotIsValidHalfWindowSize(halfWindowSize=halfWindowSize,
                                  n=length(object))

  .localMaxima(object@intensity, halfWindowSize=halfWindowSize)
})

## MassSpectrumOnDisk
setMethod(f=".findLocalMaxima",
          signature=signature(object="MassSpectrumOnDisk"),
          definition=function(object, halfWindowSize=20L) {
                 
                 if (.isEmptyWarning(object)) {
                        return(matrix(ncol=2L, dimnames=list(list(), list("mass", "intensity"))))
                 }
                 
                 localMaxima <- .findLocalMaximaLogical(object, halfWindowSize=halfWindowSize)
                 
                 cbind(mass=mass(object), intensity=intensity(object))[localMaxima,]
          })

setMethod(f=".findLocalMaximaLogical",
          signature=signature(object="MassSpectrumOnDisk"),
          definition=function(object, halfWindowSize=20L) {
                 
                 if (.isEmptyWarning(object)) {
                        return(logical())
                 }
                 
                 .stopIfNotIsValidHalfWindowSize(halfWindowSize=halfWindowSize,
                                                 n=length(object))
                 
                 .localMaxima(intensity(object), halfWindowSize=halfWindowSize)
          })
