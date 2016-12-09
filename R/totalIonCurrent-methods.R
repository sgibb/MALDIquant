## MassSpectrum
setMethod(f="totalIonCurrent",
          signature=signature(object="MassSpectrum"),
          definition=function(object) {

  left <- as.double(head(object@intensity, -1L))
  right <- as.double(tail(object@intensity, -1L))

  as.double(sum((left + right) / 2L * diff(object@mass), na.rm=TRUE))
})
