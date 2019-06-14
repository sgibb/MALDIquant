## MassSpectrum
setMethod(f="totalIonCurrent",
          signature=signature(object="MassSpectra"),
          definition=function(object) {

  i <- intensity(object)
  left <- as.double(head(i, -1L))
  right <- as.double(tail(i, -1L))

  as.double(sum((left + right) / 2L * diff(mass(object)), na.rm=TRUE))
})
