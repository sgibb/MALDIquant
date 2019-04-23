## MassSpectrum
setMethod(f="totalIonCurrent",
          signature=signature(object="AbstractMassSpectrum"),
          definition=function(object) {

  tmpIntensity <- intensity(object)
  
  left <- as.double(head(tmpIntensity, -1L))
  right <- as.double(tail(tmpIntensity, -1L))

  as.double(sum((left + right) / 2L * diff(mass(object)), na.rm=TRUE))
})
