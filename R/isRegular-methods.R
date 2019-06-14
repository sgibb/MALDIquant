## MassSpectrum
setMethod(f="isRegular",
          signature=signature(object="MassSpectra"),
          definition=function(object, threshold=1e-3) {

  s <- .irregularScore(mass(object)) <= threshold
  !is.na(s) & s
})
