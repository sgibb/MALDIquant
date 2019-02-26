## MassSpectrum
setMethod(f="isRegular",
          signature=signature(object="MassSpectrum"),
          definition=function(object, threshold=1e-3) {

  s <- .irregularScore(object@mass) <= threshold
  !is.na(s) & s
})

## MassSpectrumOnDisk
setMethod(f="isRegular",
          signature=signature(object="MassSpectrumOnDisk"),
          definition=function(object, threshold=1e-3) {
                 
                 s <- .irregularScore(mass(object)) <= threshold
                 !is.na(s) & s
          })
