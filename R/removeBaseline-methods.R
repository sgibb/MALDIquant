## MassSpectrum
setMethod(f="removeBaseline",
          signature=signature(object="AbstractMassSpectrum"),
          definition=function(object,
                              method=c("SNIP", "TopHat", "ConvexHull",
                                       "median"),
                              ...) {
  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(object)
  }

  tmpMass <- mass(object)
  tmpIntensity <- intensity(object)
                 
  ## estimate baseline
  baseline <- .estimateBaseline(x=tmpMass, y=tmpIntensity,
                                method=method, ...)

  ## substract baseline
  intensity(object) <- tmpIntensity - baseline

  object
})

## list
setMethod(f="removeBaseline",
          signature=signature(object="list"),
          definition=function(object, ...) {

  ## test arguments
  .stopIfNotIsMassSpectrumList(object)

  .mapply(removeBaseline, object, ...)
})
