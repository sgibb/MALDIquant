## MassSpectrum
setMethod(f="removeBaseline",
          signature=signature(object="MassSpectrum"),
          definition=function(object,
                              method=c("SNIP", "TopHat", "ConvexHull",
                                       "median"),
                              ...) {
  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(object)
  }

  ## estimate baseline
  baseline <- .estimateBaseline(x=object@mass, y=object@intensity,
                                method=method, ...)

  ## substract baseline
  object@intensity <- object@intensity - baseline

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
