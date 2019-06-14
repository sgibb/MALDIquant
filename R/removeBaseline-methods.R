## MassSpectrum
setMethod(f="removeBaseline",
          signature=signature(object="MassSpectra"),
          definition=function(object,
                              method=c("SNIP", "TopHat", "ConvexHull",
                                       "median"),
                              ...) {
  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(object)
  }

  i <- intensity(object)

  ## estimate baseline
  baseline <- .estimateBaseline(x=mass(object), y=i, method=method, ...)

  ## substract baseline
  object@intensity[] <- i - baseline

  object
})

## list
setMethod(f="removeBaseline",
          signature=signature(object="list"),
          definition=function(object, ...) {

  ## test arguments
  .stopIfNotIsMassSpectraList(object)

  .mapply(removeBaseline, object, ...)
})
