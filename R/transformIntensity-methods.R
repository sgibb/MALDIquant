## AbstractMassObject
setMethod(f="transformIntensity",
          signature=signature(object="AbstractMassObject"),
          definition=function(object,
                              method=c("sqrt", "log", "log2", "log10")) {

  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(object)
  }

  method <- match.arg(method)

  fun <- switch(method,
         "sqrt" = {
           sqrt
         },
         "log" = {
           log
         },
         "log2" = {
           log2
         },
         "log10" = {
           log10
         }
  )

  .transformIntensity(object, fun=fun)
})

## AbstractMassObject
setMethod(f=".transformIntensity",
          signature=signature(object="AbstractMassObject"),
          definition=function(object, fun, na.rm=TRUE, ...) {

  if (!isEmpty(object)) {
    fun <- match.fun(fun)
    
    
    tmpIntensity <- fun(intensity(object), ...)

    if (na.rm) { # Attention!
      naIdx <- which(!is.na(tmpIntensity))
      tmpIntensity <- tmpIntensity[naIdx]
      mass(object) <- mass(object)[naIdx]
    }

    intensity(object) <- tmpIntensity
    object <- .replaceNegativeIntensityValues(object)
  }

  object
})

## list
setMethod(f="transformIntensity",
          signature=signature(object="list"),
          definition=function(object, ...) {


  ## test arguments
  .stopIfNotIsMassObjectList(object)

  .lapply(object, transformIntensity, ...)
})

## list
setMethod(f=".transformIntensity",
          signature=signature(object="list"),
          definition=function(object, ...) {

  ## test arguments
  .stopIfNotIsMassObjectList(object)

  .lapply(object, .transformIntensity, ...)
})
