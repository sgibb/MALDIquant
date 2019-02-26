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

    intensity(object) <- fun(intensity(object), ...)

    if (na.rm) {
      naIdx <- which(!is.na(intensity(object)))
      intensity(object) <- intensity(object)[naIdx]
      mass(object) <- mass(object)[naIdx]
    }

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
