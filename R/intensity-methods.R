## AbstractMassObject
setMethod(f="intensity",
          signature=signature(object="AbstractMassObject"),
          definition=function(object, ...) {

  object@intensity
})

## AbstractMassObject
setReplaceMethod(f="intensity",
                 signature=signature(object="AbstractMassObject",
                                     value="numeric"),
                 definition=function(object, value) {

  if (length(object@intensity) == length(value)) {
    object@intensity <- as.double(value)
  } else {
    stop("Lengths of intensity(", length(object@intensity),
         ") and value (", length(value), ") have to be equal.")
  }
  object
})
