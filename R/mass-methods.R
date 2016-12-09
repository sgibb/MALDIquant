## AbstractMassObject
setMethod(f="mass",
          signature=signature(object="AbstractMassObject"),
          definition=function(object, ...) {

  object@mass
})

## AbstractMassObject
setReplaceMethod(f="mass",
          signature=signature(object="AbstractMassObject", value="numeric"),
          definition=function(object, value) {

  if (length(object@mass) == length(value)) {
    object@mass <- as.double(value)
  } else {
    stop("Lengths of mass (", length(object@mass), ") and value (",
         length(value), ") have to be equal.")
  }
  object
})
