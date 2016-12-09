## AbstractMassObject
setMethod(f="mz",
          signature=signature(object="AbstractMassObject"),
          definition=function(object, ...) {

  mass(object)
})

## AbstractMassObject
setReplaceMethod(f="mz",
          signature=signature(object="AbstractMassObject", value="numeric"),
          definition=function(object, value) {

  mass(object) <- value
  object
})
