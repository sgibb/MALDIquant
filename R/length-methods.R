## AbstractMassObject
setMethod(f="length",
          signature=signature(x="AbstractMassObject"),
          definition=function(x) {

  length(x@intensity)
})
