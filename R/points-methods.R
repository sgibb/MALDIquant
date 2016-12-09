## MassPeaks
setMethod(f="points",
          signature=signature(x="AbstractMassObject"),
          definition=function(x, ...) {

  points(x=x@mass, y=x@intensity, ...)
})
