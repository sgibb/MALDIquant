## MassPeaks
setMethod(f="points",
          signature=signature(x="AbstractMassObject"),
          definition=function(x, ...) {

  points(x=mass(x), y=intensity(x), ...)
})
