## AbstractMassObject
setMethod(f="lines",
    signature=signature(x="AbstractMassObject"),
    definition=function(x,
        type=ifelse(isMassPeaks(x), "h", "l"),
        ...) {

    lines(x=mass(x), y=intensity(x), type, ...)
})
