## AbstractMassObject
setMethod(f="lines",
    signature=signature(x="AbstractMassObject"),
    definition=function(x,
        type=ifelse(isMassPeaks(x), "h", "l"),
        ...) {

    lines(x=x@mass, y=x@intensity, type, ...)
})
