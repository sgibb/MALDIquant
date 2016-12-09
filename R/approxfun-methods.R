## MassSpectrum
setMethod(f="approxfun",
          signature=signature(x="MassSpectrum"),
          definition=function(x, y=NULL, method="linear", yleft, yright,
                              rule=1L,  f=0L, ties=mean) {
  if (isEmpty(x)) {
    function(x)rep.int(NA, length(x))
  } else {
    approxfun(x=x@mass, y=x@intensity, method=method,
              yleft=yleft, yright=yright, rule=rule, f=f, ties=ties)
  }
})
