## MassSpectrum
setMethod(f="estimateBaseline",
          signature=signature(object="MassSpectrum"),
          definition=function(object, method=c("SNIP", "TopHat", "ConvexHull",
                                               "median"),
                              ...) {
  if (.isEmptyWarning(object)) {
    return(NA)
  }

  cbind(mass=object@mass,
        intensity=.estimateBaseline(x=object@mass, y=object@intensity,
                                    method=method, ...))
})

## MassSpectrumOnDisk
setMethod(f="estimateBaseline",
          signature=signature(object="MassSpectrumOnDisk"),
          definition=function(object, method=c("SNIP", "TopHat", "ConvexHull",
                                               "median"),
                              ...) {
                 if (.isEmptyWarning(object)) {
                        return(NA)
                 }
                 
                 cbind(mass=mass(object),
                       intensity=.estimateBaseline(x=mass(object), y=intensity(object),
                                                   method=method, ...))
          })
