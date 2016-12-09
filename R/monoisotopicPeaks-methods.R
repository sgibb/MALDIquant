## MassPeaks
setMethod(f="monoisotopicPeaks",
          signature=signature(object="MassPeaks"),
          definition=function(object, minCor=0.95, tolerance=1e-4,
                              distance=1.00235, size=3L:10L) {
  object[.monoisotopic(x=mass(object), y=intensity(object),
                       minCor=minCor, tolerance=tolerance,
                       distance=distance, size=size)]
})

## list
setMethod(f="monoisotopicPeaks",
          signature=signature(object="list"),
          definition=function(object, ...) {

  ## test arguments
  .stopIfNotIsMassPeaksList(object)

  .lapply(object, monoisotopicPeaks, ...)
})
