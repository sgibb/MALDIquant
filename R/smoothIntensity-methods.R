## AbstractMassObject
setMethod(f="smoothIntensity",
          signature=signature(object="MassSpectrum"),
          definition=function(object,
                              method=c("SavitzkyGolay", "MovingAverage"),
                              halfWindowSize, ...) {
  ## empty spectrum?
  if (.isEmptyWarning(object)) {
    return(object)
  }

  method <- match.arg(method)

  fun <- switch(method,
                "SavitzkyGolay" = {
                  if (missing(halfWindowSize)) {
                    halfWindowSize <- 10L
                  }
                  .savitzkyGolay
                },
                "MovingAverage" = {
                  if (missing(halfWindowSize)) {
                    halfWindowSize <- 2L
                  }
                  .movingAverage
                }
  )

  .transformIntensity(object, fun=fun, halfWindowSize=halfWindowSize, ...)
})

## list
setMethod(f="smoothIntensity",
          signature=signature(object="list"),
          definition=function(object, ...) {

  ## test arguments
  .stopIfNotIsMassSpectrumList(object)

  .mapply(smoothIntensity, object, ...)
})
