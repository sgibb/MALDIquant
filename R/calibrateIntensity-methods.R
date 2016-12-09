## MassSpectrum
setMethod(f="calibrateIntensity",
          signature=signature(object="MassSpectrum"),
          definition=function(object,
                              method=c("TIC", "PQN", "median"),
                              range, ...) {

  method <- match.arg(method)

  switch(method,
    "TIC" = ,
    "median" = {
      .transformIntensity(object, fun=.calibrateIntensitySimple,
                          offset=0L,
                          scaling=.scalingFactor(object, method=method,
                                                 range=range))
    },
    "PQN" = {
      stop(dQuote("PQN"),
           " is not supported for a single MassSpectrum object.")
    })
})

## list
setMethod(f="calibrateIntensity",
          signature=signature(object="list"),
          definition=function(object,
                              method=c("TIC", "PQN", "median"), range, ...) {

  ## test arguments
  .stopIfNotIsMassSpectrumList(object)

  method <- match.arg(method)

  switch(method,
    "TIC" = ,
    "median" = {
      lapply(object, calibrateIntensity, method=method, range=range, ...)
    },
    "PQN" = {
      .calibrateProbabilisticQuotientNormalization(object, range=range)
    }
  )
})
