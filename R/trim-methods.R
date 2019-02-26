## AbstractMassObject
setMethod("trim",
          signature=signature(object="AbstractMassObject", range="numeric"),
          definition=function(object, range) {
  if (length(range) != 2L) {
    stop(sQuote("range"), " has to be a vector of length 2.")
  }

  range <- .reorderRange(range)

  sel <- which(findInterval(mass(object), range, rightmost.closed=TRUE) == 1L)

  if (!length(sel)) {
    warning("The mass range (", paste0(range, collapse=":"),
            ") is outside of the stored mass values. No data points left.")
  }

  object[sel]
})

## MassSpectrumOnDisk

## In the case of MassSpectrumOnDisk objects subsetting/trimming the original on-disk data in not possible. In
## this situation a new subsetted/trimmed MassSpectrum object is returned

setMethod("trim",
          signature=signature(object="MassSpectrumOnDisk", range="numeric"),
          definition=function(object, range) {
                 if (length(range) != 2L) {
                        stop(sQuote("range"), " has to be a vector of length 2.")
                 }
                 
                 range <- .reorderRange(range)
                 
                 sel <- which(findInterval(mass(object), range, rightmost.closed=TRUE) == 1L)
                 
                 if (!length(sel)) {
                        warning("The mass range (", paste0(range, collapse=":"),
                                ") is outside of the stored mass values. No data points left.")
                 }
                 
                 warning("For the supplied MassSpectrumOnDisk object, trimming the original on-disk data ",
                         "is not possible. In this situation a new trimmed MassSpectrum object is returned.\n")
                 
              
                 
                 createMassSpectrum(mass=mass(object)[sel], 
                                    intensity=intensity(object)[sel], 
                                    metaData=object@metaData)
                 
                 
          })

## list
setMethod("trim",
          signature=signature(object="list", range="numeric"),
          definition=function(object, range, ...) {
  .stopIfNotIsMassObjectList(object)
  .lapply(X=object, FUN=trim, range=range, ...)
})

setMethod("trim",
          signature=signature(object="list", range="missing"),
          definition=function(object, ...) {
  .stopIfNotIsMassObjectList(object)
  range <- .overlap(object)

  if (all(range == 0L)) {
    stop("No overlap found!")
  }

  .lapply(X=object, FUN=trim, range=range, ...)
})
