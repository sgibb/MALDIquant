## AbstractMassObject
setMethod("trim",
          signature=signature(object="AbstractMassObject", range="numeric"),
          definition=function(object, range) {
  if (length(range) != 2L) {
    stop(sQuote("range"), " has to be a vector of length 2.")
  }

  range <- .reorderRange(range)

  sel <- which(findInterval(object@mass, range, rightmost.closed=TRUE) == 1L)

  if (!length(sel)) {
    warning("The mass range (", paste0(range, collapse=":"),
            ") is outside of the stored mass values. No data points left.")
  }

  object[sel]
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
