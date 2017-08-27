## AbstractMassObject
setMethod(f="coordinates",
          signature=signature(object="AbstractMassObject"),
          definition=function(object, ...) {

  object@metaData$imaging$pos
})

## AbstractMassObject
setReplaceMethod(f="coordinates",
                 signature=signature(object="AbstractMassObject",
                                     value="numeric"),
                 definition=function(object, value) {

  if (!length(value) %in% c(2L, 3L)) {
    stop("2 or 3 coordinates are needed!")
  }
  object@metaData$imaging$pos <- value
  object
})

setReplaceMethod(f="coordinates",
                 signature=signature(object="AbstractMassObject",
                                     value="matrix"),
                 definition=function(object, value) {
  if (!ncol(value) %in% c(2L, 3L)) {
    stop("2 or 3 coordinates are needed!")
  }
  if (nrow(value) != 1L) {
    warning("all rows but the first are ignored!")
  }
  object@metaData$imaging$pos <- value[1L, ]
  object
})

## list
setMethod(f="coordinates",
          signature=signature(object="list"),
          definition=function(object, adjust=FALSE) {

  ## test arguments
  .stopIfNotIsMassObjectList(object)

  m <- do.call(rbind, lapply(object, coordinates))

  if (!is.null(m)) {
    colnames(m) <- c("x", "y", "z")[seq_len(ncol(m))]

    if (adjust) {
        m <- apply(m, MARGIN=2L, function(x)x - min(x) + 1L)
    }
  }
  m
})

## list
setReplaceMethod(f="coordinates",
                 signature=signature(object="list",
                                     value="matrix"),
                 definition=function(object, value) {
  ## test arguments
  .stopIfNotIsMassObjectList(object)

  .mapply("coordinates<-", object, split(value, row(value)))
})
