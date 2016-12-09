## AbstractMassObject
setMethod(f="isEmpty",
          signature=signature(x="AbstractMassObject"),
          definition=function(x) {

  !sum(as.double(x@intensity), na.rm=TRUE)
})

setMethod(f=".isEmptyWarning",
          signature=signature(x="AbstractMassObject"),
          definition=function(x) {

  if (isEmpty(x)) {
    msg <- paste0(class(x)[1L], " object")

    if (!is.null(x@metaData$file)) {
      msg <- paste0(msg, " (file: ", x@metaData$file, ")")
    }

    parentCall <- sys.call(-1L)
    warning("In ", deparse(parentCall), " : ", msg, " is empty!",
            call.=FALSE)
    return(TRUE)
  }

  FALSE
})
