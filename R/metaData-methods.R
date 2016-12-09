## AbstractMassObject
setMethod(f="metaData",
          signature=signature(object="AbstractMassObject"),
          definition=function(object) {

  object@metaData
})

## AbstractMassObject
setReplaceMethod(f="metaData",
                  signature=signature(object="AbstractMassObject"),
                  definition=function(object, value) {

  object@metaData <- value
  object
})
