## AbstractMassObject
setMethod(f="as.matrix",
          signature=signature(x="AbstractMassObject"),
          definition=function(x, index) {

  matrix(c(x@mass[index], x@intensity[index]), ncol=2L, byrow=FALSE,
         dimnames=list(NULL, c("mass", "intensity")))
})
