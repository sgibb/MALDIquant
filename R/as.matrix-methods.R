## AbstractMassObject
setMethod(f="as.matrix",
          signature=signature(x="AbstractMassObject"),
          definition=function(x, index) {

  matrix(c(mass(x)[index], intensity(x)[index]), ncol=2L, byrow=FALSE,
         dimnames=list(NULL, c("mass", "intensity")))
})
