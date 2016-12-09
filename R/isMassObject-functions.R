.isMassObject <- function(x) {
  inherits(x=x, what="AbstractMassObject")
}

isMassSpectrum <- function(x) {
  is(object=x, class2="MassSpectrum")
}

isMassPeaks <- function(x) {
  is(object=x, class2="MassPeaks")
}
